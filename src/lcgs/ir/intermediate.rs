use std::borrow::BorrowMut;
use std::collections::{HashMap, HashSet};

use crate::atl::common::Proposition;
use crate::atl::formula::{identifier, ATLExpressionParser};
use crate::atl::gamestructure::GameStructure;
use crate::lcgs::ast::{ConstDecl, Decl, DeclKind, ExprKind, Identifier, Root};
use crate::lcgs::ir::error::Error;
use crate::lcgs::ir::eval::Evaluator;
use crate::lcgs::ir::relabeling::Relabeler;
use crate::lcgs::ir::symbol_checker::{CheckMode, SymbolChecker, SymbolError};
use crate::lcgs::ir::symbol_table::{Owner, SymbolIdentifier, SymbolTable};
use pom::parser::{sym, Parser};

/// A struct that holds information about players for the intermediate representation
/// of the lazy game structure
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Player {
    index: usize,
    name: String,
    actions: Vec<SymbolIdentifier>,
}

impl Player {
    pub fn new(index: usize, name: &str) -> Player {
        Player {
            index,
            name: name.to_string(),
            actions: vec![],
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    /// Helper function to quickly turn a player into an [Owner]
    pub fn to_owner(&self) -> Owner {
        Owner::Player(self.name.clone())
    }

    pub fn get_name(&self) -> String {
        self.name.clone()
    }
}

/// An [IntermediateLCGS] is created from processing an AST and checking the validity of the
/// declarations.
#[derive(Clone, Debug)]
pub struct IntermediateLCGS {
    symbols: HashMap<SymbolIdentifier, Decl>,
    labels: Vec<SymbolIdentifier>,
    vars: Vec<SymbolIdentifier>,
    players: Vec<Player>,
}

impl IntermediateLCGS {
    /// Create an [IntermediateLCGS] from an AST root. All declarations in the resulting
    /// [IntermediateLCGS] are symbol checked and type checked.
    pub fn create(root: Root) -> Result<IntermediateLCGS, Error> {
        let mut symbols = SymbolTable::new();

        // Register global decls. Then check and optimize them
        let (players, labels, vars) = register_decls(&mut symbols, root)?;
        check_and_optimize_decls(&symbols)?;

        let ilcgs = IntermediateLCGS {
            symbols: symbols.solidify(),
            labels,
            vars,
            players,
        };

        Ok(ilcgs)
    }

    pub fn get_decl(&self, symbol: &SymbolIdentifier) -> Option<&Decl> {
        self.symbols.get(symbol)
    }

    /// Transforms a state index to a [State].
    fn state_from_index(&self, state_index: usize) -> State {
        let mut state = State(HashMap::new());
        let mut carry = state_index as i32;

        // The following method resembles the typical way of transforming a number of seconds
        // into seconds, minutes, hours, and days. In this case the time units are state variables
        // instead, and similarly to time units, each state variable has a different size.
        for symb_id in &self.vars {
            let symb = self.symbols.get(symb_id).unwrap();
            if let DeclKind::StateVar(var) = &symb.kind {
                let value = {
                    let size = var.ir_range.end() - var.ir_range.start() + 1;
                    let quotient = carry / size;
                    let remainder = carry.rem_euclid(size);
                    carry = quotient;
                    var.ir_range.start() + remainder
                };
                state.0.insert(symb_id.clone(), value);
            }
        }
        debug_assert!(
            carry == 0,
            "State overflow (carry was {}). Invalid state index.",
            carry
        );
        state
    }

    /// Transforms a state into its index
    fn index_of_state(&self, state: &State) -> usize {
        let mut combined_size = 1;
        let mut res = 0usize;

        // The following method resembles the typical way of transforming a number of seconds,
        // minutes, hours, and days into just seconds. In this case the time units are
        // state variables instead, and similarly to time units, each state variable has a
        // different size.
        for symb_id in &self.vars {
            let symb = self.symbols.get(symb_id).unwrap();
            if let DeclKind::StateVar(var) = &symb.kind {
                let size = (var.ir_range.end() - var.ir_range.start() + 1) as usize;
                let val = state.0.get(symb_id).unwrap();
                res += (val - var.ir_range.start()) as usize * combined_size;
                combined_size *= size;
            }
        }
        res
    }

    /// Returns a list of the moves available to the given player in the given state.
    fn available_actions(&self, state: &State, player: usize) -> Vec<SymbolIdentifier> {
        self.players[player]
            .actions
            .iter()
            .filter(|symb_id| {
                let symb = self.symbols.get(symb_id).unwrap();
                if let DeclKind::Transition(trans) = &symb.kind {
                    // The action is available if the condition is not evaluated to 0 in this state
                    return 0 != Evaluator::new(state).eval(&trans.condition);
                }
                panic!("Transition was not a transition.")
            })
            .cloned()
            .collect()
    }

    /// Returns the initial state of the LCGS game
    pub fn initial_state(&self) -> State {
        let mut res = State(HashMap::new());
        for symb_id in &self.vars {
            let symb = self.symbols.get(symb_id).unwrap();
            if let DeclKind::StateVar(var) = &symb.kind {
                res.0.insert(symb_id.clone(), var.ir_initial_value);
            }
        }
        res
    }

    /// Returns a vector of players
    pub fn get_player(&self) -> Vec<Player> {
        self.players.clone()
    }

    /// Returns vector of labels
    pub fn get_labels(&self) -> Vec<SymbolIdentifier> {
        self.labels.clone()
    }

    /// Returns the initial state index of the LCGS game
    pub fn initial_state_index(&self) -> usize {
        self.index_of_state(&self.initial_state())
    }
}

/// Names of declarations. First component is players and their fields. Second component
/// is global labels. And third component is global variables.
type DeclNames = (Vec<Player>, Vec<SymbolIdentifier>, Vec<SymbolIdentifier>);

/// Registers all declarations from the root in the symbol table. Constants are optimized to
/// numbers immediately. On success, a vector of [Player]s is returned with information
/// about players and the names of their actions.
fn register_decls(symbols: &mut SymbolTable, root: Root) -> Result<DeclNames, Error> {
    let mut player_decls = vec![];
    let mut player_names = HashSet::new();
    let mut labels = vec![];
    let mut vars = vec![];

    let mut next_label_index = 0;

    // Register global declarations.
    // Constants are evaluated immediately.
    // Players are put in a separate vector and handled afterwards.
    // Symbol table is given ownership of the declarations.
    for mut decl in root.decls {
        match decl.kind.borrow_mut() {
            DeclKind::Const(cons) => {
                // We can evaluate constants immediately as constants can only
                // refer to other constants that are above them in the program.
                // If they don't reduce to a single number, then the SymbolChecker
                // produces an error.
                let result = SymbolChecker::new(&symbols, Owner::Global, CheckMode::Const)
                    .check(&cons.definition)?;
                debug_assert!(matches!(result.kind, ExprKind::Number(_)));
                let name = cons.name.name().to_string();
                // Construct a resolved constant decl
                let decl = Decl {
                    kind: DeclKind::Const(Box::new(ConstDecl {
                        name: Identifier::Resolved {
                            owner: Owner::Global,
                            name: name.clone(),
                        },
                        definition: result,
                    })),
                };
                if symbols.insert(&Owner::Global, &name, decl).is_some() {
                    panic!("Constant '{}' is already declared.", &name); // TODO Use custom error
                }
            }
            DeclKind::Label(label) => {
                label.index = next_label_index;
                next_label_index += 1;
                // Insert in symbol table and add to labels list
                let name = decl.kind.ident().name().to_string();
                if symbols.insert(&Owner::Global, &name, decl).is_some() {
                    panic!("Symbol '{}' is already declared.", &name); // TODO Use custom error
                }
                labels.push(SymbolIdentifier {
                    owner: Owner::Global,
                    name,
                });
            }
            DeclKind::StateVar(_) => {
                // Insert in symbol table and add to vars list
                let name = decl.kind.ident().name().to_string();
                if symbols.insert(&Owner::Global, &name, decl).is_some() {
                    panic!("Symbol '{}' is already declared.", &name); // TODO Use custom error
                }
                vars.push(SymbolIdentifier {
                    owner: Owner::Global,
                    name,
                });
            }
            DeclKind::Template(_) => {
                let name = decl.kind.ident().name().to_string();
                if symbols.insert(&Owner::Global, &name, decl).is_some() {
                    panic!("Symbol '{}' is already declared.", &name); // TODO Use custom error
                }
            }
            DeclKind::Player(player) => {
                // We handle player declarations later
                if !player_names.insert(player.name.name().to_string()) {
                    panic!("Player '{}' is already declared", &player.name.name());
                    // TODO Use custom error
                }
                player_decls.push(decl);
            }
            _ => panic!("Not a global declaration. Parser must have failed."), // Not a global decl
        }
    }

    // Register player declarations. Here we clone the declarations since multiple
    // players can use the same template
    let mut players = vec![];
    for (index, mut decl) in player_decls.drain(..).enumerate() {
        if let DeclKind::Player(player_decl) = decl.kind.borrow_mut() {
            player_decl.index = index;

            let mut player = Player::new(index, &player_decl.name.name());
            let relabeler = Relabeler::new(&player_decl.relabeling);

            let template_decl = symbols
                .get(&Owner::Global, &player_decl.template.name())
                .expect("Unknown template") // TODO Use custom error
                .declaration
                .borrow()
                .clone();

            if let DeclKind::Template(template) = template_decl.kind {
                // Go through each declaration in the template and register a relabeled
                // clone of it that is owned by the given player
                let scope_owner = player.to_owner();
                for mut decl in template.decls {
                    match decl.kind.borrow_mut() {
                        DeclKind::Label(label) => {
                            label.index = next_label_index;
                            next_label_index += 1;
                            let relabeled_decl = relabeler.relabel_decl(&decl)?;
                            // Insert into symbol table and add to labels list
                            let name = relabeled_decl.kind.ident().name().to_string();
                            if symbols
                                .insert(&scope_owner, &name, relabeled_decl)
                                .is_some()
                            {
                                panic!("Label '{}.{}' is already declared.", &scope_owner, &name);
                            };
                            labels.push(SymbolIdentifier {
                                owner: scope_owner.clone(),
                                name,
                            });
                        }
                        DeclKind::StateVar(_) => {
                            let relabeled_decl = relabeler.relabel_decl(&decl)?;
                            // Insert into symbol table and add to vars list
                            let name = relabeled_decl.kind.ident().name().to_string();
                            if symbols
                                .insert(&scope_owner, &name, relabeled_decl)
                                .is_some()
                            {
                                panic!(
                                    "Variable '{}.{}' is already declared.",
                                    &scope_owner, &name
                                );
                            };
                            vars.push(SymbolIdentifier {
                                owner: scope_owner.clone(),
                                name,
                            });
                        }
                        DeclKind::Transition(_) => {
                            // Transitions are inserted in the symbol table, but their name
                            // is also stored in the player.actions so they can easily be found
                            // later when run.
                            let relabeled_decl = relabeler.relabel_decl(&decl)?;
                            let name = relabeled_decl.kind.ident().name().to_string();
                            if symbols
                                .insert(&scope_owner, &name, relabeled_decl)
                                .is_some()
                            {
                                panic!("Action '{}.{}' is already declared.", &scope_owner, &name);
                            };
                            player.actions.push(scope_owner.symbol_id(&name));
                        }
                        _ => panic!(
                            "Not a declaration allowed in templates. Parser must have failed."
                        ),
                    }
                }
            } else {
                panic!("'{}' is not a template.", template_decl.kind.ident().name());
                // TODO Use custom error
            }

            // The player is done. We can now register the player declaration.
            players.push(player);
            let name = player_decl.name.name().to_string();
            symbols.insert(&Owner::Global, &name, decl);
        } else {
            panic!("A non-PlayerDecl got into this vector");
        }
    }
    Ok((players, labels, vars))
}

/// Reduces the declarations in a [SymbolTable] to a more compact version, if possible.
/// Validity of identifiers are also checked and resolved.
fn check_and_optimize_decls(symbols: &SymbolTable) -> Result<(), SymbolError> {
    for (symb_id, rc_symb) in symbols {
        // Create resolved name
        let SymbolIdentifier { owner, name } = symb_id;
        let resolved_name = Identifier::Resolved {
            owner: owner.clone(),
            name: name.clone(),
        };

        // Optimize the declaration's expression(s)
        let mut declaration = rc_symb.declaration.borrow_mut();
        match declaration.kind.borrow_mut() {
            DeclKind::Label(label) => {
                label.name = resolved_name;
                label.condition =
                    SymbolChecker::new(symbols, owner.clone(), CheckMode::LabelOrTransition)
                        .check(&label.condition)?;
            }
            DeclKind::StateVar(var) => {
                var.name = resolved_name;
                // Both initial value, min, and max are expected to be constant.
                // Hence, we also evaluate them now so we don't have to do that each time.
                let checker = SymbolChecker::new(symbols, owner.clone(), CheckMode::Const);
                var.ir_initial_value = checker.check_eval(&var.initial_value)?;
                let min = checker.check_eval(&var.range.min)?;
                let max = checker.check_eval(&var.range.max)?;
                var.ir_range = min..=max;
                assert!(var.ir_range.contains(&var.ir_initial_value), "");
                var.next_value =
                    SymbolChecker::new(symbols, owner.clone(), CheckMode::StateVarUpdate)
                        .check(&var.next_value)?;
            }
            DeclKind::Transition(tran) => {
                tran.name = resolved_name;
                tran.condition =
                    SymbolChecker::new(symbols, owner.clone(), CheckMode::LabelOrTransition)
                        .check(&tran.condition)?;
            }
            DeclKind::Player(player) => {
                player.name = resolved_name;
            }
            DeclKind::Template(template) => {
                template.name = resolved_name;
            }
            DeclKind::Const(_) => {} // Needs no further reduction
        }
    }
    Ok(())
}

/// A game structure state of an LCGS. Holds a mapping of symbol names to their current value
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct State(pub HashMap<SymbolIdentifier, i32>);

impl GameStructure for IntermediateLCGS {
    fn max_player(&self) -> usize {
        self.players.len()
    }

    /// Returns the set of labels/propositions available in the given state.
    fn labels(&self, state: usize) -> HashSet<usize> {
        let state = self.state_from_index(state);
        let mut res = HashSet::new();

        // The labels id is their index in the self.labels vector
        for (i, symb_id) in self.labels.iter().enumerate() {
            let symb = self.symbols.get(symb_id).unwrap();
            if let DeclKind::Label(label) = &symb.kind {
                // We evaluate the condition with the values of the current state to know
                // whether the label is present or not
                let value = Evaluator::new(&state).eval(&label.condition);
                if value != 0 {
                    res.insert(i);
                }
            }
        }
        res
    }

    /// Returns the next state given a current state and an action for each player.
    fn transitions(&self, state: usize, choices: Vec<usize>) -> usize {
        let mut state = self.state_from_index(state);
        // To evaluate the next state we assign the actions to either 1 or 0 depending
        // on whether or not the action was taken
        for (p_index, player) in self.players.iter().enumerate() {
            // The `choices` vector only considers available actions, so we do those first
            let moves = self.available_actions(&state, p_index);
            debug_assert!(
                choices[p_index] < moves.len(),
                format!(
                    "Unknown action {} chosen for player {} in state {:?}",
                    choices[p_index], p_index, state
                )
            );
            for (a_index, a_symb_id) in moves.iter().enumerate() {
                let val = if choices[p_index] == a_index { 1 } else { 0 };
                state.0.insert(a_symb_id.clone(), val);
            }

            // Some actions might not be available. These should also be set to 0.
            for action in &player.actions {
                if !state.0.contains_key(action) {
                    state.0.insert(action.clone(), 0);
                }
            }
        }

        // Now we can evaluate the next state based on previous state and the actions taken
        let evaluator = Evaluator::new(&state);
        let mut next_state = State(HashMap::new());
        for symb_id in &self.vars {
            let symb = self.symbols.get(symb_id).unwrap();
            if let DeclKind::StateVar(var) = &symb.kind {
                let val = evaluator.eval(&var.next_value);
                next_state.0.insert(symb_id.clone(), val);
            }
        }

        self.index_of_state(&next_state)
    }

    /// Returns the number of moves available to each player in the given state.
    fn move_count(&self, state: usize) -> Vec<usize> {
        let state = self.state_from_index(state);
        self.players
            .iter()
            .enumerate()
            .map(|(i, _player)| self.available_actions(&state, i).len())
            .collect()
    }
}

impl ATLExpressionParser for IntermediateLCGS {
    fn player_parser(&self) -> Parser<u8, Proposition> {
        // In ATL, players are referred to using their name, i.e. an identifier
        identifier().convert(move |name| {
            // We check if a declaration with the given name exists,
            // and that it is a player declaration
            let symbol = Owner::Global.symbol_id(&name);
            if let Some(decl) = self.symbols.get(&symbol) {
                if let DeclKind::Player(player) = &decl.kind {
                    Ok(player.index)
                } else {
                    Err(format!("The declaration '{}' is not a player.", name))
                }
            } else {
                Err(format!(
                    "The LCGS program does not contain any player named '{}'.",
                    name
                ))
            }
        })
    }

    fn proposition_parser(&self) -> Parser<u8, Proposition> {
        // In ATL, propositions are either "something" where "something" must be a label declared
        // in the global scope, or "someone.something" where "something" is a label owned by
        // a player of name "someone".
        let parser = identifier() + (sym(b'.') * identifier()).opt();
        parser.convert(move |(name_or_owner, name)| {
            // We infer whether the label should be found in local (player) or global scope.
            // full_name is used for error descriptions.
            let (symbol, full_name) = if let Some(name) = name {
                let owner = name_or_owner;
                (
                    Owner::Player(owner.clone()).symbol_id(&name),
                    format!("{}.{}", owner, name),
                )
            } else {
                let name = name_or_owner;
                (Owner::Global.symbol_id(&name), name)
            };

            // We check if such a symbol exists, and is it a player declaration
            if let Some(decl) = self.symbols.get(&symbol) {
                if let DeclKind::Label(label) = &decl.kind {
                    Ok(label.index)
                } else {
                    Err(format!("The declaration '{}' is not a label.", full_name))
                }
            } else {
                Err(format!(
                    "The LCGS program does not contain any label with the name '{}'",
                    full_name
                ))
            }
        })
    }
}

#[cfg(test)]
mod test {
    use crate::atl::gamestructure::GameStructure;
    use crate::lcgs::ast::DeclKind;
    use crate::lcgs::ir::intermediate::IntermediateLCGS;
    use crate::lcgs::ir::symbol_table::Owner;
    use crate::lcgs::parse::parse_lcgs;

    #[test]
    fn test_symbol_01() {
        // Check if the correct symbols are inserted into the symbol table
        let input = "
        const max_health = 100;
        player alice = gamer;
        player bob = gamer;

        template gamer
            health : [0 .. max_health] init max_health;
            health' = health - 1;

            label alive = health > 0;

            [wait] 1;
            [shoot] health > 0;
        endtemplate
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        assert_eq!(lcgs.symbols.len(), 12);
        assert!(lcgs.symbols.get(&":global.max_health".into()).is_some());
        assert!(lcgs.symbols.get(&":global.alice".into()).is_some());
        assert!(lcgs.symbols.get(&":global.bob".into()).is_some());
        assert!(lcgs.symbols.get(&":global.gamer".into()).is_some());
        assert!(lcgs.symbols.get(&"alice.health".into()).is_some());
        assert!(lcgs.symbols.get(&"alice.alive".into()).is_some());
        assert!(lcgs.symbols.get(&"alice.wait".into()).is_some());
        assert!(lcgs.symbols.get(&"alice.shoot".into()).is_some());
        assert!(lcgs.symbols.get(&"bob.health".into()).is_some());
        assert!(lcgs.symbols.get(&"bob.alive".into()).is_some());
        assert!(lcgs.symbols.get(&"bob.wait".into()).is_some());
        assert!(lcgs.symbols.get(&"bob.shoot".into()).is_some());
    }

    #[test]
    fn test_symbol_02() {
        // State vars can refer to themselves in the update clause
        let input1 = "
        foo : [1 .. 10] init 1;
        foo' = foo;
        ";
        let lcgs1 = IntermediateLCGS::create(parse_lcgs(input1).unwrap()).unwrap();
        assert_eq!(lcgs1.symbols.len(), 1);
        assert!(lcgs1.symbols.get(&":global.foo".into()).is_some());

        // But other declarations cannot refer to themselves
        let input2 = "
        label foo = foo > 0;
        ";
        let lcgs2 = IntermediateLCGS::create(parse_lcgs(input2).unwrap());
        assert!(lcgs2.is_err());
    }

    #[test]
    fn test_relabeling_01() {
        // Check standard use of relabeling
        let input = "
        const max_health = 100;
        player anna = gamer [enemy=bob];
        player bob = gamer [enemy=anna];

        template gamer
            health : [0 .. max_health] init max_health;
            health' = enemy.shoot ? health - 1 : health;

            label alive = health > 0;

            [wait] 1;
            [shoot] health > 0 && enemy.health > 0;
        endtemplate
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        assert_eq!(lcgs.symbols.len(), 12);
        assert!(lcgs.symbols.get(&":global.max_health".into()).is_some());
        assert!(lcgs.symbols.get(&":global.anna".into()).is_some());
        assert!(lcgs.symbols.get(&":global.bob".into()).is_some());
        assert!(lcgs.symbols.get(&":global.gamer".into()).is_some());
        assert!(lcgs.symbols.get(&"anna.health".into()).is_some());
        assert!(lcgs.symbols.get(&"anna.alive".into()).is_some());
        assert!(lcgs.symbols.get(&"anna.wait".into()).is_some());
        assert!(lcgs.symbols.get(&"anna.shoot".into()).is_some());
        assert!(lcgs.symbols.get(&"bob.health".into()).is_some());
        assert!(lcgs.symbols.get(&"bob.alive".into()).is_some());
        assert!(lcgs.symbols.get(&"bob.wait".into()).is_some());
        assert!(lcgs.symbols.get(&"bob.shoot".into()).is_some());
    }

    #[test]
    fn test_relabeling_02() {
        // Check standard use of relabeling that affects declaration names
        let input = "
        player anna = human [var=apples, act=dance, prop=happy];
        player bob = human [var=bananas, act=run, prop=sad];

        template human
            var : [0 .. 10] init 5;
            var' = var;
            [act] 1;
            label prop = 1;
        endtemplate
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        assert_eq!(lcgs.symbols.len(), 9);
        assert!(lcgs.symbols.get(&":global.anna".into()).is_some());
        assert!(lcgs.symbols.get(&":global.bob".into()).is_some());
        assert!(lcgs.symbols.get(&":global.human".into()).is_some());
        assert!(lcgs.symbols.get(&"anna.apples".into()).is_some());
        assert!(lcgs.symbols.get(&"anna.dance".into()).is_some());
        assert!(lcgs.symbols.get(&"anna.happy".into()).is_some());
        assert!(lcgs.symbols.get(&"bob.bananas".into()).is_some());
        assert!(lcgs.symbols.get(&"bob.run".into()).is_some());
        assert!(lcgs.symbols.get(&"bob.sad".into()).is_some());
    }

    #[test]
    fn test_relabeling_03() {
        // Check use of relabeling to expressions
        let input = "
        player anna = human [act=work, income=1000 + work * 200, expenses=1000];
        player bob = human [income=1000, expenses=money * 2 / 10];

        template human
            money : [0 .. 10000] init 2000;
            money' = money + income - expenses;
            [wait] 1;
            [act] 1;
        endtemplate
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        assert_eq!(lcgs.symbols.len(), 9);
        assert!(lcgs.symbols.get(&":global.anna".into()).is_some());
        assert!(lcgs.symbols.get(&":global.bob".into()).is_some());
        assert!(lcgs.symbols.get(&":global.human".into()).is_some());
        assert!(lcgs.symbols.get(&"anna.money".into()).is_some());
        assert!(lcgs.symbols.get(&"anna.money".into()).is_some());
        assert!(lcgs.symbols.get(&"anna.wait".into()).is_some());
        assert!(lcgs.symbols.get(&"anna.work".into()).is_some());
        assert!(lcgs.symbols.get(&"bob.money".into()).is_some());
        assert!(lcgs.symbols.get(&"bob.wait".into()).is_some());
        assert!(lcgs.symbols.get(&"bob.act".into()).is_some());
    }

    #[test]
    fn test_state_translation_01() {
        // Is translation back and forth between state and index correct
        let input = "
        foo : [0 .. 9] init 0;
        foo' = foo;
        bar : [0 .. 5] init 0;
        bar' = bar;
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let index = 23;
        let state = lcgs.state_from_index(index);
        let index2 = lcgs.index_of_state(&state);
        assert_eq!(index, index2);
    }

    #[test]
    fn test_state_translation_02() {
        // Is translation back and forth between state and index correct
        // Wack ranges
        let input = "
        foo : [5 .. 23] init 5;
        foo' = foo;
        bar : [3 .. 5] init 3;
        bar' = bar;
        yum : [100 .. 102] init 100;
        yum' = yum;
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let indexes = [12, 55, 126, 78, 99];
        for i in &indexes {
            let state = lcgs.state_from_index(*i);
            let i2 = lcgs.index_of_state(&state);
            assert_eq!(*i, i2);
        }
    }

    #[test]
    fn test_state_translation_03() {
        // Is translation back and forth between state and index correct
        // Wack ranges
        let input = "
        foo : [-2 .. 13] init 5;
        foo' = foo;
        bar : [-5 .. -3] init -3;
        bar' = bar;
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let index = 14;
        let state = lcgs.state_from_index(index);
        let index2 = lcgs.index_of_state(&state);
        assert_eq!(index, index2);
    }

    #[test]
    fn test_negation_const_01() {
        let input = "
        const t = -5;
        ";
        let pp = parse_lcgs(input);
        let lcgs = IntermediateLCGS::create(pp.unwrap()).unwrap();
        assert!(lcgs.symbols.get(&":global.t".into()).is_some());
    }

    /// Helper function to get the index of a label with the given symbol name
    fn get_label_index(lcgs: &IntermediateLCGS, symbol_name: &str) -> usize {
        let symbol = lcgs.symbols.get(&symbol_name.into()).unwrap();
        if let DeclKind::Label(label) = &symbol.kind {
            label.index
        } else {
            panic!("Symbol '{}' is not a label", symbol_name)
        }
    }

    #[test]
    fn test_labels_01() {
        // Are the expected labels present
        let input = "
        foo : [0 .. 9] init 0;
        foo' = foo;
        bar : [0 .. 5] init 0;
        bar' = bar;
        label cool = foo;
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let labels = lcgs.labels(23);
        assert!(labels.contains(&0usize));
        assert_eq!(get_label_index(&lcgs, ":global.cool"), 0usize);
    }

    #[test]
    fn test_labels_02() {
        // Are the expected labels present
        let input = "
        foo : [0 .. 9] init 0;
        foo' = foo;
        bar : [0 .. 5] init 0;
        bar' = bar;
        label cool = foo;
        label great = bar == 0;
        label awesome = foo > bar;
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let labels = lcgs.labels(46);
        assert!(labels.contains(&0usize));
        assert_eq!(get_label_index(&lcgs, ":global.cool"), 0usize);
        assert!(!labels.contains(&1usize));
        assert_eq!(get_label_index(&lcgs, ":global.great"), 1usize);
        assert!(labels.contains(&2usize));
        assert_eq!(get_label_index(&lcgs, ":global.awesome"), 2usize);
    }

    #[test]
    fn test_labels_03() {
        // Are the expected labels present
        // With players and templates
        let input = "
        foo : [0 .. 9] init 0;
        foo' = foo;
        label no = foo == 0;
        player p1 = something;
        player p2 = something;
        template something
            label yes = foo == 5;
            [wait] 1;
        endtemplate
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let labels = lcgs.labels(5);
        assert!(!labels.contains(&0usize));
        assert_eq!(get_label_index(&lcgs, ":global.no"), 0usize);
        assert!(labels.contains(&1usize));
        assert_eq!(get_label_index(&lcgs, "p1.yes"), 1usize);
        assert!(labels.contains(&2usize));
        assert_eq!(get_label_index(&lcgs, "p2.yes"), 2usize);
    }

    #[test]
    fn test_move_count_01() {
        // Are the expected moves available
        let input = "
        foo : [0 .. 9] init 0;
        foo' = foo;
        player p1 = something1;
        player p2 = something2;
        template something1
            [wait] 1;
            [move] foo == 0;
        endtemplate
        template something2
            [wait] 1;
            [move] foo > 0;
        endtemplate
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let move_count = lcgs.move_count(4);
        assert_eq!(move_count[0], 1);
        assert_eq!(move_count[1], 2);
    }

    #[test]
    fn test_transition_01() {
        // Can we make transitions as expected when they depend on previous state
        let input = "
        foo : [0 .. 1] init 0;
        foo' = !foo;
        player p = something;
        template something
            [swap] 1;
        endtemplate
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let next_state = lcgs.transitions(0, vec![0]);
        assert_eq!(1, next_state);
        let next_next_state = lcgs.transitions(next_state, vec![0]);
        assert_eq!(0, next_next_state);
    }

    #[test]
    fn test_transition_02() {
        // Can we make transitions as expected when they depend on player actions
        let input = "
        foo : [0 .. 1] init 0;
        foo' = p.set_foo;
        player p = something;
        template something
            [reset_foo] 1;
            [set_foo] 1;
        endtemplate
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        assert_eq!(0, lcgs.transitions(0, vec![0]));
        assert_eq!(1, lcgs.transitions(0, vec![1]));
        assert_eq!(0, lcgs.transitions(1, vec![0]));
        assert_eq!(1, lcgs.transitions(1, vec![1]));
    }

    #[test]
    fn test_transition_03() {
        // Can we update state even though it depends on unavailable actions?
        let input = "
        player ryan = guy;
        
        some_var : [0 .. 10] init 0;
        some_var' = some_var + ryan.unavailable_action;

        template guy
            [unavailable_action] 0;
            [available_action] 1; 
        endtemplate
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let init_state = lcgs.initial_state_index();
        assert_eq!(0, init_state);
        assert_eq!(0, lcgs.transitions(init_state, vec![0]));
    }

    #[test]
    fn test_initial_state_01() {
        // Is initial state what we expect?
        let input = "
        foo : [0 .. 1] init 0;
        foo' = foo;
        bar : [0 .. 1] init 1;
        bar' = bar;
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        assert_eq!(2, lcgs.initial_state_index());
    }

    #[test]
    fn test_initial_state_02() {
        // Is initial state what we expect?
        // Wack ranges
        let input = "
        foo : [5 .. 9] init 6;
        foo' = foo;
        bar : [1 .. 6] init 1;
        bar' = bar;
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        assert_eq!(1, lcgs.initial_state_index());
    }

    /// Helper function to get the index of a player with the given name
    fn get_player_index(lcgs: &IntermediateLCGS, player_name: &str) -> usize {
        let symbol = lcgs
            .symbols
            .get(&Owner::Global.symbol_id(player_name))
            .unwrap();
        if let DeclKind::Player(player) = &symbol.kind {
            player.index
        } else {
            panic!("Symbol ':global.{}' is not a player", player_name)
        }
    }

    #[test]
    fn test_players_01() {
        // Are the players assigned the expected indexes
        let input = "
        player p1 = something;
        player p2 = something;
        player p3 = something;
        template something
            [wait] 1;
        endtemplate
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        assert_eq!(get_player_index(&lcgs, "p1"), 0usize);
        assert_eq!(get_player_index(&lcgs, "p2"), 1usize);
        assert_eq!(get_player_index(&lcgs, "p3"), 2usize);
    }
}
