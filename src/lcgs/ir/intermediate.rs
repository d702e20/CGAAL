use std::borrow::BorrowMut;
use std::collections::{HashMap, HashSet};

use crate::atl::gamestructure::GameStructure;
use crate::lcgs::ast::{
    BinaryOpKind, ConstDecl, Decl, DeclKind, Expr, ExprKind, Identifier, Root, UnaryOpKind,
};
use crate::lcgs::ir::eval::Evaluator;
use crate::lcgs::ir::symbol_checker::{CheckMode, SymbolChecker};
use crate::lcgs::ir::symbol_table::{Owner, Symbol, SymbolIdentifier, SymbolTable};

/// A struct that holds information about players for the intermediate representation
/// of the lazy game structure
pub struct Player {
    name: String,
    actions: Vec<SymbolIdentifier>,
}

impl Player {
    pub fn new(name: &str) -> Player {
        Player {
            name: name.to_string(),
            actions: vec![],
        }
    }

    /// Helper function to quickly turn a player into an [Owner]
    pub fn to_owner(&self) -> Owner {
        Owner::Player(self.name.clone())
    }
}

/// An [IntermediateLCGS] is created from processing an AST and checking the validity of the
/// declarations.
pub struct IntermediateLCGS {
    symbols: SymbolTable,
    labels: Vec<SymbolIdentifier>,
    vars: Vec<SymbolIdentifier>,
    players: Vec<Player>,
}

impl IntermediateLCGS {
    /// Create an [IntermediateLCGS] from an AST root. All declarations in the resulting
    /// [IntermediateLCGS] are symbol checked and type checked.
    pub fn create(mut root: Root) -> Result<IntermediateLCGS, ()> {
        let mut symbols = SymbolTable::new();

        // Register global decls. Then check and optimize them
        let (players, labels, vars) = register_decls(&mut symbols, root)?;
        check_and_optimize_decls(&mut symbols)?;

        let ilcgs = IntermediateLCGS {
            symbols,
            labels,
            vars,
            players,
        };

        return Ok(ilcgs);
    }

    /// Transforms a state index to a [State].
    fn state_from_index(&self, state_index: usize) -> State {
        let mut state = State(HashMap::new());
        let mut carry = state_index as i32;

        // The following method resembles the typical way of transforming a number of seconds
        // into seconds, minutes, hours, and days. In this case the time units are state variables
        // instead, and similarly to time units, each state variable has a different size.
        for symb_id in &self.vars {
            let SymbolIdentifier { owner, name } = symb_id;
            if let DeclKind::StateVar(var) = &self
                .symbols
                .get(owner, name)
                .unwrap()
                .declaration
                .borrow()
                .kind
            {
                let value = {
                    let size = var.ir_range.len() as i32;
                    let quotient = carry / size;
                    let remainder = carry.rem_euclid(size);
                    carry = quotient;
                    var.ir_range.start + remainder
                };
                state.0.insert(symb_id.clone(), value);
            }
        }
        debug_assert!(carry == 0, "State overflow. Invalid state index.");
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
            let SymbolIdentifier { owner, name } = symb_id;
            let var = self.symbols.get(owner, name).unwrap();
            if let DeclKind::StateVar(var) = &var.declaration.borrow().kind {
                let size = var.ir_range.len() as i32;
                let val = state.0.get(symb_id).unwrap();
                res += ((val - var.ir_range.start) * combined_size) as usize;
                combined_size *= size;
            }
        }
        res
    }

    /// Returns a list of the moves available to the given player in the given state.
    fn available_moves(&self, state: &State, player: usize) -> Vec<SymbolIdentifier> {
        self.players[player]
            .actions
            .iter()
            .filter(|symb_id| {
                let SymbolIdentifier { owner, name } = symb_id;
                let symb = self.symbols.get(owner, name).unwrap();
                if let DeclKind::Transition(trans) = &symb.declaration.borrow().kind {
                    // The action is available if the condition is not evaluated to 0 in this state
                    return 0 != Evaluator::new(state).eval(&trans.condition).unwrap();
                }
                panic!("Transition was not a transition.")
            })
            .map(|s| s.clone())
            .collect()
    }

    /// Returns the initial state of the LCGS game
    fn initial_state(&self) -> State {
        let mut res = State(HashMap::new());
        for symb_id in &self.vars {
            let SymbolIdentifier { owner, name } = symb_id;
            let symb = self.symbols.get(owner, name).unwrap();
            if let DeclKind::StateVar(var) = &symb.declaration.borrow().kind {
                res.0.insert(symb_id.clone(), var.ir_initial_value);
            }
        }
        res
    }

    /// Returns the initial state index of the LCGS game
    fn initial_state_index(&self) -> usize {
        self.index_of_state(&self.initial_state())
    }
}

/// Registers all declarations from the root in the symbol table. Constants are optimized to
/// numbers immediately. On success, a vector of [Player]s is returned with information
/// about players and the names of their actions.
fn register_decls(
    symbols: &mut SymbolTable,
    root: Root,
) -> Result<(Vec<Player>, Vec<SymbolIdentifier>, Vec<SymbolIdentifier>), ()> {
    let mut player_decls = vec![];
    let mut player_names = HashSet::new();
    let mut labels = vec![];
    let mut vars = vec![];

    // Register global declarations.
    // Constants are evaluated immediately.
    // Players are put in a separate vector and handled afterwards.
    // Symbol table is given ownership of the declarations.
    for decl in root.decls {
        match &decl.kind {
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
            DeclKind::Label(_) => {
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
    for decl in player_decls {
        if let DeclKind::Player(player_decl) = &decl.kind {
            let mut player = Player::new(&player_decl.name.name());

            let template_decl = symbols
                .get(&Owner::Global, &player_decl.template.name())
                .expect("Unknown template") // TODO Use custom error
                .declaration
                .borrow()
                .clone();

            if let DeclKind::Template(template) = template_decl.kind {
                // Go through each declaration in the template and register a clone of it
                // that is owned by the given player
                let scope_owner = player.to_owner();
                for decl in template.decls {
                    match &decl.kind {
                        DeclKind::Label(_) => {
                            // Insert into symbol table and add to labels list
                            let name = decl.kind.ident().name().to_string();
                            if symbols.insert(&scope_owner, &name, decl.clone()).is_some() {
                                panic!("Symbol '{}.{}' is already declared.", &scope_owner, &name);
                            };
                            labels.push(SymbolIdentifier {
                                owner: scope_owner.clone(),
                                name,
                            });
                        }
                        DeclKind::StateVar(_) => {
                            // Insert into symbol table and add to vars list
                            let name = decl.kind.ident().name().to_string();
                            if symbols.insert(&scope_owner, &name, decl.clone()).is_some() {
                                panic!("Symbol '{}.{}' is already declared.", &scope_owner, &name);
                            };
                            vars.push(SymbolIdentifier {
                                owner: scope_owner.clone(),
                                name,
                            });
                        }
                        DeclKind::Transition(tran) => {
                            // Transitions are inserted in the symbol table, but their name
                            // is also stored in the player.actions so they can easily be found
                            // later when run.
                            let name = tran.name.name().to_string();
                            if symbols.insert(&scope_owner, &name, decl.clone()).is_some() {
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
                panic!("'{}' is not a template.", decl.kind.ident().name()); // TODO Use custom error
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
fn check_and_optimize_decls(symbols: &SymbolTable) -> Result<(), ()> {
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
                var.ir_range = min..(max + 1);
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
pub struct State(pub HashMap<SymbolIdentifier, i32>);

impl GameStructure for IntermediateLCGS {
    fn max_player(&self) -> u32 {
        self.players.len() as u32
    }

    /// Returns the set of labels/propositions available in the given state.
    fn labels(&self, state: usize) -> HashSet<usize> {
        let state = self.state_from_index(state);
        let mut res = HashSet::new();

        // The labels id is their index in the self.labels vector
        for (i, symb_id) in self.labels.iter().enumerate() {
            let SymbolIdentifier { owner, name } = symb_id;
            let symb = self.symbols.get(owner, name).unwrap();
            if let DeclKind::Label(label) = &symb.declaration.borrow().kind {
                // We evaluate the condition with the values of the current state to know
                // whether the label is present or not
                let value = Evaluator::new(&state).eval(&label.condition).unwrap();
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
            let moves = self.available_moves(&state, p_index);
            for (a_index, a_symb_id) in moves.iter().enumerate() {
                let val = if choices[p_index] == a_index { 1 } else { 0 };
                state.0.insert(a_symb_id.clone(), val);
            }
        }

        // Now we can evaluate the next state based on previous state and the actions taken
        let evaluator = Evaluator::new(&state);
        let mut next_state = State(HashMap::new());
        for symb_id in &self.vars {
            let SymbolIdentifier { owner, name } = symb_id;
            let symb = self.symbols.get(owner, name).unwrap();
            if let DeclKind::StateVar(var) = &symb.declaration.borrow().kind {
                let val = evaluator.eval(&var.next_value).unwrap();
                next_state.0.insert(symb_id.clone(), val);
            }
        }

        self.index_of_state(&next_state)
    }

    /// Returns the number of moves available to each player in the given state.
    fn move_count(&self, state: usize) -> Vec<u32> {
        let state = self.state_from_index(state);
        self.players
            .iter()
            .enumerate()
            .map(|(i, player)| self.available_moves(&state, i).len() as u32)
            .collect()
    }
}

#[cfg(test)]
mod test {
    use crate::atl::gamestructure::GameStructure;
    use crate::lcgs::ir::intermediate::IntermediateLCGS;
    use crate::lcgs::ir::symbol_table::Owner;
    use crate::lcgs::parse::parse_lcgs;

    #[test]
    fn test_symbol_01() {
        // Check if the correct symbols are inserted into the symbol table
        let input = br"
        const max_health = 100;
        player anna = gamer;
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
        assert!(lcgs.symbols.get(&Owner::Global, "max_health").is_some());
        assert!(lcgs.symbols.get(&Owner::Global, "anna").is_some());
        assert!(lcgs.symbols.get(&Owner::Global, "bob").is_some());
        assert!(lcgs.symbols.get(&Owner::Global, "gamer").is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("anna".to_string()), "health")
            .is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("anna".to_string()), "alive")
            .is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("anna".to_string()), "wait")
            .is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("anna".to_string()), "shoot")
            .is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("bob".to_string()), "health")
            .is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("bob".to_string()), "alive")
            .is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("bob".to_string()), "wait")
            .is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("bob".to_string()), "shoot")
            .is_some());
    }

    #[test]
    fn test_symbol_02() {
        // State vars can refer to themselves in the update clause
        let input1 = br"
        foo : [1 .. 10] init 1;
        foo' = foo;
        ";
        let lcgs1 = IntermediateLCGS::create(parse_lcgs(input1).unwrap()).unwrap();
        assert_eq!(lcgs1.symbols.len(), 1);
        assert!(lcgs1.symbols.get(&Owner::Global, "foo").is_some());

        // But other declarations cannot refer to themselves
        let input2 = br"
        label foo = foo > 0;
        ";
        let lcgs2 =
            std::panic::catch_unwind(|| IntermediateLCGS::create(parse_lcgs(input2).unwrap()));
        assert!(lcgs2.is_err());
    }

    #[test]
    fn test_state_translation_01() {
        // Is translation back and forth between state and index correct
        let input = br"
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
        let input = br"
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
    fn test_labels_01() {
        // Are the expected labels present
        let input = br"
        foo : [0 .. 9] init 0;
        foo' = foo;
        bar : [0 .. 5] init 0;
        bar' = bar;
        label cool = foo;
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let labels = lcgs.labels(23);
        assert!(labels.contains(&0usize));
    }

    #[test]
    fn test_labels_02() {
        // Are the expected labels present
        let input = br"
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
        assert!(!labels.contains(&1usize));
        assert!(labels.contains(&2usize));
    }

    #[test]
    fn test_labels_03() {
        // Are the expected labels present
        // With players and templates
        let input = br"
        foo : [0 .. 9] init 0;
        foo' = foo;
        player p1 = something;
        player p2 = something;
        template something
            label yes = foo == 5;
            [wait] 1;
        endtemplate
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let labels = lcgs.labels(5);
        assert!(labels.contains(&0usize));
        assert!(labels.contains(&1usize));
    }

    #[test]
    fn test_move_count_01() {
        // Are the expected moves available
        let input = br"
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
        let input = br"
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
        let input = br"
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
    fn test_initial_state_01() {
        // Is initial state what we expect?
        let input = br"
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
        let input = br"
        foo : [5 .. 9] init 6;
        foo' = foo;
        bar : [1 .. 6] init 1;
        bar' = bar;
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        assert_eq!(1, lcgs.initial_state_index());
    }
}
