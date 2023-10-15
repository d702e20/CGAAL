use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::ops::DerefMut;
use crate::atl::Phi;

use crate::game_structure::{ActionIdx, GameStructure, PlayerIdx, PropIdx, StateIdx};
use crate::game_structure::lcgs::eval::Evaluator;
use crate::game_structure::lcgs::query::convert_expr_to_phi;
use crate::game_structure::lcgs::relabeling::Relabeler;
use crate::game_structure::lcgs::symbol_checker::{CheckMode, SymbolChecker};
use crate::game_structure::lcgs::symbol_table::{SymbIdx, SymbolTable};
use crate::parsing::ast::{Decl, DeclKind, Expr, ExprKind, Ident, LcgsRoot};
use crate::parsing::errors::{ErrorLog, SpannedError};
use crate::parsing::span::NO_SPAN;

/// A struct that holds information about players for the intermediate representation
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Player {
    pub index: PlayerIdx,
    pub symbol_index: SymbIdx,
    pub actions: Vec<SymbIdx>,
}

impl Player {
    pub fn new(index: PlayerIdx, symbol_index: SymbIdx) -> Player {
        Player {
            index,
            symbol_index,
            actions: vec![],
        }
    }
}

/// An [IntermediateLCGS] is created from processing an AST and checking the validity of the
/// declarations.
#[derive(Clone, Debug)]
pub struct IntermediateLcgs {
    decls: Vec<Decl>,
    labels: Vec<SymbIdx>,
    vars: Vec<SymbIdx>,
    players: Vec<Player>,
}

impl IntermediateLcgs {
    /// Create an [IntermediateLCGS] from an AST root. All declarations in the resulting
    /// [IntermediateLCGS] are symbol checked and type checked.
    pub fn create(root: LcgsRoot, errors: &ErrorLog) -> Result<IntermediateLcgs, ()> {
        let mut symbols = SymbolTable::new();

        // Register global decls. Then check and optimize them
        let (players, labels, vars) = register_decls(&mut symbols, root).map_err(|se| errors.log_err(se))?;
        check_and_optimize_decls(&symbols).map_err(|se| errors.log_err(se))?;

        let ilcgs = IntermediateLcgs {
            decls: symbols.solidify(),
            labels,
            vars,
            players,
        };

        Ok(ilcgs)
    }

    /// Convert an ATL expression to a [Phi] that can be used in a query.
    pub fn create_phi(&self, expr: Expr, errors: &ErrorLog) -> Result<Phi, ()> {
        convert_expr_to_phi(expr, &self, errors)
    }

    pub fn get_decl(&self, symbol: &SymbIdx) -> Option<&Decl> {
        self.decls.get(symbol.0)
    }

    pub fn get_decl_by_name(&self, name: &str) -> Option<&Decl> {
        self.decls.iter().find(|decl| &decl.ident.to_string() == &name)
    }

    /// Transforms a state index to a [State].
    pub(crate) fn state_from_index(&self, state_index: StateIdx) -> State {
        let mut state = State(HashMap::new());
        let mut carry = state_index.0;

        // The following method resembles the typical way of transforming a number of seconds
        // into seconds, minutes, hours, and days. In this case the time units are state variables
        // instead, and similarly to time units, each state variable has a different size.
        for symb_id in &self.vars {
            let symb = self.decls.get(symb_id.0).unwrap();
            if let DeclKind::StateVar(var) = &symb.kind {
                let value = {
                    let size = (var.range.val.end() - var.range.val.start() + 1) as usize;
                    let quotient = carry / size;
                    let remainder = carry.rem_euclid(size);
                    carry = quotient;
                    var.range.val.start() + remainder as i32
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

    pub fn label_index_to_decl(&self, label_index: PropIdx) -> &Decl {
        let label_symbol = self.labels[label_index.0].clone();

        let label_decl = self.decls.get(label_symbol.0).unwrap();
        label_decl
    }

    /// Transforms a state into its index
    pub(crate) fn index_of_state(&self, state: &State) -> StateIdx {
        let mut combined_size = 1;
        let mut res = 0usize;

        // The following method resembles the typical way of transforming a number of seconds,
        // minutes, hours, and days into just seconds. In this case the time units are
        // state variables instead, and similarly to time units, each state variable has a
        // different size.
        for symb_id in &self.vars {
            let symb = self.decls.get(symb_id.0).unwrap();
            if let DeclKind::StateVar(var) = &symb.kind {
                let size = (var.range.val.end() - var.range.val.start() + 1) as usize;
                let val = state.0.get(symb_id).unwrap();
                res += (val - var.range.val.start()) as usize * combined_size;
                combined_size *= size;
            }
        }
        StateIdx(res)
    }

    /// Returns a list of the moves available to the given player in the given state.
    pub(crate) fn available_actions(
        &self,
        state: &State,
        player: PlayerIdx,
    ) -> Vec<SymbIdx> {
        self.players[player.0]
            .actions
            .iter()
            .filter(|symb_id| {
                let symb = &self.decls[symb_id.0];
                if let DeclKind::Action(cond) = &symb.kind {
                    // The action is available if the condition is not evaluated to 0 in this state
                    return 0 != Evaluator::new(state).eval(&cond);
                }
                panic!("Action was not a action.")
            })
            .cloned()
            .collect()
    }

    /// Returns the initial state of the LCGS game
    pub fn initial_state(&self) -> State {
        let mut res = State(HashMap::new());
        for symb_id in &self.vars {
            let symb = &self.decls[symb_id.0];
            if let DeclKind::StateVar(var) = &symb.kind {
                res.0.insert(symb_id.clone(), var.init_val);
            }
        }
        res
    }

    /// Returns the variables that make up a state
    pub fn get_vars(&self) -> &[SymbIdx] {
        &self.vars
    }

    /// Returns a vector of players
    pub fn get_players(&self) -> &[Player] {
        &self.players
    }

    /// Returns vector of labels
    pub fn get_labels(&self) -> &[SymbIdx] {
        &self.labels
    }
}

/// Names of declarations. First component is players and their fields. Second component
/// is global labels. And third component is global variables.
type DeclNames = (Vec<Player>, Vec<SymbIdx>, Vec<SymbIdx>);

/// Registers all declarations from the root in the symbol table. Constants are optimized to
/// numbers immediately. On success, a vector of [Player]s is returned with information
/// about players and the names of their actions.
fn register_decls(
    symbols: &mut SymbolTable,
    root: LcgsRoot,
) -> Result<DeclNames, SpannedError> {
    let mut players = vec![];
    let mut labels = vec![];
    let mut vars = vec![];

    let mut next_player_index = 0;
    let mut next_label_index = 0;

    // Register global declarations.
    // Constants are evaluated immediately.
    // Players are put in a separate vector and handled afterwards.
    // Symbol table is given ownership of the declarations.
    for Decl { span, ident, kind } in root.decls {
        match kind {
            DeclKind::Const(expr) => {
                // We can evaluate constants immediately as constants can only
                // refer to other constants that are above them in the program.
                // If they don't reduce to a single number, then the SymbolChecker
                // produces an error.
                let reduced = SymbolChecker::new(symbols, &None, CheckMode::ConstExpr).check_eval(&expr)?;
                let decl = Decl::new(span, ident.name, DeclKind::Const(Expr::new(expr.span, ExprKind::Num(reduced))));
                let _ = symbols.insert(decl).map_err(|msg| SpannedError::new(span, msg))?;
            }
            DeclKind::StateLabel(_, expr) => {
                // Insert in symbol table and add to labels list
                let decl = Decl::new(span, ident.name, DeclKind::StateLabel(PropIdx(next_label_index), expr));
                let index = symbols.insert(decl).map_err(|msg| SpannedError::new(span, msg))?;
                labels.push(index);
                next_label_index += 1;
            }
            DeclKind::StateVar(state_var) => {
                // Insert in symbol table and add to vars list
                let decl = Decl::new(span, ident.name, DeclKind::StateVar(state_var));
                let index = symbols.insert(decl).map_err(|msg| SpannedError::new(span, msg))?;
                vars.push(index);
            }
            DeclKind::Template(inner_decls) => {
                let decl = Decl::new(span, ident.name, DeclKind::Template(inner_decls));
                let _ = symbols.insert(decl).map_err(|msg| SpannedError::new(span, msg))?;
            }
            DeclKind::Player(mut player) => {
                player.index = PlayerIdx(next_player_index);
                let decl = Decl::new(span, ident.name, DeclKind::Player(player));
                let index = symbols.insert(decl).map_err(|msg| SpannedError::new(span, msg))?;
                players.push(index);
                next_player_index += 1;
            }
            _ => panic!("Not a global declaration. Parser must have failed."),
        }
    }

    // Register player declarations. Here we clone the declarations since multiple
    // players can use the same template
    let mut players_vec = vec![];
    for (index, symbol_index) in players.drain(..).enumerate() {
        let pdecl_rc = symbols.get(symbol_index).borrow();
        let DeclKind::Player(pdecl) = &pdecl_rc.kind else { unreachable!() };
        let mut player = Player::new(PlayerIdx(index), symbol_index);

        let tdecl_opt = symbols.get_by_name(&pdecl.template_ident.to_string());
        let Some(tdecl_rc) = tdecl_opt else {
            return Err(SpannedError::new(
                pdecl.template_ident.span,
                format!("Undeclared template '{}'", pdecl.template_ident),
            ));
        };
        let tdecl = tdecl_rc.borrow();
        let DeclKind::Template(inner_decls) = &tdecl.kind else {
            return Err(SpannedError::new(
                tdecl.span,
                format!("'{}' is not a template.", tdecl.ident),
            ));
        };

        let relabeler = Relabeler::new(&pdecl.relabellings);

        // Go through each declaration in the template and register a relabeled
        // clone of it that is owned by the given player // FIXME docs
        let new_decls = inner_decls.iter().map(|idecl| {
            let mut new_decl = relabeler.relabel_decl(idecl.clone())?;
            new_decl.ident.owner = Some(Ident::new(NO_SPAN, pdecl_rc.ident.name.text.clone()));
            // FIXME Updates indexes!
            Ok((new_decl, idecl.span))
        }).collect::<Result<Vec<_>, _>>()?;

        drop(pdecl_rc);
        drop(tdecl);

        for (new_decl, span) in new_decls {
            let vec = match &new_decl.kind {
                DeclKind::StateLabel(_, _) => &mut labels,
                DeclKind::StateVar(_) => &mut vars,
                DeclKind::Action(_) => &mut player.actions,
                _ => panic!(
                    "Not a declaration allowed in templates. Parser must have failed."
                ),
            };
            let index = symbols.insert(new_decl).map_err(|msg| SpannedError::new(span, msg))?;
            vec.push(index);
        }

        // The player is done. We can now register the player declaration.
        players_vec.push(player);
    }
    Ok((players_vec, labels, vars))
}

/// Reduces the declarations in a [SymbolTable] to a more compact version, if possible.
/// Validity of identifiers are also checked and resolved.
fn check_and_optimize_decls(symbols: &SymbolTable) -> Result<(), SpannedError> {
    for symbol in symbols.iter() {
        // Optimize the declaration's expression(s)
        let mut decl_ref = symbol.borrow_mut();
        let Decl {
            span: _,
            kind,
            ident,
        } = decl_ref.deref_mut();
        match kind {
            DeclKind::StateLabel(_, expr) => {
                *expr =
                    SymbolChecker::new(symbols, &ident.owner, CheckMode::StateExpr)
                        .check(&expr)?;
            }
            DeclKind::StateVar(var) => {
                // Both initial value, min, and max are expected to be constant.
                // Hence, we also evaluate them now so we don't have to do that each time.
                // FIXME: These constants are not restricted to use only constants declared above it
                let checker = SymbolChecker::new(symbols, &ident.owner, CheckMode::ConstExpr);
                var.init_val = checker.check_eval(&var.init)?;
                let min = checker.check_eval(&var.range.min)?;
                let max = checker.check_eval(&var.range.max)?;
                var.range.val = min..=max;
                if !var.range.val.contains(&var.init_val) {
                    return Err(SpannedError::new(
                        var.init.span,
                        format!(
                            "Initial value {} is not in range {}..{}",
                            var.init_val, min, max
                        ),
                    ));
                }
                var.update =
                    SymbolChecker::new(symbols, &ident.owner, CheckMode::UpdateExpr)
                        .check(&var.update)?;
            }
            DeclKind::Action(cond) => {
                *cond =
                    SymbolChecker::new(symbols, &ident.owner, CheckMode::StateExpr)
                        .check(&cond)?;
            }
            // Needs no symbol check or evaluate
            DeclKind::Player(_) => {}
            DeclKind::Template(_) => {}
            DeclKind::Const(_) => {}
            DeclKind::Error => {}
        }
    }
    Ok(())
}

/// A game structure state of an LCGS. Holds a mapping of symbol names to their current value
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct State(pub HashMap<SymbIdx, i32>);

impl Display for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        let len = self.0.len();
        for (i, (symb_id, val)) in self.0.iter().enumerate() {
            write!(f, "{}:{}", symb_id, val)?;
            if i < len - 1 {
                write!(f, ",")?;
            }
        }
        write!(f, "}}")
    }
}

impl GameStructure for IntermediateLcgs {
    fn initial_state_index(&self) -> StateIdx {
        self.index_of_state(&self.initial_state())
    }

    fn player_count(&self) -> usize {
        self.players.len()
    }

    /// Returns the set of labels/propositions available in the given state.
    fn labels(&self, state: StateIdx) -> HashSet<PropIdx> {
        let state = self.state_from_index(state);
        let mut res = HashSet::new();

        for symb_id in &self.labels {
            let symb = &self.decls[symb_id.0];
            if let DeclKind::StateLabel(idx, cond) = &symb.kind {
                // We evaluate the condition with the values of the current state to know
                // whether the label is present or not
                let value = Evaluator::new(&state).eval(&cond);
                if value != 0 {
                    res.insert(*idx);
                }
            }
        }
        res
    }

    /// Returns the next state given a current state and an action for each player.
    fn get_successor(&self, state: StateIdx, choices: &[ActionIdx]) -> StateIdx {
        let mut state = self.state_from_index(state);
        // To evaluate the next state we assign the actions to either 1 or 0 depending
        // on whether or not the action was taken
        for (p_index, player) in self.players.iter().enumerate() {
            // ActionIdx is a index into the currently available actions
            let moves = self.available_actions(&state, PlayerIdx(p_index));
            debug_assert!(
                choices[p_index].0 < moves.len(),
                "Unknown action {} chosen for player {}. They only have {} actions available in state {:?}",
                choices[p_index],
                p_index,
                moves.len(),
                state
            );

            // First set all actions (also the unavailable actions) to 0,
            // then set the chosen action to 1
            for action in &player.actions {
                state.0.insert(action.clone(), 0);
            }
            let act = moves[choices[p_index].0];
            state.0.insert(act, 1);
        }

        // Now we can evaluate the next state based on previous state and the actions taken
        let evaluator = Evaluator::new(&state);
        let mut next_state = State(HashMap::new());
        for symb_id in &self.vars {
            let symb = &self.decls[symb_id.0];
            if let DeclKind::StateVar(var) = &symb.kind {
                let val = evaluator.eval(&var.update);
                next_state.0.insert(symb_id.clone(), val);
            }
        }

        self.index_of_state(&next_state)
    }

    /// Returns the number of moves available to each player in the given state.
    fn move_count(&self, state: StateIdx) -> Vec<usize> {
        let state = self.state_from_index(state);
        self.players
            .iter()
            .enumerate()
            .map(|(i, _player)| self.available_actions(&state, PlayerIdx(i)).len())
            .collect()
    }

    fn state_name(&self, state: StateIdx) -> String {
        self.state_from_index(state).to_string()
    }

    fn label_name(&self, proposition: PropIdx) -> String {
        self.decls[self.labels[proposition.0].0].ident.to_string()
    }

    fn player_name(&self, player: PlayerIdx) -> String {
        self.decls[self.players[player.0].index.0].ident.to_string()
    }

    fn action_name(
        &self,
        state: StateIdx,
        player: PlayerIdx,
        action: ActionIdx,
    ) -> String {
        let state = self.state_from_index(state);
        let actions = self.available_actions(&state, player);
        self.decls[actions[action.0].0].ident.to_string()
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::game_structure::GameStructure;
    use crate::game_structure::lcgs::ast::DeclKind;
    use crate::game_structure::lcgs::ir::intermediate::{IntermediateLcgs, State};
    use crate::game_structure::lcgs::ir::symbol_table::Owner;
    use crate::game_structure::lcgs::ir::symbol_table::SymbolIdentifier;
    use crate::game_structure::lcgs::parse::parse_lcgs;

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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
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
        let lcgs1 = IntermediateLcgs::create(parse_lcgs(input1).unwrap()).unwrap();
        assert_eq!(lcgs1.symbols.len(), 1);
        assert!(lcgs1.symbols.get(&":global.foo".into()).is_some());

        // But other declarations cannot refer to themselves
        let input2 = "
        label foo = foo > 0;
        ";
        let lcgs2 = IntermediateLcgs::create(parse_lcgs(input2).unwrap());
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let index = 14;
        let state = lcgs.state_from_index(index);
        let index2 = lcgs.index_of_state(&state);
        assert_eq!(index, index2);
    }

    #[test]
    fn test_state_translation_04() {
        // Is translation back and forth between state and index correct
        // Player vars and wack ranges
        let input = "
        foo : [5 .. 20] init 5;
        foo' = foo;
        bar : [-2 .. 5] init 3;
        bar' = bar;

        player p1 = test;
        player p2 = test;
        player p3 = test;

        template test
            yum : [0 .. 8] init 0;
            yum' = yum;
            zap : [-2 .. 3] init -1;
            zap' = zap;
            [wait] 1;
        endtemplate
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let indexes = [12, 55, 126, 78, 99, 150, 555, 992, 1001, 733];
        for i in &indexes {
            let state = lcgs.state_from_index(*i);
            let i2 = lcgs.index_of_state(&state);
            assert_eq!(*i, i2);
        }
    }

    #[test]
    fn test_state_translation_05() {
        // Is translation back and forth between state and index correct
        // Big ranges
        let input = "
        foo : [0 .. 2000000] init 5;
        foo' = foo;
        bar : [0 .. 2000000] init 3;
        bar' = bar;
        ";

        // Index to state to index
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let indexes = [12_340, 1_987_158, 3_000_000_000];
        for i in &indexes {
            let state = lcgs.state_from_index(*i);
            let i2 = lcgs.index_of_state(&state);
            assert_eq!(*i, i2);
        }

        // State to index to state
        let mut map: HashMap<SymbolIdentifier, i32> = HashMap::new();
        map.insert(":global.foo".into(), 2_000_000);
        map.insert(":global.bar".into(), 2_000_000);
        let state = State(map);
        let index = lcgs.index_of_state(&state);
        let state2 = lcgs.state_from_index(index);
        assert_eq!(state, state2)
    }

    #[test]
    fn test_negation_const_01() {
        let input = "
        const t = -5;
        ";
        let pp = parse_lcgs(input);
        let lcgs = IntermediateLcgs::create(pp.unwrap()).unwrap();
        assert!(lcgs.symbols.get(&":global.t".into()).is_some());
    }

    /// Helper function to get the index of a label with the given symbol name
    fn get_label_index(lcgs: &IntermediateLcgs, symbol_name: &str) -> usize {
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        assert_eq!(1, lcgs.initial_state_index());
    }

    /// Helper function to get the index of a player with the given name
    fn get_player_index(lcgs: &IntermediateLcgs, player_name: &str) -> usize {
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
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        assert_eq!(get_player_index(&lcgs, "p1"), 0usize);
        assert_eq!(get_player_index(&lcgs, "p2"), 1usize);
        assert_eq!(get_player_index(&lcgs, "p3"), 2usize);
    }
}
