use crate::atl::Phi;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};

use crate::game_structure::lcgs::eval::Evaluator;
use crate::game_structure::lcgs::query::convert_expr_to_phi;
use crate::game_structure::lcgs::symbol_checker::{symbol_check, SymbolRegistry};
use crate::game_structure::lcgs::symbol_table::SymbIdx;
use crate::game_structure::{ActionIdx, GameStructure, PlayerIdx, PropIdx, StateIdx};
use crate::parsing::ast::{Decl, DeclKind, Expr, LcgsRoot};
use crate::parsing::errors::ErrorLog;

/// A struct that holds information about players for the intermediate representation
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Player {
    pub player_index: PlayerIdx,
    pub symbol_index: SymbIdx,
    pub actions: Vec<SymbIdx>,
}

impl Player {
    pub fn new(index: PlayerIdx, symbol_index: SymbIdx) -> Player {
        Player {
            player_index: index,
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
        let SymbolRegistry {
            symbols,
            players,
            labels,
            vars,
        } = symbol_check(root, &errors)?;

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
        self.decls
            .iter()
            .find(|decl| &decl.ident.to_string() == &name)
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
    pub(crate) fn available_actions(&self, state: &State, player: PlayerIdx) -> Vec<SymbIdx> {
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
        self.decls[self.players[player.0].symbol_index.0]
            .ident
            .to_string()
    }

    fn action_name(&self, state: StateIdx, player: PlayerIdx, action: ActionIdx) -> String {
        let state = self.state_from_index(state);
        let actions = self.available_actions(&state, player);
        self.decls[actions[action.0].0].ident.to_string()
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::game_structure::lcgs::intermediate::{IntermediateLcgs, State};
    use crate::game_structure::lcgs::symbol_table::SymbIdx;
    use crate::game_structure::{ActionIdx, GameStructure, PlayerIdx, PropIdx, StateIdx};
    use crate::parsing::ast::DeclKind;
    use crate::parsing::errors::ErrorLog;
    use crate::parsing::parse_lcgs;

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
        let errors = ErrorLog::new();
        let lcgs = IntermediateLcgs::create(parse_lcgs(input, &errors).unwrap(), &errors).unwrap();
        assert!(errors.is_empty());
        assert_eq!(lcgs.decls.len(), 12);
        assert_eq!(&lcgs.decls[0].ident.to_string(), "max_health");
        assert_eq!(&lcgs.decls[1].ident.to_string(), "alice");
        assert_eq!(&lcgs.decls[2].ident.to_string(), "bob");
        assert_eq!(&lcgs.decls[3].ident.to_string(), "gamer");
        assert_eq!(&lcgs.decls[4].ident.to_string(), "alice.health");
        assert_eq!(&lcgs.decls[5].ident.to_string(), "alice.alive");
        assert_eq!(&lcgs.decls[6].ident.to_string(), "alice.wait");
        assert_eq!(&lcgs.decls[7].ident.to_string(), "alice.shoot");
        assert_eq!(&lcgs.decls[8].ident.to_string(), "bob.health");
        assert_eq!(&lcgs.decls[9].ident.to_string(), "bob.alive");
        assert_eq!(&lcgs.decls[10].ident.to_string(), "bob.wait");
        assert_eq!(&lcgs.decls[11].ident.to_string(), "bob.shoot");
    }

    #[test]
    fn test_symbol_02() {
        // State vars can refer to themselves in the update clause
        let input1 = "
        foo : [1 .. 10] init 1;
        foo' = foo;
        ";
        let errors = ErrorLog::new();
        let lcgs1 =
            IntermediateLcgs::create(parse_lcgs(input1, &errors).unwrap(), &errors).unwrap();
        assert!(errors.is_empty());
        assert_eq!(lcgs1.decls.len(), 1);
        assert_eq!(&lcgs1.decls[0].ident.to_string(), "foo");

        // But other declarations cannot refer to themselves
        let input2 = "
        label foo = foo > 0;
        ";
        let lcgs2 = IntermediateLcgs::create(parse_lcgs(input2, &errors).unwrap(), &errors);
        assert!(lcgs2.is_err());
    }

    #[test]
    fn test_relabeling_01() {
        // Check standard use of relabeling
        let input = "
        const max_health = 100;
        player alice = gamer [enemy=bob];
        player bob = gamer [enemy=alice];

        template gamer
            health : [0 .. max_health] init max_health;
            health' = enemy.shoot ? health - 1 : health;

            label alive = health > 0;

            [wait] 1;
            [shoot] health > 0 && enemy.health > 0;
        endtemplate
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        assert!(errors.is_empty());
        assert_eq!(lcgs.decls.len(), 12);
        assert_eq!(&lcgs.decls[0].ident.to_string(), "max_health");
        assert_eq!(&lcgs.decls[1].ident.to_string(), "alice");
        assert_eq!(&lcgs.decls[2].ident.to_string(), "bob");
        assert_eq!(&lcgs.decls[3].ident.to_string(), "gamer");
        assert_eq!(&lcgs.decls[4].ident.to_string(), "alice.health");
        assert_eq!(&lcgs.decls[5].ident.to_string(), "alice.alive");
        assert_eq!(&lcgs.decls[6].ident.to_string(), "alice.wait");
        assert_eq!(&lcgs.decls[7].ident.to_string(), "alice.shoot");
        assert_eq!(&lcgs.decls[8].ident.to_string(), "bob.health");
        assert_eq!(&lcgs.decls[9].ident.to_string(), "bob.alive");
        assert_eq!(&lcgs.decls[10].ident.to_string(), "bob.wait");
        assert_eq!(&lcgs.decls[11].ident.to_string(), "bob.shoot");
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        assert!(errors.is_empty());
        assert_eq!(lcgs.decls.len(), 9);
        assert_eq!(&lcgs.decls[0].ident.to_string(), "anna");
        assert_eq!(&lcgs.decls[1].ident.to_string(), "bob");
        assert_eq!(&lcgs.decls[2].ident.to_string(), "human");
        assert_eq!(&lcgs.decls[3].ident.to_string(), "anna.apples");
        assert_eq!(&lcgs.decls[4].ident.to_string(), "anna.dance");
        assert_eq!(&lcgs.decls[5].ident.to_string(), "anna.happy");
        assert_eq!(&lcgs.decls[6].ident.to_string(), "bob.bananas");
        assert_eq!(&lcgs.decls[7].ident.to_string(), "bob.run");
        assert_eq!(&lcgs.decls[8].ident.to_string(), "bob.sad");
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        assert!(errors.is_empty());
        assert_eq!(lcgs.decls.len(), 9);
        assert_eq!(&lcgs.decls[0].ident.to_string(), "anna");
        assert_eq!(&lcgs.decls[1].ident.to_string(), "bob");
        assert_eq!(&lcgs.decls[2].ident.to_string(), "human");
        assert_eq!(&lcgs.decls[3].ident.to_string(), "anna.money");
        assert_eq!(&lcgs.decls[4].ident.to_string(), "anna.wait");
        assert_eq!(&lcgs.decls[5].ident.to_string(), "anna.work");
        assert_eq!(&lcgs.decls[6].ident.to_string(), "bob.money");
        assert_eq!(&lcgs.decls[7].ident.to_string(), "bob.wait");
        assert_eq!(&lcgs.decls[8].ident.to_string(), "bob.act");
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let index = StateIdx(23);
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let indexes = [12, 55, 126, 78, 99].map(StateIdx);
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let index = StateIdx(14);
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let indexes = [12, 55, 126, 78, 99, 150, 555, 992, 1001, 733].map(StateIdx);
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let indexes = [
            StateIdx(12_340),
            StateIdx(1_987_158),
            StateIdx(3_000_000_000),
        ];
        for i in &indexes {
            let state = lcgs.state_from_index(*i);
            let i2 = lcgs.index_of_state(&state);
            assert_eq!(*i, i2);
        }

        // State to index to state
        let mut map: HashMap<SymbIdx, i32> = HashMap::new();
        map.insert(lcgs.get_decl_by_name("foo").unwrap().index, 2_000_000);
        map.insert(lcgs.get_decl_by_name("bar").unwrap().index, 2_000_000);
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        assert!(lcgs.get_decl_by_name("t").is_some());
    }

    /// Helper function to get the index of a label with the given symbol name
    fn get_label_index(lcgs: &IntermediateLcgs, label_name: &str) -> PropIdx {
        if let Some(decl) = lcgs.get_decl_by_name(label_name) {
            if let DeclKind::StateLabel(idx, _) = &decl.kind {
                *idx
            } else {
                panic!("'{}' is not a state label", label_name)
            }
        } else {
            panic!("'{}' does not exist", label_name);
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let labels = lcgs.labels(StateIdx(23));
        assert!(labels.contains(&PropIdx(0)));
        assert_eq!(get_label_index(&lcgs, "cool"), PropIdx(0));
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let labels = lcgs.labels(StateIdx(46));
        assert!(labels.contains(&PropIdx(0)));
        assert_eq!(get_label_index(&lcgs, "cool"), PropIdx(0));
        assert!(!labels.contains(&PropIdx(1)));
        assert_eq!(get_label_index(&lcgs, "great"), PropIdx(1));
        assert!(labels.contains(&PropIdx(2)));
        assert_eq!(get_label_index(&lcgs, "awesome"), PropIdx(2));
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let labels = lcgs.labels(StateIdx(5));
        assert!(!labels.contains(&PropIdx(0)));
        assert_eq!(get_label_index(&lcgs, "no"), PropIdx(0));
        assert!(labels.contains(&PropIdx(1)));
        assert_eq!(get_label_index(&lcgs, "p1.yes"), PropIdx(1));
        assert!(labels.contains(&PropIdx(2)));
        assert_eq!(get_label_index(&lcgs, "p2.yes"), PropIdx(2));
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let move_count = lcgs.move_count(StateIdx(4));
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let next_state = lcgs.get_successor(StateIdx(0), &[ActionIdx(0)]);
        assert_eq!(StateIdx(1), next_state);
        let next_next_state = lcgs.get_successor(next_state, &[ActionIdx(0)]);
        assert_eq!(StateIdx(0), next_next_state);
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        assert_eq!(
            StateIdx(0),
            lcgs.get_successor(StateIdx(0), &[ActionIdx(0)])
        );
        assert_eq!(
            StateIdx(1),
            lcgs.get_successor(StateIdx(0), &[ActionIdx(1)])
        );
        assert_eq!(
            StateIdx(0),
            lcgs.get_successor(StateIdx(1), &[ActionIdx(0)])
        );
        assert_eq!(
            StateIdx(1),
            lcgs.get_successor(StateIdx(1), &[ActionIdx(1)])
        );
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let init_state = lcgs.initial_state_index();
        assert_eq!(StateIdx(0), init_state);
        assert_eq!(StateIdx(0), lcgs.get_successor(init_state, &[ActionIdx(0)]));
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        assert_eq!(StateIdx(2), lcgs.initial_state_index());
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        assert_eq!(StateIdx(1), lcgs.initial_state_index());
    }

    /// Helper function to get the index of a player with the given name
    fn get_player_index(lcgs: &IntermediateLcgs, player_name: &str) -> PlayerIdx {
        if let Some(decl) = lcgs.get_decl_by_name(player_name) {
            if let DeclKind::Player(player) = &decl.kind {
                player.index
            } else {
                panic!("'{}' is not a player", player_name)
            }
        } else {
            panic!("'{}' does not exists", player_name);
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        assert_eq!(get_player_index(&lcgs, "p1"), PlayerIdx(0));
        assert_eq!(get_player_index(&lcgs, "p2"), PlayerIdx(1));
        assert_eq!(get_player_index(&lcgs, "p3"), PlayerIdx(2));
    }
}
