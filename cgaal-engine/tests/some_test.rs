use cgaal_engine::{
    algorithms::{
        certain_zero::{
            distributed_certain_zero, search_strategy::bfs::BreadthFirstSearchBuilder,
            CertainZeroResult,
        },
        game_strategy::{
            compute_game_strategy, partial::PartialStrategy, WitnessStrategy,
            error::Error,
        },
    },
    atl::parse_phi,
    edg::atledg::{vertex::AtlVertex, AtlDependencyGraph},
    game_structure::{
        lcgs::{ir::intermediate::IntermediateLcgs, parse::parse_lcgs},
        GameStructure,
    },
};

mod common;

const GAME: &str = "
player p1 = thing;
player p2 = thing;

template thing
    a : [0..2] init 0;
    a' = gt1 + !gt1 * min(a + inc, 2);

    label at_0 = a == 0;
    label at_1 = a == 1;
    label at_2 = a == 2;

    [wait] 1;
    [gt1] 1;
    [inc] 1;
endtemplate

label p2_ahead = p1.a < p2.a;
";

// States, STATE_<p2.a><p1.a>
const STATE_00: usize = 0;
const STATE_01: usize = 1;
const STATE_02: usize = 2;
const STATE_10: usize = 3;
const STATE_11: usize = 4;
const STATE_12: usize = 5;
const STATE_20: usize = 6;
const STATE_21: usize = 7;
const STATE_22: usize = 8;

const ACT_P1_WAIT: usize = 0;
const ACT_P1_GT1: usize = 1;
const ACT_P1_INC: usize = 2;

const FORMULA_NO_STRATEGY_NEEDED: &str = "p1.at_0";
const FORMULA_UNSUPPORTED: &str = "<<p1>> F (<<p2>> F p1.at_2)";
const FORMULA_ENFORCE_NEXT_TRUE: &str = "<<p1>> X p1.at_1";
const FORMULA_ENFORCE_NEXT_FALSE: &str = "<<p1>> X p1.at_2";
const FORMULA_ENFORCE_UNTIL_TRUE: &str = "<<p1>> (true U p1.at_2)";
const FORMULA_ENFORCE_UNTIL_FALSE: &str = "<<p1>> (p2.at_0 U false)";
const FORMULA_ENFORCE_EVENTUALLY_TRUE: &str = "<<p1>> F p1.at_2";
const FORMULA_ENFORCE_EVENTUALLY_FALSE: &str = "<<p1>> F false";
const FORMULA_ENFORCE_INVARIANTLY_TRUE: &str = "<<p1>> G p1.at_0";
const FORMULA_ENFORCE_INVARIANTLY_FALSE: &str = "<<p1>> G p2.at_0";
const FORMULA_DESPITE_NEXT_TRUE: &str = "[[p1]] X !p2.at_2";
const FORMULA_DESPITE_NEXT_FALSE: &str = "[[p1]] X p1.at_1";
const FORMULA_DESPITE_UNTIL_TRUE: &str = "[[p1]] (true U p2.at_2)";
const FORMULA_DESPITE_UNTIL_UNDECIDED: &str = "[[p1]] (true U p2_ahead)";
const FORMULA_DESPITE_UNTIL_FALSE: &str = "[[p1]] (!p1.at_2 U false)";
const FORMULA_DESPITE_EVENTUALLY_TRUE: &str = "[[p1]] F p2.at_2";
const FORMULA_DESPITE_EVENTUALLY_UNDECIDED: &str = "[[p1]] F p1.at_2";
const FORMULA_DESPITE_INVARIANTLY_TRUE: &str = "[[p1]] G p2.at_2";
const FORMULA_DESPITE_INVARIANTLY_FALSE: &str = "[[p1]] G !p1.at_2";

const WORKER_COUNT: u64 = 4;

/// This macro is used to assert that a partial strategy is correct.
/// Validating this is tricky due to the partial-ness: Not all states needs to be assigned a move,
/// but if it is assigned a move, that move must be valid.
/// Additionally, there can be more than one valid move.
/// This macro allows us to easily express which moves are valid in each state and whether
/// some move is required or not in each state.
///
/// The first argument is the name of the map which maps states to partial moves.
/// The following arguments are of the following form: STATE => MOVES.
/// The moves are a comma-separated list of valid moves for that state.
/// If the first move is '@', then a move is not require for this state (@=None).
/// If '@' is the only move listed, then p1 must not be assigned a move in the given state.
/// The macro can only assert the strategy of player 0 (p1).
///
/// Example:
/// ```ignore
/// assert_partial_strat_moves!(
///     move_to_pick;
///     STATE_00 => ACT_P1_GT1, ACT_P1_INC;    // In STATE_00, p1 must choose ACT_P1_GT1 or ACT_P1_INC
///     STATE_01 => ACT_P1_INC;
///     STATE_02 => @;                         // In STATE_02, p1 must not need to have a move
///     STATE_10 => @, ACT_P1_GT1, ACT_P1_INC; // In STATE_10, p1 may choose ACT_P1_GT1 or ACT_P1_INC
///     STATE_11 => ACT_P1_INC;
///     STATE_12 => @;
///     STATE_20 => @, ACT_P1_GT1, ACT_P1_INC;
///     STATE_21 => @, ACT_P1_INC;
///     STATE_22 => @;
/// );
/// ```
///
macro_rules! assert_partial_strat_moves {
    // Base case
    ($move_map:ident;) => {};
    // Must be one of ..
    ($move_map:ident; $state:ident => $($act:ident),+; $($rest:tt)*) => {
        {
            assert!(
                $move_map.get(&$state).map(|pm| matches!(pm[0].unwrap_specific(), $($act)|+)).unwrap_or(false),
                concat!("Expected move in ", stringify!($state), " ({}) to be one of [", $(stringify!($act ({}))), +,"], but move was {:?}"),
                &$state, $($act), *, &$move_map.get(&$state).map(|pm| &pm[0])
            );
        }
        assert_partial_strat_moves!($move_map; $($rest)*);
    };
    // Is None or one of ..
    ($move_map:ident; $state:ident => @, $($act:ident),+; $($rest:tt)*) => {
        {
            assert!($move_map.get(&$state).map(|pm| matches!(pm[0].unwrap_specific(), $($act)|+)).unwrap_or(true),
                concat!("Expected move in ", stringify!($state), " ({}) to be None or one of [", $(stringify!($act ({}))), +,"], but move was {:?}"),
                &$state, $($act), *, &$move_map.get(&$state).map(|pm| &pm[0])
            );
        }
        assert_partial_strat_moves!($move_map; $($rest)*);
    };
    // Must be None
    ($move_map:ident; $state:ident => @; $($rest:tt)*) => {
        {
            assert!($move_map.get(&$state).is_none(),
                concat!("Expected move in ", stringify!($state), " ({}) to be None, but move was {:?}"),
                &$state, &$move_map.get(&$state).map(|pm| &pm[0])
            );
        }
        assert_partial_strat_moves!($move_map; $($rest)*);
    };
}

macro_rules! strat_synthesis_test {
    ($game:expr, $phi:expr, $($rest:tt)*) => {
        let ast = parse_lcgs($game).unwrap();
        let game = IntermediateLcgs::create(ast).unwrap();
        let phi = parse_phi(&game, $phi).unwrap();
        let v0 = AtlVertex::Full {
            state: game.initial_state_index(),
            formula: phi.into(),
        };
        let edg = AtlDependencyGraph {
            game_structure: game,
        };
        let czr = distributed_certain_zero(
            edg.clone(),
            v0.clone(),
            WORKER_COUNT,
            BreadthFirstSearchBuilder,
            true,
            true,
        );
        let CertainZeroResult::AllFoundAssignments(ass) = czr else { unreachable!() };

        strat_synthesis_test!(@ &v0, &edg, &ass, $($rest)*);
    };
    (@ $v0:expr, $edg:expr, $ass:expr, TRUE, $($strat:tt)*) => {
        assert!($ass.get($v0).unwrap().is_true(), "Root assignment should be true, was {:?}", $ass.get($v0));
        strat_synthesis_test!(@@ $v0, $edg, $ass, $($strat)*);
    };
    (@ $v0:expr, $edg:expr, $ass:expr, FALSE, $($rest:tt)*) => {
        assert!($ass.get($v0).unwrap().is_false(), "Root assignment should be false, was {:?}", $ass.get($v0));
        strat_synthesis_test!(@@ $v0, $edg, $ass, $($rest)*);
    };
    (@ $v0:expr, $edg:expr, $ass:expr, UNDECIDED, $($rest:tt)*) => {
        assert!(!$ass.get($v0).unwrap().is_certain(), "Root assignment should be undecided, was {:?}", $ass.get($v0));
        strat_synthesis_test!(@@ $v0, $edg, $ass, $($rest)*);
    };
    (@@ $v0:expr, $edg:expr, $ass:expr, Strategy: $($moves:tt)*) => {
        let strat = compute_game_strategy($edg, $v0, $ass).unwrap();
        assert!(matches!(&strat, WitnessStrategy::Strategy(_)));

        let WitnessStrategy::Strategy(PartialStrategy {
            players,
            move_to_pick,
        }) = strat else { unreachable!() };

        assert_eq!(players.as_slice(), [0]);
        assert_partial_strat_moves!(move_to_pick; $($moves)*);
    };
    (@@ $v0:expr, $edg:expr, $ass:expr, NoStrategyExist) => {
        let strat = compute_game_strategy($edg, $v0, $ass).unwrap();
        assert!(matches!(&strat, WitnessStrategy::NoStrategyExist));
    };
    (@@ $v0:expr, $edg:expr, $ass:expr, NoStrategyNeeded) => {
        let strat = compute_game_strategy($edg, $v0, $ass).unwrap();
        assert!(matches!(&strat, WitnessStrategy::NoStrategyNeeded));
    };
    (@@ $v0:expr, $edg:expr, $ass:expr, Unsupported) => {
        assert!(matches!(compute_game_strategy($edg, $v0, $ass), Err(Error::UnsupportedFormula)));
    }
}

#[test]
fn test_strat_syn_no_strat_needed() {
    strat_synthesis_test!(
        GAME,
        "p1.at_0",
        TRUE,
        NoStrategyNeeded
    );
}

#[test]
fn strat_syn_unsupported_formula() {
    strat_synthesis_test!(
        GAME,
        "<<p1>> F (<<p2>> G false)",
        UNDECIDED,
        Unsupported
    );
}

#[ignore]
#[test]
fn strat_syn_enforce_next_true() {
    strat_synthesis_test!(
        GAME,
        "<<p1>> X p1.at_1",
        TRUE,
        Strategy:
        STATE_00 => ACT_P1_GT1, ACT_P1_INC;
        STATE_01 => @;
        STATE_02 => @;
        STATE_10 => @;
        STATE_11 => @;
        STATE_12 => @;
        STATE_20 => @;
        STATE_21 => @;
        STATE_22 => @;
    );
}

#[ignore]
#[test]
fn strat_syn_enforce_next_false() {
    strat_synthesis_test!(
        GAME,
        "<<p1>> X p1.at_2",
        TRUE,
        NoStrategyExist
    );
}

#[test]
fn strat_syn_enforce_eventually_true() {
    strat_synthesis_test!(
        GAME,
        "<<p1>> F p1.at_2",
        TRUE,
        Strategy:
        STATE_00 => ACT_P1_GT1, ACT_P1_INC;
        STATE_01 => ACT_P1_INC;
        STATE_02 => @;
        STATE_10 => @, ACT_P1_GT1, ACT_P1_INC;
        STATE_11 => ACT_P1_INC;
        STATE_12 => @;
        STATE_20 => @, ACT_P1_GT1, ACT_P1_INC;
        STATE_21 => @, ACT_P1_INC;
        STATE_22 => @;
    );
}

#[test]
fn strat_syn_enforce_eventually_undecided() {
    strat_synthesis_test!(
        GAME,
        "<<p1>> F false",
        UNDECIDED,
        NoStrategyExist
    );
}

#[test]
fn strat_syn_enforce_until_true() {
    strat_synthesis_test!(
        GAME,
        "<<p1>> (!p2_ahead U p1.at_2)",
        TRUE,
        Strategy:
        STATE_00 => ACT_P1_GT1, ACT_P1_INC;
        STATE_01 => ACT_P1_INC;
        STATE_02 => @;
        STATE_10 => @, ACT_P1_GT1, ACT_P1_INC;
        STATE_11 => ACT_P1_INC;
        STATE_12 => @;
        STATE_20 => @, ACT_P1_GT1, ACT_P1_INC;
        STATE_21 => @, ACT_P1_INC;
        STATE_22 => @;
    );
}

#[test]
fn strat_syn_enforce_until_undecided() {
    strat_synthesis_test!(
        GAME,
        "<<p1>> (p1.at_0 U p1.at_2)",
        UNDECIDED,
        NoStrategyExist
    );
}

#[test]
fn strat_syn_enforce_invariant_true() {
    strat_synthesis_test!(
        GAME,
        "<<p1>> G p1.at_0",
        TRUE,
        Strategy:
        STATE_00 => ACT_P1_WAIT;
        STATE_01 => @;
        STATE_02 => @;
        STATE_10 => ACT_P1_WAIT;
        STATE_11 => @;
        STATE_12 => @;
        STATE_20 => ACT_P1_WAIT;
        STATE_21 => @;
        STATE_22 => @;
    );
}

#[test]
fn strat_syn_enforce_invariant_false() {
    strat_synthesis_test!(
        GAME,
        "<<p1>> G p2.at_0",
        FALSE,
        NoStrategyExist
    );
}

#[test]
fn strat_syn_despite_next_true() {
    strat_synthesis_test!(
        GAME,
        "[[p1]] X p2.at_1",
        TRUE,
        NoStrategyExist
    );
}

#[ignore] // TODO
#[test]
fn strat_syn_despite_next_false() {
    strat_synthesis_test!(
        GAME,
        "[[p1]] X p1.at_1",
        FALSE,
        Strategy:
        STATE_00 => ACT_P1_WAIT;
        STATE_01 => @;
        STATE_02 => @;
        STATE_10 => @;
        STATE_11 => @;
        STATE_12 => @;
        STATE_20 => @;
        STATE_21 => @;
        STATE_22 => @;
    );
}

#[test]
fn strat_syn_despite_eventually_true() {
    strat_synthesis_test!(
        GAME,
        "[[p1]] F p2.at_2",
        TRUE,
        NoStrategyExist
    );
}

#[test]
fn strat_syn_despite_eventually_undecided() {
    strat_synthesis_test!(
        GAME,
        "[[p1]] F p2_ahead",
        UNDECIDED,
        Strategy:
        STATE_00 => ACT_P1_GT1, ACT_P1_INC;
        STATE_01 => ACT_P1_WAIT, ACT_P1_GT1, ACT_P1_INC;
        STATE_02 => ACT_P1_WAIT, ACT_P1_GT1, ACT_P1_INC;
        STATE_10 => @;
        STATE_11 => ACT_P1_INC;
        STATE_12 => ACT_P1_WAIT, ACT_P1_INC;
        STATE_20 => @;
        STATE_21 => @;
        STATE_22 => ACT_P1_WAIT, ACT_P1_INC;
    );
}

#[test]
fn strat_syn_despite_until_true() {
    strat_synthesis_test!(
        GAME,
        "[[p1]] (p2.at_0 U p2.at_1)",
        TRUE,
        NoStrategyExist
    );
}

#[ignore]
#[test]
fn strat_syn_despite_until_false() {
    strat_synthesis_test!(
        GAME,
        "[[p1]] (!p1.at_2 U false)",
        FALSE,
        Strategy:
        STATE_00 => ACT_P1_GT1, ACT_P1_INC;
        STATE_01 => ACT_P1_INC;
        STATE_02 => @;
        STATE_10 => @, ACT_P1_GT1, ACT_P1_INC;
        STATE_11 => ACT_P1_INC;
        STATE_12 => @;
        STATE_20 => @, ACT_P1_GT1, ACT_P1_INC;
        STATE_21 => @, ACT_P1_INC; // FIXME: ALgorithm thinks WAIT is ok
        STATE_22 => @;
    );
}

#[test]
fn strat_syn_despite_until_undecided() {
    strat_synthesis_test!(
        GAME,
        "[[p1]] (true U p2_ahead)",
        UNDECIDED,
        Strategy:
        STATE_00 => ACT_P1_GT1, ACT_P1_INC;
        STATE_01 => ACT_P1_WAIT, ACT_P1_GT1, ACT_P1_INC;
        STATE_02 => ACT_P1_WAIT, ACT_P1_GT1, ACT_P1_INC;
        STATE_10 => @;
        STATE_11 => ACT_P1_INC;
        STATE_12 => ACT_P1_WAIT, ACT_P1_INC;
        STATE_20 => @;
        STATE_21 => @;
        STATE_22 => ACT_P1_WAIT, ACT_P1_INC;
    );
}

#[test]
fn strat_syn_despite_invariantly_false() {
    let ast = parse_lcgs(GAME).unwrap();
    let game = IntermediateLcgs::create(ast).unwrap();
    let phi = parse_phi(&game, FORMULA_DESPITE_INVARIANTLY_FALSE).unwrap();
    let v0 = AtlVertex::Full {
        state: game.initial_state_index(),
        formula: phi.into(),
    };
    let edg = AtlDependencyGraph {
        game_structure: game,
    };
    let czr = distributed_certain_zero(
        edg.clone(),
        v0.clone(),
        WORKER_COUNT,
        BreadthFirstSearchBuilder,
        true,
        true,
    );
    let CertainZeroResult::AllFoundAssignments(ass) = czr else { unreachable!() };

    assert!(ass.get(&v0).unwrap().is_false());

    let strat = compute_game_strategy(&edg, &v0, &ass).unwrap();

    assert!(matches!(&strat, WitnessStrategy::Strategy(_)));

    let WitnessStrategy::Strategy(PartialStrategy {
        players,
        move_to_pick,
    }) = strat else { unreachable!() };

    assert_eq!(players.as_slice(), [0]);
    assert_partial_strat_moves!(
        move_to_pick;
        STATE_00 => ACT_P1_GT1, ACT_P1_INC;
        STATE_01 => ACT_P1_INC;
        STATE_02 => @;
        STATE_10 => @, ACT_P1_GT1, ACT_P1_INC;
        STATE_11 => @, ACT_P1_INC;
        STATE_12 => @;
        STATE_20 => @, ACT_P1_GT1, ACT_P1_INC;
        STATE_21 => @, ACT_P1_INC;
        STATE_22 => @;
    );
}
