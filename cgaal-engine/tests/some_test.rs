use cgaal_engine::{algorithms::{game_strategy::{compute_game_strategy, model_check, WitnessStrategy, partial::PartialStrategy}, certain_zero::{search_strategy::bfs::BreadthFirstSearchBuilder, distributed_certain_zero, CertainZeroResult}}, game_structure::{lcgs::{ir::intermediate::IntermediateLcgs, parse::parse_lcgs}, GameStructure}, atl::parse_phi, edg::atledg::{AtlDependencyGraph, vertex::AtlVertex}};

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

const FORMULA_ENFORCE_NEXT_TRUE: &str = "<<p1>> X p1.at_1";
const FORMULA_ENFORCE_NEXT_FALSE: &str = "<<p1>> X p1.at_2";
const FORMULA_ENFORCE_UNTIL_TRUE: &str = "<<p1>> (true U p1.at_2)";
const FORMULA_ENFORCE_UNTIL_FALSE: &str = "<<p1>> (p2.at_0 U false)";
const FORMULA_ENFORCE_EVENTUALLY_TRUE: &str = "<<p1>> F p1.at_2";
const FORMULA_ENFORCE_EVENTUALLY_FALSE: &str = "<<p1>> F false";
const FORMULA_ENFORCE_INVARIANTLY_TRUE: &str = "<<p1>> G p1.at_0";
const FORMULA_ENFORCE_INVARIANTLY_FALSE: &str = "<<p1>> G p2.at_0";
const FORMULA_DESPITE_NEXT_TRUE: &str = "[[p1]] X !p2.at_2";
const FORMULA_DESPITE_NEXT_FALSE: &str = "[[p1]] X p2.at_1";
const FORMULA_DESPITE_UNTIL_TRUE: &str = "[[p1]] (true U p2.at_2)";
const FORMULA_DESPITE_UNTIL_UNDECIDED: &str = "[[p1]] (true U p2_ahead)";
const FORMULA_DESPITE_UNTIL_FALSE: &str = "[[p1]] (p1.at_2 U false)";
const FORMULA_EVENTUALLY_TRUE: &str = "[[p1]] F p2.at_2";
const FORMULA_EVENTUALLY_FALSE: &str = "[[p1]] F p1.at_2";

const WORKER_COUNT: u64 = 4;    

#[test]
fn strat_syn_enforce_eventually_true() {
    let ast = parse_lcgs(GAME).unwrap();
    let game = IntermediateLcgs::create(ast).unwrap();
    let phi = parse_phi(&game, FORMULA_ENFORCE_EVENTUALLY_TRUE).unwrap();
    let v0 = AtlVertex::Full { state: game.initial_state_index(), formula: phi.into() };
    let edg = AtlDependencyGraph { game_structure: game };
    let czr = distributed_certain_zero(edg.clone(), v0.clone(), WORKER_COUNT, BreadthFirstSearchBuilder, true, true);
    let CertainZeroResult::AllFoundAssignments(ass) = czr else { unreachable!() };

    assert!(ass.get(&v0).unwrap().is_true());

    let strat = compute_game_strategy(&edg, &v0, &ass).unwrap();
    
    assert!(matches!(&strat, WitnessStrategy::Strategy(_)));
    
    let WitnessStrategy::Strategy(PartialStrategy {
        players,
        move_to_pick,
    }) = strat else { unreachable!() };
    
    assert_eq!(players.as_slice(), [0]);
    assert!(matches!(move_to_pick.get(&STATE_00).unwrap()[0].unwrap_specific(), ACT_P1_GT1 | ACT_P1_INC));
    assert_eq!(move_to_pick.get(&STATE_01).unwrap()[0].unwrap_specific(), ACT_P1_INC);
    // TODO: Remaining. Note that some may not exist and that is okay too. Macro?
}
