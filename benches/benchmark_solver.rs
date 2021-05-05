use atl_checker::algorithms::certain_zero::distributed_certain_zero;
use atl_checker::algorithms::certain_zero::search_strategy::bfs::BreadthFirstSearchBuilder;
use atl_checker::atl::Phi;
use atl_checker::edg::{ATLDependencyGraph, ATLVertex};
use atl_checker::game_structure::lcgs::ir::intermediate::IntermediateLCGS;
use atl_checker::game_structure::lcgs::parse::parse_lcgs;
use atl_checker::game_structure::EagerGameStructure;
use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use std::fs::File;
use std::io::Read;
use std::sync::Arc;
// CWD is atl-checker, use relative paths - implemented as macro, since concat! only works for tokens
// workaround src: https://github.com/rust-lang/rust/issues/31383
macro_rules! model_path_prefix {
    () => {
        "../lcgs-examples/"
    };
}

/// Benchmark solver given json-model and -formula. TODO; deprecated but retained for future use
macro_rules! bench_json {
    ($name:ident, $model:expr, $formula:expr) => {
        fn $name(c: &mut Criterion) {
            c.bench_function(stringify!($name), |b| {
                b.iter(|| {
                    let game_structure: EagerGameStructure =
                        serde_json::from_str(include_str!(concat!("json/", $model))).unwrap();
                    let graph = ATLDependencyGraph { game_structure };

                    let formula: Arc<Phi> =
                        serde_json::from_str(include_str!(concat!("json/", $formula))).unwrap();

                    let v0 = ATLVertex::FULL { state: 0, formula };

                    distributed_certain_zero(graph, v0, num_cpus::get() as u64);
                })
            });
        }
    };
}

macro_rules! bench_lcgs {
    ($name:ident, $model:expr, $formula:expr) => {
        fn $name(c: &mut Criterion) {
            c.bench_function(stringify!($name), |b| {
                b.iter(|| {
                    let lcgs = parse_lcgs(include_str!(concat!(model_path_prefix!(), $model)))
                        .expect(&format!("Could not read model {}", $model));
                    let game_structure =
                        IntermediateLCGS::create(lcgs).expect("Could not symbolcheck");
                    let graph = ATLDependencyGraph { game_structure };

                    let formula =
                        serde_json::from_str(include_str!(concat!(model_path_prefix!(), $formula)))
                            .expect(&format!("Could not read formula {}", $formula));

                    let v0 = ATLVertex::FULL {
                        state: graph.game_structure.initial_state_index(),
                        formula,
                    };

                    distributed_certain_zero(
                        graph,
                        v0,
                        num_cpus::get() as u64,
                        BreadthFirstSearchBuilder,
                    );
                });
            });
        }
    };
}

macro_rules! bench_lcgs_threads {
    ($name:ident, $model:expr, $formula:expr) => {
        fn $name(c: &mut Criterion) {
            let mut group = c.benchmark_group(stringify!($name));

            for core_count in 1..num_cpus::get() + 1 {
                let core_count = core_count as u64; //todo, 1. this should be simplified if able
                                                    //todo, 2. is criterion throughput useful here?
                group.bench_with_input(
                    BenchmarkId::from_parameter(core_count),
                    &core_count,
                    |b, &core_count| {
                        b.iter(|| {
                            let lcgs =
                                parse_lcgs(include_str!(concat!(model_path_prefix!(), $model)))
                                    .expect(&format!("Could not read model {}", $model));
                            let game_structure =
                                IntermediateLCGS::create(lcgs).expect("Could not symbolcheck");
                            let graph = ATLDependencyGraph { game_structure };

                            let formula = serde_json::from_str(include_str!(concat!(
                                model_path_prefix!(),
                                $formula
                            )))
                            .expect(&format!("Could not read formula {}", $formula));

                            let v0 = ATLVertex::FULL {
                                state: graph.game_structure.initial_state_index(),
                                formula,
                            };

                            distributed_certain_zero(
                                graph,
                                v0,
                                core_count,
                                BreadthFirstSearchBuilder,
                            );
                        });
                    },
                );
            }
        }
    };
}

// define benchmark cases, adhere to naming scheme of "$model_$formula_(single/threads)"

// STATIC THREAD COUNT
bench_lcgs!(
    mexican_standoff_3p_3hp_lcgs_survive,
    "mexican_standoff/mexican_standoff_3p_3hp.lcgs",
    "mexican_standoff/can_p1_guarantee_to_survive_FALSE.json"
);

bench_lcgs!(
    mexican_standoff_5p_1hp_lcgs_survive,
    "mexican_standoff/mexican_standoff_5p_1hp.lcgs",
    "mexican_standoff/can_p1_guarantee_to_survive_FALSE.json"
);

// tic tac toe
bench_lcgs!(
    ttt1,
    "tic_tac_toe/tic_tac_toe.lcgs",
    "tic_tac_toe/can_cross_avoid_lose_TRUE.json"
);

bench_lcgs!(
    ttt2,
    "tic_tac_toe/tic_tac_toe.lcgs",
    "tic_tac_toe/can_cross_guarantee_tie_TRUE.json"
);

bench_lcgs!(
    ttt3,
    "tic_tac_toe/tic_tac_toe.lcgs",
    "tic_tac_toe/can_cross_guarantee_win_FALSE.json"
);

bench_lcgs!(
    ttt4,
    "tic_tac_toe/tic_tac_toe.lcgs",
    "tic_tac_toe/can_nought_avoid_lose_TRUE.json"
);

bench_lcgs!(
    ttt5,
    "tic_tac_toe/tic_tac_toe.lcgs",
    "tic_tac_toe/can_nought_guarantee_win_FALSE.json"
);

// robot grid
bench_lcgs!(
    rc1,
    "robot_grid/robot_grid_N3.lcgs",
    "robot_grid/can_r1_and_r2_swap_with_help_from_r3_FALSE.json"
);

bench_lcgs!(
    rc2,
    "robot_grid/robot_grid_N3.lcgs",
    "robot_grid/everyone_starts_home_TRUE.json"
);

bench_lcgs!(
    rc3,
    "robot_grid/robot_grid_N3.lcgs",
    "robot_grid/exist_path_to_targets_with_no_crashes_TRUE.json"
);

// peterson
bench_lcgs!(
    pa1_3proc,
    "peterson/3/peterson_03.lcgs",
    "peterson/p0_eventually_get_access_if_requested_TRUE.json"
);

bench_lcgs!(
    pa2_3proc,
    "peterson/3/peterson_03.lcgs",
    "peterson/p0_eventually_reach_CS_TRUE.json"
);

bench_lcgs!(
    pa3_3proc,
    "peterson/3/peterson_03.lcgs",
    "peterson/3/ensure_mutual_exclusion_TRUE_03.json"
);

bench_lcgs!(
    pa4_3proc,
    "peterson/3/peterson_03.lcgs",
    "peterson/3/multiple_in_CS_FALSE_03.json"
);

bench_lcgs!(
    pa5_3proc,
    "peterson/3/peterson_03.lcgs",
    "peterson/3/never_reach_deadlock_TRUE_03.json"
);

bench_lcgs!(
    pa1_4proc,
    "peterson/4/peterson_04.lcgs",
    "peterson/p0_eventually_get_access_if_requested_TRUE.json"
);

bench_lcgs!(
    pa2_4proc,
    "peterson/4/peterson_04.lcgs",
    "peterson/p0_eventually_reach_CS_TRUE.json"
);

bench_lcgs!(
    pa3_4proc,
    "peterson/4/peterson_04.lcgs",
    "peterson/4/ensure_mutual_exclusion_TRUE_04.json"
);

bench_lcgs!(
    pa4_4proc,
    "peterson/4/peterson_04.lcgs",
    "peterson/4/multiple_in_CS_FALSE_04.json"
);

bench_lcgs!(
    pa5_4proc,
    "peterson/4/peterson_04.lcgs",
    "peterson/4/never_reach_deadlock_TRUE_04.json"
);

bench_lcgs!(
    //takes ~15s for one run
    pa1_9proc,
    "peterson/9/peterson_09.lcgs",
    "peterson/p0_eventually_get_access_if_requested_TRUE.json"
);

// gossiping girls
bench_lcgs!(
    gg1_circular,
    "gossipping_girls/gossipping_girls_circular.lcgs",
    "gossipping_girls/all_girls_ensure_that_all_girls_gets_omicient_before_10_steps_TRUE.json"
);

bench_lcgs!(
    gg2_circular,
    "gossipping_girls/gossipping_girls_circular.lcgs",
    "gossipping_girls/all_girls_ensure_that_only_player_one_gets_omicient_before_10_steps_TRUE.json"
);

bench_lcgs!(
    gg3_circular,
    "gossipping_girls/gossipping_girls_circular.lcgs",
    "gossipping_girls/all_girls_ensure_that_player_one_gets_omicient_before_10_steps_TRUE.json"
);

bench_lcgs!(
    gg4_circular,
    "gossipping_girls/gossipping_girls_circular.lcgs",
    "gossipping_girls/all_omniscient_but_first_after_10_steps_TRUE.json"
);

bench_lcgs!(
    gg5_circular,
    "gossipping_girls/gossipping_girls_circular.lcgs",
    "gossipping_girls/eventually_10_steps_are_passed_TRUE.json"
);

bench_lcgs!(
    gg6_circular,
    "gossipping_girls/gossipping_girls_circular.lcgs",
    "gossipping_girls/girl_one_gurantee_to_become_omicient_before_10_steps_FALSE.json"
);

bench_lcgs!(
    gg7_circular,
    "gossipping_girls/gossipping_girls_circular.lcgs",
    "gossipping_girls/guarantee_all_girls_eventually_become_omniscient_but_not_girl_one_TRUE.json"
);

bench_lcgs!(
    gg1_total,
    "gossipping_girls/gossipping_girls_total_network.lcgs",
    "gossipping_girls/all_girls_ensure_that_all_girls_gets_omicient_before_10_steps_TRUE.json"
);

bench_lcgs!(
    gg2_total,
    "gossipping_girls/gossipping_girls_total_network.lcgs",
    "gossipping_girls/all_girls_ensure_that_only_player_one_gets_omicient_before_10_steps_TRUE.json"
);

bench_lcgs!(
    gg3_total,
    "gossipping_girls/gossipping_girls_total_network.lcgs",
    "gossipping_girls/all_girls_ensure_that_player_one_gets_omicient_before_10_steps_TRUE.json"
);

bench_lcgs!(
    gg4_total,
    "gossipping_girls/gossipping_girls_total_network.lcgs",
    "gossipping_girls/all_omniscient_but_first_after_10_steps_TRUE.json"
);

bench_lcgs!(
    gg5_total,
    "gossipping_girls/gossipping_girls_total_network.lcgs",
    "gossipping_girls/eventually_10_steps_are_passed_TRUE.json"
);

bench_lcgs!(
    gg6_total,
    "gossipping_girls/gossipping_girls_total_network.lcgs",
    "gossipping_girls/girl_one_gurantee_to_become_omicient_before_10_steps_FALSE.json"
);

bench_lcgs!(
    gg7_total,
    "gossipping_girls/gossipping_girls_total_network.lcgs",
    "gossipping_girls/guarantee_all_girls_eventually_become_omniscient_but_not_girl_one_TRUE.json"
);

// rock paper scissors
bench_lcgs!(
    rps1,
    "rock_paper_scissors/rock_paper_scissors.lcgs",
    "rock_paper_scissors/p1_always_wins_FALSE.json"
);

bench_lcgs!(
    rps2,
    "rock_paper_scissors/rock_paper_scissors.lcgs",
    "rock_paper_scissors/p1_can_win_eventually_FALSE.json"
);

// matching pennies
bench_lcgs!(
    mp1,
    "matching_pennies/matching_pennies_game.lcgs",
    "matching_pennies/can_odd_win_round_eventually_FALSE.json"
);

bench_lcgs!(
    mp2,
    "matching_pennies/matching_pennies_game.lcgs",
    "matching_pennies/can_they_guarantee_that_odd_always_has_larger_sum_TRUE.json"
);

bench_lcgs!(
    mp3,
    "matching_pennies/matching_pennies_game.lcgs",
    "matching_pennies/can_they_win_simultaneously_FALSE.json"
);

// MULTIPLE THREAD COUNT
// mexican
bench_lcgs_threads!(
    mexican_standoff_3p_3hp_lcgs_survive_threads,
    "mexican_standoff/mexican_standoff_3p_3hp.lcgs",
    "mexican_standoff/can_p1_guarantee_to_survive_FALSE.json"
);

bench_lcgs_threads!(
    mexican_standoff_5p_1hp_lcgs_survive_threads,
    "mexican_standoff/mexican_standoff_5p_1hp.lcgs",
    "mexican_standoff/can_p1_guarantee_to_survive_FALSE.json"
);

// gossiping girls
bench_lcgs_threads!(
    gossipping_girls_circular_all_ensure_p1_omniscient_before_10_steps_threads,
    "gossipping_girls/gossipping_girls_circular.lcgs",
    "gossipping_girls/all_girls_ensure_that_player_one_gets_omicient_before_10_steps_TRUE.json"
);

// matching pennies
bench_lcgs_threads!(
    matching_pennies_can_odd_win_round_eventually_threads,
    "matching_pennies/matching_pennies_game.lcgs",
    "matching_pennies/can_odd_win_round_eventually_FALSE.json"
);

bench_lcgs_threads!(
    matching_pennies_can_they_guarantee_that_odd_always_has_larger_sum_threads,
    "matching_pennies/matching_pennies_game.lcgs",
    "matching_pennies/can_they_guarantee_that_odd_always_has_larger_sum_TRUE.json"
);

// peterson
bench_lcgs_threads!(
    peterson_3_ensure_mutual_exclusion_threads,
    "peterson/3/peterson_03.lcgs",
    "peterson/3/ensure_mutual_exclusion_TRUE_03.json"
);

// robot grid
bench_lcgs_threads!(
    robot_grid_3x3_can_r1_and_r2_swap_with_help_from_r3_threads,
    "robot_grid/robot_grid_N3.lcgs",
    "robot_grid/can_r1_and_r2_swap_with_help_from_r3_FALSE.json"
);

bench_lcgs_threads!(
    robot_grid_3x3_exist_path_to_targets_with_no_crashes_threads,
    "robot_grid/robot_grid_N3.lcgs",
    "robot_grid/exist_path_to_targets_with_no_crashes_TRUE.json"
);

// rock paper scissors
bench_lcgs_threads!(
    rock_paper_scissors_p1_always_wins_threads,
    "rock_paper_scissors/rock_paper_scissors.lcgs",
    "rock_paper_scissors/p1_always_wins_FALSE.json"
);

bench_lcgs_threads!(
    rock_paper_scissors_p1_can_win_eventually_threads,
    "rock_paper_scissors/rock_paper_scissors.lcgs",
    "rock_paper_scissors/p1_can_win_eventually_FALSE.json"
);

// tic tac toe
bench_lcgs_threads!(
    tic_tac_toe_can_cross_guarantee_tie_threads,
    "tic_tac_toe/tic_tac_toe.lcgs",
    "tic_tac_toe/can_cross_guarantee_tie_TRUE.json"
);

bench_lcgs_threads!(
    tic_tac_toe_can_nought_avoid_lose_threads,
    "tic_tac_toe/tic_tac_toe.lcgs",
    "tic_tac_toe/can_nought_avoid_lose_TRUE.json"
);

// groups take a name as first argument, all subsequent arguments are benchmarks for this group
criterion_group!(
    static_thread_benches,
    mexican_standoff_3p_3hp_lcgs_survive,
    mexican_standoff_5p_1hp_lcgs_survive,
    ttt1,
    ttt2,
    ttt3,
    ttt4,
    ttt5,
    //rc1,
    //rc2,
    //rc3, //rc benches takes 215s total
    pa1_3proc,
    pa2_3proc,
    pa3_3proc,
    pa4_3proc,
    pa5_3proc,
    pa1_4proc,
    pa2_4proc,
    pa3_4proc,
    pa4_4proc,
    pa5_4proc,
    gg1_circular,
    gg2_circular,
    gg3_circular,
    gg4_circular,
    gg5_circular,
    gg6_circular,
    gg7_circular,
    /*
    gg1_total,
    gg2_total,
    gg3_total,
    gg4_total,
    gg5_total,
    gg6_total,
    gg7_total, // gg_total benches takes avg 69s a piece (nice)
    */
    rps1,
    rps2,
    mp1,
    mp2,
    mp3,
);

criterion_group!(
    multi_thread_benches,
    matching_pennies_can_odd_win_round_eventually_threads,
);
criterion_main!(static_thread_benches); // choose which group to bench
