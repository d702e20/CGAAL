use atl_checker::atl::dependencygraph::{ATLDependencyGraph, ATLVertex};
use atl_checker::atl::formula::Phi;
use atl_checker::atl::gamestructure::EagerGameStructure;
use atl_checker::edg::distributed_certain_zero;
use atl_checker::lcgs::ir::intermediate::IntermediateLCGS;
use atl_checker::lcgs::parse::parse_lcgs;
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

                    distributed_certain_zero(graph, v0, num_cpus::get() as u64);
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

                            distributed_certain_zero(graph, v0, core_count);
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

// gossiping girls
bench_lcgs!(
    gossipping_girls_circular_all_ensure_p1_omniscient_before_10_steps,
    "gossipping_girls/gossipping_girls_circular.lcgs",
    "gossipping_girls/all_girls_ensure_that_player_one_gets_omicient_before_10_steps_TRUE.json"
);

// matching pennies
bench_lcgs!(
    matching_pennies_can_odd_win_round_eventually,
    "matching_pennies/matching_pennies_game.lcgs",
    "matching_pennies/can_odd_win_round_eventually_FALSE.json"
);

bench_lcgs!(
    matching_pennies_can_they_guarantee_that_odd_always_has_larger_sum,
    "matching_pennies/matching_pennies_game.lcgs",
    "matching_pennies/can_they_guarantee_that_odd_always_has_larger_sum_TRUE.json"
);

// peterson
bench_lcgs!(
    peterson_3_ensure_mutual_exclusion,
    "peterson/3/peterson-03.lcgs",
    "peterson/3/ensure-mutual-exclusion-TRUE-03.json"
);

// power control
bench_lcgs!(
    power_control_p1_enforce_eventually_p1_quality_greatest,
    "power_control_in_cellular_networks/power_control_in_cellular_networks.lcgs",
    "power_control_in_cellular_networks/p1_enforce_eventually_p1_quality_greatest_TRUE.json"
);

// public good game
bench_lcgs!(
    public_good_game_can_p1_get_capped_before_p2,
    "public_good_game/public_good_game.lcgs",
    "public_good_game/can_p1_get_capped_before_p2_FALSE.json"
);

// robot grid
bench_lcgs!(
    robot_grid_can_r1_and_r2_swap_with_help_from_r3,
    "robot_grid/robot_grid_N3.lcgs",
    "robot_grid/can_r1_and_r2_swap_with_help_from_r3_FALSE.json"
);

bench_lcgs!(
    robot_grid_exist_path_to_targets_with_no_crashes,
    "robot_grid/robot_grid_N3.lcgs",
    "robot_grid/exist_path_to_targets_with_no_crashes_TRUE.json"
);

// rock paper scissors
bench_lcgs!(
    rock_paper_scissors_p1_always_wins,
    "rock_paper_scissors/rock_paper_scissors.lcgs",
    "rock_paper_scissors/p1_always_wins_FALSE.json"
);

bench_lcgs!(
    rock_paper_scissors_p1_can_win_eventually,
    "rock_paper_scissors/rock_paper_scissors.lcgs",
    "rock_paper_scissors/p1_can_win_eventually_FALSE.json"
);

// tic tac toe
bench_lcgs!(
    tic_tac_toe_can_cross_guarantee_tie,
    "tic_tac_toe/tic_tac_toe.lcgs",
    "tic_tac_toe/can_cross_guarantee_tie_TRUE.json"
);

bench_lcgs!(
    tic_tac_toe_can_nought_avoid_lose,
    "tic_tac_toe/tic_tac_toe.lcgs",
    "tic_tac_toe/can_nought_avoid_lose_TRUE.json"
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
    "peterson/3/peterson-03.lcgs",
    "peterson/3/ensure-mutual-exclusion-TRUE-03.json"
);

// power control
bench_lcgs_threads!(
    power_control_p1_enforce_eventually_p1_quality_greatest_threads,
    "power_control_in_cellular_networks/power_control_in_cellular_networks.lcgs",
    "power_control_in_cellular_networks/p1_enforce_eventually_p1_quality_greatest_TRUE.json"
);

// public good game
bench_lcgs_threads!(
    public_good_game_can_p1_get_capped_before_p2_threads,
    "public_good_game/public_good_game.lcgs",
    "public_good_game/can_p1_get_capped_before_p2_FALSE.json"
);

// robot grid
bench_lcgs_threads!(
    robot_grid_can_r1_and_r2_swap_with_help_from_r3_threads,
    "robot_grid/robot_grid_N3.lcgs",
    "robot_grid/can_r1_and_r2_swap_with_help_from_r3_FALSE.json"
);

bench_lcgs_threads!(
    robot_grid_exist_path_to_targets_with_no_crashes_threads,
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
    //mexican_standoff_5p_1hp_lcgs_survive,
    mexican_standoff_3p_3hp_lcgs_survive,
    gossipping_girls_circular_all_ensure_p1_omniscient_before_10_steps,
    matching_pennies_can_odd_win_round_eventually,
    matching_pennies_can_they_guarantee_that_odd_always_has_larger_sum,
    peterson_3_ensure_mutual_exclusion,
    power_control_p1_enforce_eventually_p1_quality_greatest,
    //public_good_game_can_p1_get_capped_before_p2, // broken model
    //robot_grid_can_r1_and_r2_swap_with_help_from_r3, // a single run takes ~500s @ 1 thread
    //robot_grid_exist_path_to_targets_with_no_crashes, // a single run takes ~500s @ 1 thread
    rock_paper_scissors_p1_always_wins,
    rock_paper_scissors_p1_can_win_eventually,
    tic_tac_toe_can_cross_guarantee_tie,
    tic_tac_toe_can_nought_avoid_lose,
);

criterion_group!(
    multi_thread_benches,
    //mexican_standoff_5p_1hp_lcgs_survive_threads,
    mexican_standoff_3p_3hp_lcgs_survive_threads,
    gossipping_girls_circular_all_ensure_p1_omniscient_before_10_steps_threads,
    matching_pennies_can_odd_win_round_eventually_threads,
    matching_pennies_can_they_guarantee_that_odd_always_has_larger_sum_threads,
    peterson_3_ensure_mutual_exclusion_threads,
    power_control_p1_enforce_eventually_p1_quality_greatest_threads,
    //public_good_game_can_p1_get_capped_before_p2_threads, // broken model
    //robot_grid_can_r1_and_r2_swap_with_help_from_r3_threads, // a single run takes ~500s @ 1 thread
    //robot_grid_exist_path_to_targets_with_no_crashes_threads, // a single run takes ~500s @ 1 thread
    rock_paper_scissors_p1_always_wins_threads,
    rock_paper_scissors_p1_can_win_eventually_threads,
    tic_tac_toe_can_cross_guarantee_tie_threads,
    tic_tac_toe_can_nought_avoid_lose_threads,
);
criterion_main!(static_thread_benches); // choose which group to bench
