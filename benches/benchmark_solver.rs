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

/// Benchmark solver given json-model and -formula.
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

/// Reads a formula in JSON format from a file. Exits upon error.
fn load_formula(path: &str) -> Arc<Phi> {
    let mut file = File::open(path).expect(&format!("could not open formula path: {}", path));
    let mut formula = String::new();
    file.read_to_string(&mut formula)
        .expect("could not read read formula into string");
    serde_json::from_str(formula.as_str()).expect("could not parse formula json")
}

macro_rules! bench_lcgs {
    ($name:ident, $model:expr, $formula:expr) => {
        fn $name(c: &mut Criterion) {
            c.bench_function(stringify!($name), |b| {
                b.iter(|| {
                    let lcgs = parse_lcgs(include_str!(concat!("lcgs/", $model)))
                        .expect(&format!("Could not read model {}", $model));
                    let game_structure =
                        IntermediateLCGS::create(lcgs).expect("Could not symbolcheck");
                    let graph = ATLDependencyGraph { game_structure };

                    let formula = load_formula(concat!("benches/lcgs/", $formula));

                    let v0 = ATLVertex::FULL {
                        state: graph.game_structure.initial_state_index(),
                        formula,
                    };

                    let result = distributed_certain_zero(graph, v0, num_cpus::get() as u64);
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
                let core_count = core_count as u64; //todo, this should be simplified if able
                                                    //todo is criterion throughput useful here?
                group.bench_with_input(
                    BenchmarkId::from_parameter(core_count),
                    &core_count,
                    |b, &core_count| {
                        b.iter(|| {
                            let lcgs = parse_lcgs(include_str!(concat!("lcgs/", $model)))
                                .expect(&format!("Could not read model {}", $model));
                            let game_structure =
                                IntermediateLCGS::create(lcgs).expect("Could not symbolcheck");
                            let graph = ATLDependencyGraph { game_structure };

                            let formula = load_formula(concat!("benches/lcgs/", $formula));

                            let v0 = ATLVertex::FULL {
                                state: graph.game_structure.initial_state_index(),
                                formula,
                            };

                            let result = distributed_certain_zero(graph, v0, core_count);
                        });
                    },
                );
            }
        }
    };
}
bench_json!(
    mexican_standoff_json,
    "Mexican_Standoff/mexican-standoff.json",
    "Mexican_Standoff/test-formula.json"
);

bench_lcgs!(
    mexican_standoff_lcgs_alive_till_not,
    "Mexican_Standoff/Mexican_Standoff.lcgs",
    "Mexican_Standoff/Mexican_Standoff_p1_is_alive_till_he_aint.json"
);

bench_lcgs!(
    mexican_standoff_lcgs_wack,
    "Mexican_Standoff/Mexican_Standoff.lcgs",
    "Mexican_Standoff/wack.json"
);

bench_lcgs!(
    gossiping_girls_circular_10_steps_eventually,
    "Gossipping_Girls_Circular/Gossipping_Girls_Circular.lcgs",
    "Gossipping_Girls_Circular/eventually_10_steps_are_passed_TRUE.json"
);

bench_lcgs!(
    gossiping_girls_circular_10_steps_omniscient_atleast,
    "Gossipping_Girls_Circular/Gossipping_Girls_Circular.lcgs",
    "Gossipping_Girls_Circular/girl1_not_omniscient_until_atleast_10_steps_TRUE.json"
);

bench_lcgs!(
    gossiping_girls_circular_10_steps_omniscient_before_2,
    "Gossipping_Girls_Circular/Gossipping_Girls_Circular.lcgs",
    "Gossipping_Girls_Circular/p1_omniscient_before_10_steps.json" // todo, missing truth postfix
);

bench_lcgs!(
    gossiping_girls_circular_10_steps_omniscient_before,
    "Gossipping_Girls_Circular/Gossipping_Girls_Circular.lcgs",
    "Gossipping_Girls_Circular/eventually_10_steps_are_passed_TRUE.json"
);

bench_lcgs!(
    gossiping_girls_total_enforce_stupidity,
    "Gossipping_Girls_Total_Network/Gossipping_Girls_Total_Network.lcgs",
    "Gossipping_Girls_Total_Network/girl1_enforce_own_stupidity_forever_FALSE.json"
);

bench_lcgs!(
    power_control_collab,
    "Power_Control_in_Cellular_Networks/Power_Control_in_Cellular_Networks.lcgs",
    "Power_Control_in_Cellular_Networks/collab_to_make_p1_quality_higher_than_p2_with_negation_TRUE.json"
);

bench_lcgs!(
    public_good_game2_p1_always_worth_more,
    "Public_Good_Game2/Public_Good_Game2.lcgs",
    "Public_Good_Game2/Public_Good_Game_p1_always_worth_more.json"
);

bench_lcgs_threads!(
    mexican_standoff_lcgs_alive_till_not_threads,
    "Mexican_Standoff/Mexican_Standoff.lcgs",
    "Mexican_Standoff/Mexican_Standoff_p1_is_alive_till_he_aint.json"
);

criterion_group!(
    static_thread_benches,
    //mexican_standoff_json,
    mexican_standoff_lcgs_wack,
    //mexican_standoff_lcgs_alive_till_not,
    //gossiping_girls_circular_10_steps_omniscient_atleast,
    //gossiping_girls_circular_10_steps_omniscient_before,
    //gossiping_girls_circular_10_steps,
    //gossiping_girls_circular_10_steps_omniscient_before_2,
    //gossiping_girls_total_enforce_stupidity,
    //power_control_collab,
    //public_good_game2_p1_always_worth_more
);

criterion_group!(
    multi_thread_benches,
    mexican_standoff_lcgs_alive_till_not_threads
);
criterion_main!(multi_thread_benches); // choose which group to bench
