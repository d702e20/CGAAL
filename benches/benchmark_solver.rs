use atl_checker::atl::dependencygraph::{ATLDependencyGraph, ATLVertex};
use atl_checker::atl::formula::Phi;
use atl_checker::atl::gamestructure::EagerGameStructure;
use atl_checker::edg::distributed_certain_zero;
use atl_checker::lcgs::ir::intermediate::IntermediateLCGS;
use atl_checker::lcgs::parse::parse_lcgs;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::fs::File;
use std::io::Read;
use std::sync::Arc;

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

                    distributed_certain_zero(graph, v0, 1);
                })
            });
        }
    };
}

/// Reads a formula in JSON format from a file.
/// This function will exit the program if it encounters an error.
fn load_formula(path: &str) -> Arc<Phi> {
    let mut file = File::open(path).expect(&format!("path: {}", path));
    let mut formula = String::new();
    file.read_to_string(&mut formula).expect("hit2");
    serde_json::from_str(formula.as_str()).expect("hit3")
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

                    distributed_certain_zero(graph, v0, num_cpus::get() as u64);
                })
            });
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
    gossiping_girls_circular_omniscient,
    "Gossipping_Girls_Circular/Gossipping_Girls_Circular.lcgs",
    "Gossipping_Girls_Circular/girl1_not_omniscient_until_atleast_10_steps_TRUE.json"
);

bench_lcgs!(
    gossiping_girls_circular_10_steps,
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

criterion_group!(
    benches,
    //mexican_standoff_json,
    //mexican_standoff_lcgs_wack,
    //mexican_standoff_lcgs_alive_till_not,
    //gossiping_girls_circular_omniscient,
    //gossiping_girls_circular_10_steps,
    //gossiping_girls_total_enforce_stupidity,
    power_control_collab
);
criterion_main!(benches);
