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
    let mut file = File::open(path).unwrap();
    let mut formula = String::new();
    file.read_to_string(&mut formula).unwrap();
    serde_json::from_str(formula.as_str()).unwrap()
}

macro_rules! bench_lcgs {
    ($name:ident, $model:expr, $formula:expr) => {
        fn $name(c: &mut Criterion) {
            c.bench_function(stringify!($name), |b| {
                b.iter(|| {
                    let lcgs = parse_lcgs(include_str!(concat!("lcgs/", $model))).unwrap();
                    let game_structure = IntermediateLCGS::create(lcgs).unwrap();
                    let graph = ATLDependencyGraph { game_structure };

                    let formula = load_formula(concat!("lcgs/", $formula));

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
    mexican_standoff_lcgs,
    "Mexican_Standoff/Mexican_Standoff_Prism-inspired-2.lcgs",
    "Mexican_Standoff/Mexican_Standoff_p1_is_alive_till_he_aint.json"
);

criterion_group!(benches, mexican_standoff_json, mexican_standoff_lcgs);
criterion_main!(benches);
