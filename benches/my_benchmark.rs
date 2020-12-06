use atl_checker::atl::dependencygraph::{ATLDependencyGraph, ATLVertex};
use atl_checker::atl::formula::Phi;
use atl_checker::atl::gamestructure::EagerGameStructure;
use atl_checker::edg::distributed_certain_zero;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::fs::File;
use std::io::Read;
use std::sync::Arc;

fn fibonacci(n: u64) -> u64 {
    match n {
        0 => 1,
        1 => 1,
        n => fibonacci(n - 1) + fibonacci(n - 2),
    }
}

fn bench_json_cgs(model: &str, formula: &str) {
    let game_structure: EagerGameStructure =
        serde_json::from_str(include_str!("../test.json")).unwrap();
    let graph = ATLDependencyGraph { game_structure };

    let formula: Arc<Phi> = serde_json::from_str(include_str!("../test-formula.json")).unwrap();

    let v0 = ATLVertex::FULL { state: 0, formula };

    distributed_certain_zero(graph, v0, 1);
}

fn criterion_benchmark(c: &mut Criterion) {
    //c.bench_function("fib 20", |b| b.iter(|| fibonacci(black_box(20))));
    c.bench_function("mexican_standoff-test-formula", |b| {
        b.iter(|| {
            bench_json_cgs(
                black_box("mexican-standoff.json"),
                black_box("test-formula.json"),
            )
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
