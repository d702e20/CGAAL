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

//fixme: "not found in this scope" something with criterion requires referenced functions to be public
fn bench_json_cgs(model: &str, formula: &str) {
    model_check_lazy_cgs_bench(model, formula);
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
