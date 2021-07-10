#![allow(unused)]
#[cfg(feature = "bench")]
use criterion::{black_box, criterion_group, criterion_main, Criterion};
#[cfg(feature = "bench")]
use takolib::cli_options::Options;
#[cfg(feature = "bench")]
use takolib::database::DBStorage;
#[cfg(feature = "bench")]
use takolib::passes::parser::parse_string;
#[cfg(feature = "bench")]
use takolib::passes::type_checker::infer;
#[cfg(feature = "bench")]
use takolib::primitives::Val::Variable;

#[cfg(feature = "bench")]
use std::sync::Arc;

#[cfg(feature = "bench")]
pub fn criterion_benchmark(c: &mut Criterion) {
    let module = vec![];

    c.bench_function("microbench_type_of_i32", |b| {
        let code = Arc::new("12".to_string());
        let mut db = DBStorage::default();
        let prog = black_box(parse_string(&mut db, &module, &code).expect("should parse"));
        let env = Variable("test_program".to_string()); // TODO: Track the type env
        b.iter(|| infer(&mut db, &prog, &env));
    });

    c.bench_function("microbench_parse_and_type_of_i32_pre_cache", |b| {
        let code = Arc::new("12".to_string());
        let mut db = DBStorage::default();
        let prog = parse_string(&mut db, &module, &code).expect("should parse");
        let env = Variable("test_program".to_string()); // TODO: Track the type env
        infer(&mut db, &prog, &env);
        b.iter(|| {
            let prog = black_box(parse_string(&mut db, &module, &code).expect("should parse"));
            infer(&mut db, &prog, &env)
        });
    });

    c.bench_function("microbench_parse_and_type_of_i32", |b| {
        let code = Arc::new("12".to_string());
        let mut db = DBStorage::default();
        let prog = black_box(parse_string(&mut db, &module, &code).expect("should parse"));
        let env = Variable("test_program".to_string()); // TODO: Track the type env
        b.iter(|| infer(&mut db, &prog, &env));
    });

    /* c.bench_function(
        "microbench_type_of_plus_expr",
        |b| {
            let mut db = DBStorage::default();
            let prog = black_box(parse_string(&mut db, "12+32".to_string()));
            b.iter(|| infer(&mut db, &prog, &env));
        }
    );*/
}

#[cfg(feature = "bench")]
criterion_group!(benches, criterion_benchmark);
#[cfg(feature = "bench")]
criterion_main!(benches);

#[cfg(not(feature = "bench"))]
fn main() {
    println!("Retry with '--features=bench' to run benchmarks");
}
