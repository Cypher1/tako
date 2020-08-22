#![allow(unused)]
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use takolib::database::DB;
use takolib::parser::parse_string_for_test;
use takolib::type_checker::infer;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function(
        "microbench_type_of_i32",
        |b| {
            let mut db = DB::default();
            let prog = black_box(parse_string_for_test(&mut db, "12".to_string()));
            b.iter(|| infer(&mut db, &prog));
        }
    );

    c.bench_function(
        "microbench_type_of_i32_raw",
        |b| {
            let mut db = DB::default();
            parse_string_for_test(&mut db, "12".to_string());
            b.iter(|| {
                let prog = black_box(parse_string_for_test(&mut db, "12".to_string()));
                infer(&mut db, &prog)
            });
        }
    );

    /* c.bench_function(
        "microbench_type_of_plus_expr",
        |b| {
            let mut db = DB::default();
            let prog = black_box(parse_string_for_test(&mut db, "12+32".to_string()));
            b.iter(|| infer(&mut db, &prog));
        }
    );*/
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

