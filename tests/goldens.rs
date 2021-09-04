use log::{error, info};
use pretty_assertions::assert_eq;
use std::collections::HashMap;
use std::fs::read_to_string;
use takolib::cli_options::Options;
use takolib::database::DBStorage;


#[derive(Debug, PartialEq)]
pub enum TestResult {
    Error,
    Success,            // With an unspecified value
    Output(String),     // With an expected value
    OutputFile(String), // With an expected value in a file
    ReturnValue(i32),
}
use TestResult::{Error, Output, OutputFile, ReturnValue, Success};

#[derive(Debug, PartialEq)]
pub struct TestOptions {
    expected: TestResult,
    options: Options,
}

fn test_expecting(expected: TestResult, options: Vec<&str>) {
    takolib::init_for_test();

    let mut storage = DBStorage::default();
    storage.options = Options::new(options);
    let mut stdout: Vec<String> = vec![];
    let result = {
        use takolib::externs::Res;
        use takolib::primitives::Prim::{Str, I32};
        use takolib::primitives::Val::PrimVal;
        let mut print_impl =
            &mut |_: &mut DBStorage,
                  args: HashMap<String, Box<dyn Fn() -> takolib::externs::Res>>,
                  _: takolib::ast::Info|
             -> Res {
                stdout.push(
                    match args.get("it").expect("Expected value named 'it' not found")()? {
                        PrimVal(Str(s)) => s,
                        s => format!("{:?}", s),
                    },
                );
                Ok(PrimVal(I32(0)))
            };
        let files = storage.options.files.clone();
        if files.len() != 1 {
            panic!("Tests currently only support a single file.");
        }
        takolib::work(&mut storage, &files[0], Some(&mut print_impl))
    };

    match (result, expected) {
        (Ok(result), Success) => {
            info!("Success. Result:\n{:?}", result);
        }
        (Ok(result), ReturnValue(value)) => {
            assert_eq!(result, format!("{}", value));
            info!("Success. Result:\n{:?}", result);
        }
        (Ok(result), Output(s)) => {
            assert_eq!(s, format!("{}{}", stdout.join(""), result));
            info!("Success. Result:\n{:?}", result);
        }
        (Ok(result), OutputFile(gold)) => {
            info!("Loading golden file {}", gold);
            let read = read_to_string(&gold);
            let golden = read
                .unwrap_or_else(|_| panic!("golden file {} could not be read", gold))
                .replace("\r", "");
            assert_eq!(format!("{}{}", stdout.join(""), result), golden);
            info!("Success. Result:\n{:?}", result);
        }
        (Err(err), Error) => {
            info!("Received error:\n{:?}", err);
        }
        (Ok(result), Error) => {
            info!("---Got result---\n{:?}", result);
            panic!("Expected error");
        }
        (Err(err), expectation) => {
            error!("---Expected---\n{:?}", expectation);
            panic!("Error: {}", err);
        }
    }
}

fn compile(file: &str) {
    test_expecting(Success, vec![file]);
}

fn compile_matching_golden(golden: &str, file: &str) {
    test_expecting(OutputFile(golden.to_string()), vec![file]);
}

fn run(file: &str) {
    test_expecting(Success, vec!["--run", file]);
}

fn run_with_error(file: &str) {
    test_expecting(Error, vec!["--run", file]);
}

#[test]
fn one_plus_2() {
    run("examples/1_plus_2.tk");
}

#[test]
fn plus_123() {
    test_expecting(
        Output("6".to_string()),
        vec!["--run", "examples/plus_123.tk"],
    );
}

#[test]
fn alt() {
    run("examples/alt.tk");
}

#[test]
fn arguments() {
    run("examples/arguments.tk");
}

//#[test] // Re-enable when type checking works
fn assignment_returns_unit() {
    run_with_error("counter_examples/assignment_returns.tk");
}

#[test]
fn bare_words() {
    run_with_error("counter_examples/bare_words.tk");
}

#[test]
fn bool_requirement() {
    run("examples/bool_requirement.tk");
}

#[test]
fn bool_times_bool() {
    run_with_error("counter_examples/bool_times_bool.tk");
}

#[test]
fn code_reuse() {
    run("examples/code_reuse.tk");
}

#[test]
fn comment() {
    run("examples/comment.tk");
}

#[test]
fn compile_1_plus_2() {
    compile_matching_golden("tests/goldens/examples_1_plus_2.cc", "examples/1_plus_2.tk");
}

#[test]
fn compile_arguments() {
    compile_matching_golden(
        "tests/goldens/examples_arguments.cc",
        "examples/arguments.tk",
    );
}

#[test]
fn compile_code_reuse() {
    compile_matching_golden(
        "tests/goldens/examples_code_reuse.cc",
        "examples/code_reuse.tk",
    );
}

#[test]
fn compile_comment() {
    compile_matching_golden("tests/goldens/examples_comment.cc", "examples/comment.tk");
}

#[test]
fn compile_div() {
    compile_matching_golden("tests/goldens/examples_div.cc", "examples/div.tk");
}

#[test]
fn compile_empty_def_args() {
    compile_matching_golden(
        "tests/goldens/examples_empty_def_args.cc",
        "examples/empty_def_args.tk",
    );
}

#[test]
fn compile_hello_name() {
    compile_matching_golden(
        "tests/goldens/examples_hello_name.cc",
        "examples/hello_name.tk",
    );
}

#[test]
fn compile_higher_order() {
    compile_matching_golden(
        "tests/goldens/examples_higher_order.cc",
        "examples/higher_order.tk",
    );
}

#[test]
fn compile_ignored_let() {
    compile_matching_golden(
        "tests/goldens/examples_ignored_let.cc",
        "examples/ignored_let.tk",
    );
}

#[test]
fn compile_lambda() {
    compile_matching_golden("tests/goldens/examples_lambda.cc", "examples/lambda.tk");
}

#[test]
fn compile_multi_comment() {
    compile_matching_golden(
        "tests/goldens/examples_multi_comment.cc",
        "examples/multi_comment.tk",
    );
}

#[test]
fn compile_multi_comment_nested() {
    compile_matching_golden(
        "tests/goldens/examples_multi_comment_nested.cc",
        "examples/multi_comment_nested.tk",
    );
}

#[test]
fn compile_neg() {
    compile_matching_golden("tests/goldens/examples_neg.cc", "examples/neg.tk");
}

#[test]
fn compile_nested() {
    compile_matching_golden("tests/goldens/examples_nested.cc", "examples/nested.tk");
}

#[test]
fn compile_nested_as_function() {
    compile_matching_golden(
        "tests/goldens/examples_nested_as_function.cc",
        "examples/nested_as_function.tk",
    );
}

#[test]
fn compile_nested_explicit() {
    compile_matching_golden(
        "tests/goldens/examples_nested_explicit.cc",
        "examples/nested_explicit.tk",
    );
}

#[test]
fn compile_non_overlapping_anons() {
    compile_matching_golden(
        "tests/goldens/examples_non_overlapping_anons.cc",
        "examples/non_overlapping_anons.tk",
    );
}

#[test]
fn compile_not() {
    compile_matching_golden("tests/goldens/examples_not.cc", "examples/not.tk");
}

#[test]
fn compile_optional_semis() {
    compile_matching_golden(
        "tests/goldens/examples_optional_semis.cc",
        "examples/optional_semis.tk",
    );
}

#[test]
fn compile_order_of_ops() {
    compile_matching_golden(
        "tests/goldens/examples_order_of_ops.cc",
        "examples/order_of_ops.tk",
    );
}

#[test]
fn compile_paren() {
    compile_matching_golden("tests/goldens/examples_paren.cc", "examples/paren.tk");
}

#[test]
fn compile_pow() {
    compile_matching_golden("tests/goldens/examples_pow.cc", "examples/pow.tk");
}

#[test]
fn compile_pow_twice() {
    compile_matching_golden(
        "tests/goldens/examples_pow_twice.cc",
        "examples/pow_twice.tk",
    );
}

#[test]
fn compile_shadowing() {
    test_expecting(Error, vec!["counter_examples/shadowing.tk"]);
}

#[test]
fn compile_simple() {
    compile_matching_golden("tests/goldens/examples_simple.cc", "examples/simple.tk");
}

#[test]
fn compile_simple_call() {
    compile_matching_golden(
        "tests/goldens/examples_simple_call.cc",
        "examples/simple_call.tk",
    );
}

#[test]
fn compile_sub() {
    compile_matching_golden("tests/goldens/examples_sub.cc", "examples/sub.tk");
}

#[test]
fn compile_three_vars() {
    compile_matching_golden(
        "tests/goldens/examples_three_vars.cc",
        "examples/three_vars.tk",
    );
}

#[test]
fn compile_tmp() {
    compile_matching_golden("tests/goldens/examples_tmp.cc", "examples/tmp.tk");
}

#[test]
fn compile_x_plus_1() {
    compile_matching_golden("tests/goldens/examples_x_plus_1.cc", "examples/x_plus_1.tk");
}

#[test]
fn defaulting() {
    run("examples/defaulting.tk");
}

#[test]
fn div() {
    run("examples/div.tk");
}

#[test]
fn dupe_alt() {
    test_expecting(Error, vec!["--run", "counter_examples/dupe_alt.tk"]);
}

#[test]
fn empty_args() {
    run("examples/empty_args.tk");
}

#[test]
fn empty_def_args() {
    run("examples/empty_def_args.tk");
}

#[test]
fn extension() {
    run("examples/extension.weird");
}

#[test]
fn fac() {
    run("examples/fac.tk");
}

#[test]
fn hello_name() {
    test_expecting(
        OutputFile("tests/goldens/examples_hello_name.txt".to_string()),
        vec!["--run", "examples/hello_name.tk", "--", "Peanut", "Jay"],
    );
}

#[test]
fn higher_order() {
    run("examples/higher_order.tk");
}

#[test]
fn if_statement() {
    run("examples/if.tk");
}

#[test]
fn if_using_implicit_kw() {
    run("examples/if_using_implicit_kw.tk");
}

#[test]
fn ignored_let() {
    run("examples/ignored_let.tk");
}

#[test]
fn lambda() {
    run("examples/lambda.tk");
}

#[test]
fn r#loop() {
    run("examples/loop.tk");
}

#[test]
fn missing_arguments() {
    test_expecting(
        Error,
        vec!["--run", "counter_examples/missing_arguments.tk"],
    );
}

#[test]
fn multi_comment() {
    run("examples/multi_comment.tk");
}

#[test]
fn multi_comment_nested() {
    run("examples/multi_comment_nested.tk");
}

#[test]
fn mutual_recursion() {
    run("examples/mutual_recursion.tk");
}

#[test]
fn neg() {
    run("examples/neg.tk");
}

#[test]
fn negative_bool() {
    test_expecting(Error, vec!["--run", "counter_examples/negative_bool.tk"]);
}

#[test]
fn nested() {
    test_expecting(Error, vec!["--run", "counter_examples/nested.tk"]);
}

#[test]
fn nested_as_function() {
    run("examples/nested_as_function.tk");
}

#[test]
fn nested_explicit() {
    run("examples/nested_explicit.tk");
}

#[test]
fn nested_explicit_side_effect() {
    test_expecting(
        Error,
        vec!["--run", "counter_examples/nested_explicit_side_effect.tk"],
    );
}

#[test]
fn non_lambda() {
    test_expecting(Error, vec!["--run", "counter_examples/non_lambda.tk"]);
}

#[test]
fn non_overlapping_anons() {
    run("examples/non_overlapping_anons.tk");
}

#[test]
fn not() {
    run("examples/not.tk");
}

#[test]
fn not_int() {
    test_expecting(Error, vec!["--run", "counter_examples/not_int.tk"]);
}

#[test]
fn not_string() {
    test_expecting(Error, vec!["--run", "counter_examples/not_string.tk"]);
}

#[test]
fn optional_semis() {
    run("examples/optional_semis.tk");
}

#[test]
fn order_of_ops() {
    run("examples/order_of_ops.tk");
}

#[test]
fn override_print() {
    test_expecting(
        OutputFile("tests/goldens/examples_override_print.txt".to_string()),
        vec!["--run", "examples/override_print.tk", "--"],
    );
}

#[test]
fn paren() {
    run("examples/paren.tk");
}

#[test]
fn pow() {
    run("examples/pow.tk");
}

#[test]
fn pow_twice() {
    run("examples/pow_twice.tk");
}

#[test]
fn compile_printing() {
    compile("examples/printing.tk");
}

#[test]
fn error_printing() {
    run("examples/error_printing.tk");
}

#[test]
fn compile_error_printing() {
    compile("examples/error_printing.tk");
}

#[test]
fn compile_parse_i32() {
    compile("examples/parse_i32.tk");
}

#[test]
fn requirement() {
    run_with_error("counter_examples/requirement.tk");
}

#[test]
fn shadowing() {
    run("counter_examples/shadowing.tk");
}

#[test]
fn simple() {
    run("examples/simple.tk");
}

#[test]
fn simple_call() {
    run("examples/simple_call.tk");
}

#[test]
fn sub() {
    run("examples/sub.tk");
}

#[test]
fn sym_op() {
    run_with_error("counter_examples/sym_op.tk");
}

#[test]
fn three_vars() {
    run("examples/three_vars.tk");
}

#[test]
fn tmp() {
    run("examples/tmp.tk");
}

#[test]
fn type_bool() {
    run("examples/type_bool.tk");
}

#[test]
fn type_i32() {
    run("examples/type_i32.tk");
}

#[test]
fn type_str() {
    run("examples/type_str.tk");
}

#[test]
fn x_plus_1() {
    run("examples/x_plus_1.tk");
}
