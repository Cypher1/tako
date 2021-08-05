use log::*;
use std::collections::HashMap;
use std::fs::read_to_string;

// use pretty_assertions::assert_eq;
use takolib::cli_options::Options;
use takolib::database::DBStorage;
use takolib::errors::TError;

#[derive(Debug, PartialEq)]
pub enum TestResult {
    Error,
    Success,            // With an unspecified value
    Output(String),     // With an expected value
    OutputFile(String), // With an expected value in a file
    ReturnValue(i32),
}
use TestResult::*;

#[derive(Debug, PartialEq)]
pub struct TestOptions {
    expected: TestResult,
    options: Options,
}

type Test = Result<(), TError>;

fn test_expecting(expected: TestResult, options: Vec<&str>) -> Test {
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
            Ok(())
        }
        (Ok(result), ReturnValue(value)) => {
            assert_eq!(result, format!("{}", value));
            info!("Success. Result:\n{:?}", result);
            Ok(())
        }
        (Ok(result), Output(s)) => {
            assert_eq!(s, format!("{}{}", stdout.join(""), result));
            info!("Success. Result:\n{:?}", result);
            Ok(())
        }
        (Ok(result), OutputFile(gold)) => {
            info!("Loading golden file {}", gold);
            let read = read_to_string(&gold);
            let golden = read
                .unwrap_or_else(|_| panic!("golden file {} could not be read", gold))
                .replace("\r", "");
            assert_eq!(format!("{}{}", stdout.join(""), result), golden);
            info!("Success. Result:\n{:?}", result);
            Ok(())
        }
        (Err(err), Error) => {
            info!("Received error:\n{:?}", err);
            Ok(())
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

fn compile(file: &str) -> Test {
    test_expecting(Success, vec![file])
}

fn compile_matching_golden(golden: &str, file: &str) -> Test {
    test_expecting(OutputFile(golden.to_string()), vec![file])
}

fn run(file: &str) -> Test {
    test_expecting(Success, vec!["--run", file])
}

fn run_with_error(file: &str) -> Test {
    test_expecting(Error, vec!["--run", file])
}

#[test]
fn one_plus_2() -> Test {
    run("examples/1_plus_2.tk")
}

#[test]
fn plus_123() -> Test {
    test_expecting(
        Output("6".to_string()),
        vec!["--run", "examples/plus_123.tk"],
    )
}

#[test]
fn alt() -> Test {
    run("examples/alt.tk")
}

#[test]
fn arguments() -> Test {
    run("examples/arguments.tk")
}

//#[test] // Re-enable when type checking works
fn assignment_returns_unit() -> Test {
    run_with_error("counter_examples/assignment_returns.tk")
}

#[test]
fn bare_words() -> Test {
    run_with_error("counter_examples/bare_words.tk")
}

#[test]
fn bool_requirement() -> Test {
    run("examples/bool_requirement.tk")
}

#[test]
fn bool_times_bool() -> Test {
    run_with_error("counter_examples/bool_times_bool.tk")
}

#[test]
fn code_reuse() -> Test {
    run("examples/code_reuse.tk")
}

#[test]
fn comment() -> Test {
    run("examples/comment.tk")
}

#[test]
fn compile_1_plus_2() -> Test {
    compile_matching_golden("tests/goldens/examples_1_plus_2.cc", "examples/1_plus_2.tk")
}

#[test]
fn compile_arguments() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_arguments.cc",
        "examples/arguments.tk",
    )
}

#[test]
fn compile_code_reuse() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_code_reuse.cc",
        "examples/code_reuse.tk",
    )
}

#[test]
fn compile_comment() -> Test {
    compile_matching_golden("tests/goldens/examples_comment.cc", "examples/comment.tk")
}

#[test]
fn compile_div() -> Test {
    compile_matching_golden("tests/goldens/examples_div.cc", "examples/div.tk")
}

#[test]
fn compile_empty_def_args() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_empty_def_args.cc",
        "examples/empty_def_args.tk",
    )
}

#[test]
fn compile_hello_name() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_hello_name.cc",
        "examples/hello_name.tk",
    )
}

#[test]
fn compile_higher_order() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_higher_order.cc",
        "examples/higher_order.tk",
    )
}

#[test]
fn compile_ignored_let() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_ignored_let.cc",
        "examples/ignored_let.tk",
    )
}

#[test]
fn compile_lambda() -> Test {
    compile_matching_golden("tests/goldens/examples_lambda.cc", "examples/lambda.tk")
}

#[test]
fn compile_multi_comment() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_multi_comment.cc",
        "examples/multi_comment.tk",
    )
}

#[test]
fn compile_multi_comment_nested() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_multi_comment_nested.cc",
        "examples/multi_comment_nested.tk",
    )
}

#[test]
fn compile_neg() -> Test {
    compile_matching_golden("tests/goldens/examples_neg.cc", "examples/neg.tk")
}

#[test]
fn compile_nested() -> Test {
    compile_matching_golden("tests/goldens/examples_nested.cc", "examples/nested.tk")
}

#[test]
fn compile_nested_as_function() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_nested_as_function.cc",
        "examples/nested_as_function.tk",
    )
}

#[test]
fn compile_nested_explicit() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_nested_explicit.cc",
        "examples/nested_explicit.tk",
    )
}

#[test]
fn compile_non_overlapping_anons() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_non_overlapping_anons.cc",
        "examples/non_overlapping_anons.tk",
    )
}

#[test]
fn compile_not() -> Test {
    compile_matching_golden("tests/goldens/examples_not.cc", "examples/not.tk")
}

#[test]
fn compile_optional_semis() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_optional_semis.cc",
        "examples/optional_semis.tk",
    )
}

#[test]
fn compile_order_of_ops() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_order_of_ops.cc",
        "examples/order_of_ops.tk",
    )
}

#[test]
fn compile_paren() -> Test {
    compile_matching_golden("tests/goldens/examples_paren.cc", "examples/paren.tk")
}

#[test]
fn compile_pow() -> Test {
    compile_matching_golden("tests/goldens/examples_pow.cc", "examples/pow.tk")
}

#[test]
fn compile_pow_twice() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_pow_twice.cc",
        "examples/pow_twice.tk",
    )
}

#[test]
fn compile_shadowing() -> Test {
    test_expecting(Error, vec!["counter_examples/shadowing.tk"])
}

#[test]
fn compile_simple() -> Test {
    compile_matching_golden("tests/goldens/examples_simple.cc", "examples/simple.tk")
}

#[test]
fn compile_simple_call() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_simple_call.cc",
        "examples/simple_call.tk",
    )
}

#[test]
fn compile_sub() -> Test {
    compile_matching_golden("tests/goldens/examples_sub.cc", "examples/sub.tk")
}

#[test]
fn compile_three_vars() -> Test {
    compile_matching_golden(
        "tests/goldens/examples_three_vars.cc",
        "examples/three_vars.tk",
    )
}

#[test]
fn compile_tmp() -> Test {
    compile_matching_golden("tests/goldens/examples_tmp.cc", "examples/tmp.tk")
}

#[test]
fn compile_x_plus_1() -> Test {
    compile_matching_golden("tests/goldens/examples_x_plus_1.cc", "examples/x_plus_1.tk")
}

#[test]
fn defaulting() -> Test {
    run("examples/defaulting.tk")
}

#[test]
fn div() -> Test {
    run("examples/div.tk")
}

#[test]
fn dupe_alt() -> Test {
    test_expecting(Error, vec!["--run", "counter_examples/dupe_alt.tk"])
}

#[test]
fn empty_args() -> Test {
    run("examples/empty_args.tk")
}

#[test]
fn empty_def_args() -> Test {
    run("examples/empty_def_args.tk")
}

#[test]
fn extension() -> Test {
    run("examples/extension.weird")
}

#[test]
fn fac() -> Test {
    run("examples/fac.tk")
}

#[test]
fn hello_name() -> Test {
    test_expecting(
        OutputFile("tests/goldens/examples_hello_name.txt".to_string()),
        vec!["--run", "examples/hello_name.tk", "--", "Peanut", "Jay"],
    )
}

#[test]
fn higher_order() -> Test {
    run("examples/higher_order.tk")
}

#[test]
fn if_statement() -> Test {
    run("examples/if.tk")
}

#[test]
fn if_using_implicit_kw() -> Test {
    run("examples/if_using_implicit_kw.tk")
}

#[test]
fn ignored_let() -> Test {
    run("examples/ignored_let.tk")
}

#[test]
fn lambda() -> Test {
    run("examples/lambda.tk")
}

#[test]
fn r#loop() -> Test {
    run("examples/loop.tk")
}

#[test]
fn missing_arguments() -> Test {
    test_expecting(
        Error,
        vec!["--run", "counter_examples/missing_arguments.tk"],
    )
}

#[test]
fn multi_comment() -> Test {
    run("examples/multi_comment.tk")
}

#[test]
fn multi_comment_nested() -> Test {
    run("examples/multi_comment_nested.tk")
}

#[test]
fn mutual_recursion() -> Test {
    run("examples/mutual_recursion.tk")
}

#[test]
fn neg() -> Test {
    run("examples/neg.tk")
}

#[test]
fn negative_bool() -> Test {
    test_expecting(Error, vec!["--run", "counter_examples/negative_bool.tk"])
}

#[test]
fn nested() -> Test {
    test_expecting(Error, vec!["--run", "counter_examples/nested.tk"])
}

#[test]
fn nested_as_function() -> Test {
    run("examples/nested_as_function.tk")
}

#[test]
fn nested_explicit() -> Test {
    run("examples/nested_explicit.tk")
}

#[test]
fn nested_explicit_side_effect() -> Test {
    test_expecting(
        Error,
        vec!["--run", "counter_examples/nested_explicit_side_effect.tk"],
    )
}

#[test]
fn non_lambda() -> Test {
    test_expecting(Error, vec!["--run", "counter_examples/non_lambda.tk"])
}

#[test]
fn non_overlapping_anons() -> Test {
    run("examples/non_overlapping_anons.tk")
}

#[test]
fn not() -> Test {
    run("examples/not.tk")
}

#[test]
fn not_int() -> Test {
    test_expecting(Error, vec!["--run", "counter_examples/not_int.tk"])
}

#[test]
fn not_string() -> Test {
    test_expecting(Error, vec!["--run", "counter_examples/not_string.tk"])
}

#[test]
fn optional_semis() -> Test {
    run("examples/optional_semis.tk")
}

#[test]
fn order_of_ops() -> Test {
    run("examples/order_of_ops.tk")
}

#[test]
fn override_print() -> Test {
    test_expecting(
        OutputFile("tests/goldens/examples_override_print.txt".to_string()),
        vec!["--run", "examples/override_print.tk", "--"],
    )
}

#[test]
fn paren() -> Test {
    run("examples/paren.tk")
}

#[test]
fn pow() -> Test {
    run("examples/pow.tk")
}

#[test]
fn pow_twice() -> Test {
    run("examples/pow_twice.tk")
}

#[test]
fn printing() -> Test {
    compile("examples/printing.tk")
}

#[test]
fn error_printing() -> Test {
    run("examples/error_printing.tk")
}

#[test]
fn compile_error_printing() -> Test {
    compile("examples/error_printing.tk")
}

#[test]
fn compile_parse_i32() -> Test {
    compile("examples/parse_i32.tk")
}

#[test]
fn requirement() -> Test {
    run_with_error("counter_examples/requirement.tk")
}

#[test]
fn shadowing() -> Test {
    run("counter_examples/shadowing.tk")
}

#[test]
fn simple() -> Test {
    run("examples/simple.tk")
}

#[test]
fn simple_call() -> Test {
    run("examples/simple_call.tk")
}

#[test]
fn sub() -> Test {
    run("examples/sub.tk")
}

#[test]
fn sym_op() -> Test {
    run_with_error("counter_examples/sym_op.tk")
}

#[test]
fn three_vars() -> Test {
    run("examples/three_vars.tk")
}

#[test]
fn tmp() -> Test {
    run("examples/tmp.tk")
}

#[test]
fn type_bool() -> Test {
    run("examples/type_bool.tk")
}

#[test]
fn type_i32() -> Test {
    run("examples/type_i32.tk")
}

#[test]
fn type_str() -> Test {
    run("examples/type_str.tk")
}

#[test]
fn x_plus_1() -> Test {
    run("examples/x_plus_1.tk")
}
