use std::collections::HashMap;
use std::fs::read_to_string;

use pretty_assertions::assert_eq;
use takolib::cli_options::Options;
use takolib::database::{Compiler, DB};

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

fn test_with_expectation(expected: TestResult, options: Vec<&str>) {
    let options = Options::new(options);
    let mut db = DB::default();
    db.set_options(options);

    let mut stdout: Vec<String> = vec![];
    let result = {
        use takolib::ast::{
            Info,
            Prim::{Str, I32},
        };
        use takolib::interpreter::Res;
        let mut print_impl =
            &mut |_: &dyn Compiler,
                  args: HashMap<String, Box<dyn Fn() -> takolib::interpreter::Res>>,
                  _: takolib::ast::Info|
             -> Res {
                stdout.push(match args.get("it").unwrap()()? {
                    Str(s, _) => s,
                    s => format!("{:?}", s),
                });
                Ok(I32(0, Info::default()))
            };
        let files = db.files();
        if files.len() != 1 {
            panic!("Tests currently only support a single file.");
        }
        takolib::work(&mut db, &files[0], Some(&mut print_impl))
    };

    match (result, expected) {
        (Ok(result), Success) => {
            eprintln!("Success. Result:\n{:?}", result);
        }
        (Ok(result), ReturnValue(value)) => {
            assert_eq!(result, format!("{}", value));
            eprintln!("Success. Result:\n{:?}", result);
        }
        (Ok(result), Output(s)) => {
            assert_eq!(s, format!("{}{}", stdout.join(""), result));
            eprintln!("Success. Result:\n{:?}", result);
        }
        (Ok(result), OutputFile(gold)) => {
            eprintln!("Loading golden file {}", gold);
            let read = read_to_string(&gold);
            let golden = read
                .unwrap_or_else(|_| panic!("golden file {} could not be read", gold))
                .replace("\r", "");
            assert_eq!(golden, format!("{}{}", stdout.join(""), result));
            eprintln!("Success. Result:\n{:?}", result);
        }
        (Err(err), Error) => {
            eprintln!("Received error:\n{:?}", err);
        }
        (Ok(result), Error) => {
            eprintln!("---Got result---\n{:?}", result);
            panic!("Expected error");
        }
        (Err(err), expectation) => {
            eprintln!("---Got error---\n{}", err);
            eprintln!("---Expected---\n{:?}", expectation);
            panic!(format!("Error: {:?}", err));
        }
    }
}

fn compile_with_success(file: &str) {
    test_with_expectation(Success, vec![file]);
}

fn interpret_with_success(file: &str) {
    test_with_expectation(Success, vec!["--run", file]);
}

fn interpret_with_error(file: &str) {
    test_with_expectation(Error, vec!["--run", file]);
}

#[test]
fn one_plus_2() {
    interpret_with_success("examples/1_plus_2.tk");
}

#[test]
fn plus_123() {
    test_with_expectation(
        Output("6".to_string()),
        vec!["--run", "examples/plus_123.tk"],
    );
}

#[test]
fn alt() {
    interpret_with_success("examples/alt.tk")
}

#[test]
fn arguments() {
    interpret_with_success("examples/arguments.tk")
}

//#[test] // Re-enable when type checking works
fn assignment_returns_unit() {
    interpret_with_error("counter_examples/assignment_returns.tk");
}

#[test]
fn bare_words() {
    interpret_with_error("counter_examples/bare_words.tk");
}

#[test]
fn bool_requirement() {
    interpret_with_success("examples/bool_requirement.tk")
}

#[test]
fn bool_times_bool() {
    interpret_with_error("counter_examples/bool_times_bool.tk");
}

#[test]
fn code_reuse() {
    interpret_with_success("examples/code_reuse.tk")
}

#[test]
fn comment() {
    interpret_with_success("examples/comment.tk")
}

#[test]
fn compile_1_plus_2() {
    test_with_expectation(
        OutputFile("goldens/examples_1_plus_2.cc".to_string()),
        vec!["examples/1_plus_2.tk"],
    );
}

#[test]
fn compile_arguments() {
    test_with_expectation(
        OutputFile("goldens/examples_arguments.cc".to_string()),
        vec!["examples/arguments.tk"],
    );
}

#[test]
fn compile_code_reuse() {
    test_with_expectation(
        OutputFile("goldens/examples_code_reuse.cc".to_string()),
        vec!["examples/code_reuse.tk"],
    );
}

#[test]
fn compile_comment() {
    test_with_expectation(
        OutputFile("goldens/examples_comment.cc".to_string()),
        vec!["examples/comment.tk"],
    );
}

#[test]
fn compile_div() {
    test_with_expectation(
        OutputFile("goldens/examples_div.cc".to_string()),
        vec!["examples/div.tk"],
    );
}

#[test]
fn compile_empty_def_args() {
    test_with_expectation(
        OutputFile("goldens/examples_empty_def_args.cc".to_string()),
        vec!["examples/empty_def_args.tk"],
    );
}

#[test]
fn compile_hello_name() {
    test_with_expectation(
        OutputFile("goldens/examples_hello_name.cc".to_string()),
        vec!["examples/hello_name.tk"],
    );
}

#[test]
fn compile_higher_order() {
    test_with_expectation(
        OutputFile("goldens/examples_higher_order.cc".to_string()),
        vec!["examples/higher_order.tk"],
    );
}

#[test]
fn compile_ignored_let() {
    test_with_expectation(
        OutputFile("goldens/examples_ignored_let.cc".to_string()),
        vec!["examples/ignored_let.tk"],
    );
}

#[test]
fn compile_lambda() {
    test_with_expectation(
        OutputFile("goldens/examples_lambda.cc".to_string()),
        vec!["examples/lambda.tk"],
    );
}

#[test]
fn compile_multi_comment() {
    test_with_expectation(
        OutputFile("goldens/examples_multi_comment.cc".to_string()),
        vec!["examples/multi_comment.tk"],
    );
}

#[test]
fn compile_multi_comment_nested() {
    test_with_expectation(
        OutputFile("goldens/examples_multi_comment_nested.cc".to_string()),
        vec!["examples/multi_comment_nested.tk"],
    );
}

#[test]
fn compile_neg() {
    test_with_expectation(
        OutputFile("goldens/examples_neg.cc".to_string()),
        vec!["examples/neg.tk"],
    );
}

#[test]
fn compile_nested() {
    test_with_expectation(
        OutputFile("goldens/examples_nested.cc".to_string()),
        vec!["examples/nested.tk"],
    );
}

#[test]
fn compile_nested_as_function() {
    test_with_expectation(
        OutputFile("goldens/examples_nested_as_function.cc".to_string()),
        vec!["examples/nested_as_function.tk"],
    );
}

#[test]
fn compile_nested_explicit() {
    test_with_expectation(
        OutputFile("goldens/examples_nested_explicit.cc".to_string()),
        vec!["examples/nested_explicit.tk"],
    );
}

#[test]
fn compile_non_overlapping_anons() {
    test_with_expectation(
        OutputFile("goldens/examples_non_overlapping_anons.cc".to_string()),
        vec!["examples/non_overlapping_anons.tk"],
    );
}

#[test]
fn compile_not() {
    test_with_expectation(
        OutputFile("goldens/examples_not.cc".to_string()),
        vec!["examples/not.tk"],
    );
}

#[test]
fn compile_optional_semis() {
    test_with_expectation(
        OutputFile("goldens/examples_optional_semis.cc".to_string()),
        vec!["examples/optional_semis.tk"],
    );
}

#[test]
fn compile_order_of_ops() {
    test_with_expectation(
        OutputFile("goldens/examples_order_of_ops.cc".to_string()),
        vec!["examples/order_of_ops.tk"],
    );
}

#[test]
fn compile_paren() {
    test_with_expectation(
        OutputFile("goldens/examples_paren.cc".to_string()),
        vec!["examples/paren.tk"],
    );
}

#[test]
fn compile_pow() {
    test_with_expectation(
        OutputFile("goldens/examples_pow.cc".to_string()),
        vec!["examples/pow.tk"],
    );
}

#[test]
fn compile_pow_twice() {
    test_with_expectation(
        OutputFile("goldens/examples_pow_twice.cc".to_string()),
        vec!["examples/pow_twice.tk"],
    );
}

#[test]
fn compile_shadowing() {
    test_with_expectation(Error, vec!["counter_examples/shadowing.tk"]);
}

#[test]
fn compile_simple() {
    test_with_expectation(
        OutputFile("goldens/examples_simple.cc".to_string()),
        vec!["examples/simple.tk"],
    );
}

#[test]
fn compile_simple_call() {
    test_with_expectation(
        OutputFile("goldens/examples_simple_call.cc".to_string()),
        vec!["examples/simple_call.tk"],
    );
}

#[test]
fn compile_sub() {
    test_with_expectation(
        OutputFile("goldens/examples_sub.cc".to_string()),
        vec!["examples/sub.tk"],
    );
}

#[test]
fn compile_three_vars() {
    test_with_expectation(
        OutputFile("goldens/examples_three_vars.cc".to_string()),
        vec!["examples/three_vars.tk"],
    );
}

#[test]
fn compile_tmp() {
    test_with_expectation(
        OutputFile("goldens/examples_tmp.cc".to_string()),
        vec!["examples/tmp.tk"],
    );
}

#[test]
fn compile_x_plus_1() {
    test_with_expectation(
        OutputFile("goldens/examples_x_plus_1.cc".to_string()),
        vec!["examples/x_plus_1.tk"],
    );
}

#[test]
fn defaulting() {
    interpret_with_success("examples/defaulting.tk")
}

#[test]
fn div() {
    interpret_with_success("examples/div.tk")
}

#[test]
fn dupe_alt() {
    test_with_expectation(Error, vec!["--run", "counter_examples/dupe_alt.tk"]);
}

#[test]
fn empty_args() {
    interpret_with_success("examples/empty_args.tk")
}

#[test]
fn empty_def_args() {
    interpret_with_success("examples/empty_def_args.tk")
}

#[test]
fn extension() {
    interpret_with_success("examples/extension.weird")
}

#[test]
fn fac() {
    interpret_with_success("examples/fac.tk")
}

#[test]
fn hello_name() {
    test_with_expectation(
        OutputFile("goldens/examples_hello_name.txt".to_string()),
        vec!["--run", "examples/hello_name.tk", "--", "Peanut", "Josh"],
    );
}

#[test]
fn higher_order() {
    interpret_with_success("examples/higher_order.tk")
}

#[test]
fn if_statement() {
    interpret_with_success("examples/if.tk")
}

#[test]
fn if_using_implicit_kw() {
    interpret_with_success("examples/if_using_implicit_kw.tk")
}

#[test]
fn ignored_let() {
    interpret_with_success("examples/ignored_let.tk")
}

#[test]
fn lambda() {
    interpret_with_success("examples/lambda.tk")
}

#[test]
fn r#loop() {
    interpret_with_success("examples/loop.tk")
}

#[test]
fn missing_arguments() {
    test_with_expectation(
        Error,
        vec!["--run", "counter_examples/missing_arguments.tk"],
    );
}

#[test]
fn multi_comment() {
    interpret_with_success("examples/multi_comment.tk")
}

#[test]
fn multi_comment_nested() {
    interpret_with_success("examples/multi_comment_nested.tk")
}

#[test]
fn mutual_recursion() {
    interpret_with_success("examples/mutual_recursion.tk")
}

#[test]
fn neg() {
    interpret_with_success("examples/neg.tk")
}

#[test]
fn negative_bool() {
    test_with_expectation(Error, vec!["--run", "counter_examples/negative_bool.tk"]);
}

#[test]
fn nested() {
    test_with_expectation(Error, vec!["--run", "counter_examples/nested.tk"]);
}

#[test]
fn nested_as_function() {
    interpret_with_success("examples/nested_as_function.tk")
}

#[test]
fn nested_explicit() {
    interpret_with_success("examples/nested_explicit.tk")
}

#[test]
fn nested_explicit_side_effect() {
    test_with_expectation(
        Error,
        vec!["--run", "counter_examples/nested_explicit_side_effect.tk"],
    );
}

#[test]
fn non_lambda() {
    test_with_expectation(Error, vec!["--run", "counter_examples/non_lambda.tk"]);
}

#[test]
fn non_overlapping_anons() {
    interpret_with_success("examples/non_overlapping_anons.tk")
}

#[test]
fn not() {
    interpret_with_success("examples/not.tk")
}

#[test]
fn not_int() {
    test_with_expectation(Error, vec!["--run", "counter_examples/not_int.tk"]);
}

#[test]
fn not_string() {
    test_with_expectation(Error, vec!["--run", "counter_examples/not_string.tk"]);
}

#[test]
fn optional_semis() {
    interpret_with_success("examples/optional_semis.tk")
}

#[test]
fn order_of_ops() {
    interpret_with_success("examples/order_of_ops.tk")
}

#[test]
fn override_print() {
    test_with_expectation(
        OutputFile("goldens/examples_override_print.txt".to_string()),
        vec!["--run", "examples/override_print.tk", "--"],
    );
}

#[test]
fn paren() {
    interpret_with_success("examples/paren.tk")
}

#[test]
fn pow() {
    interpret_with_success("examples/pow.tk")
}

#[test]
fn pow_twice() {
    interpret_with_success("examples/pow_twice.tk")
}

#[test]
fn printing() {
    compile_with_success("examples/printing.tk");
}

#[test]
fn error_printing() {
    compile_with_success("examples/error_printing.tk");
}

#[test]
fn requirement() {
    test_with_expectation(Error, vec!["--run", "counter_examples/requirement.tk"]);
}

#[test]
fn shadowing() {
    interpret_with_success("counter_examples/shadowing.tk")
}

#[test]
fn simple() {
    interpret_with_success("examples/simple.tk")
}

#[test]
fn simple_call() {
    interpret_with_success("examples/simple_call.tk")
}

#[test]
fn sub() {
    interpret_with_success("examples/sub.tk")
}

#[test]
fn sym_op() {
    interpret_with_error("counter_examples/sym_op.tk");
}

#[test]
fn three_vars() {
    interpret_with_success("examples/three_vars.tk")
}

#[test]
fn tmp() {
    interpret_with_success("examples/tmp.tk")
}

#[test]
fn type_bool() {
    interpret_with_success("examples/type_bool.tk")
}

#[test]
fn type_i32() {
    interpret_with_success("examples/type_i32.tk")
}

#[test]
fn type_str() {
    interpret_with_success("examples/type_str.tk")
}

#[test]
fn x_plus_1() {
    interpret_with_success("examples/x_plus_1.tk")
}
