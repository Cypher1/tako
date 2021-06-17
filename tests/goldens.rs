use std::collections::HashMap;
use std::fs::read_to_string;

use pretty_assertions::assert_eq;
use takolib::cli_options::Options;
use takolib::database::{Compiler, DB};
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

fn test_with_expectation(expected: TestResult, options: Vec<&str>) -> Test {
    let options = Options::new(options);
    let mut db = DB::default();
    db.set_options(options);

    let mut stdout: Vec<String> = vec![];
    let result = {
        use takolib::interpreter::Res;
        use takolib::primitives::Prim::{Str, I32};
        use takolib::primitives::Val::PrimVal;
        let mut print_impl =
            &mut |_: &dyn Compiler,
                  args: HashMap<String, Box<dyn Fn() -> takolib::interpreter::Res>>,
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
        let files = db.files();
        if files.len() != 1 {
            panic!("Tests currently only support a single file.");
        }
        takolib::work(&mut db, &files[0], Some(&mut print_impl))
    };

    match (result, expected) {
        (Ok(result), Success) => {
            eprintln!("Success. Result:\n{:?}", result);
            Ok(())
        }
        (Ok(result), ReturnValue(value)) => {
            assert_eq!(result, format!("{}", value));
            eprintln!("Success. Result:\n{:?}", result);
            Ok(())
        }
        (Ok(result), Output(s)) => {
            assert_eq!(s, format!("{}{}", stdout.join(""), result));
            eprintln!("Success. Result:\n{:?}", result);
            Ok(())
        }
        (Ok(result), OutputFile(gold)) => {
            eprintln!("Loading golden file {}", gold);
            let read = read_to_string(&gold);
            let golden = read
                .unwrap_or_else(|_| panic!("golden file {} could not be read", gold))
                .replace("\r", "");
            assert_eq!(format!("{}{}", stdout.join(""), result), golden);
            eprintln!("Success. Result:\n{:?}", result);
            Ok(())
        }
        (Err(err), Error) => {
            eprintln!("Received error:\n{:?}", err);
            Ok(())
        }
        (Ok(result), Error) => {
            eprintln!("---Got result---\n{:?}", result);
            panic!("Expected error");
        }
        (Err(err), expectation) => {
            eprintln!("---Expected---\n{:?}", expectation);
            panic!("Error: {}", err);
        }
    }
}

fn compile_with_success(file: &str) -> Test {
    test_with_expectation(Success, vec![file])
}

fn interpret_with_success(file: &str) -> Test {
    test_with_expectation(Success, vec!["--run", file])
}

fn interpret_with_error(file: &str) -> Test {
    test_with_expectation(Error, vec!["--run", file])
}

#[test]
fn one_plus_2() -> Test {
    interpret_with_success("examples/1_plus_2.tk")
}

#[test]
fn plus_123() -> Test {
    test_with_expectation(
        Output("6".to_string()),
        vec!["--run", "examples/plus_123.tk"],
    )
}

#[test]
fn alt() -> Test {
    interpret_with_success("examples/alt.tk")
}

#[test]
fn arguments() -> Test {
    interpret_with_success("examples/arguments.tk")
}

//#[test] // Re-enable when type checking works
fn assignment_returns_unit() -> Test {
    interpret_with_error("counter_examples/assignment_returns.tk")
}

#[test]
fn bare_words() -> Test {
    interpret_with_error("counter_examples/bare_words.tk")
}

#[test]
fn bool_requirement() -> Test {
    interpret_with_success("examples/bool_requirement.tk")
}

#[test]
fn bool_times_bool() -> Test {
    interpret_with_error("counter_examples/bool_times_bool.tk")
}

#[test]
fn code_reuse() -> Test {
    interpret_with_success("examples/code_reuse.tk")
}

#[test]
fn comment() -> Test {
    interpret_with_success("examples/comment.tk")
}

#[test]
fn compile_1_plus_2() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_1_plus_2.cc".to_string()),
        vec!["examples/1_plus_2.tk"],
    )
}

#[test]
fn compile_arguments() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_arguments.cc".to_string()),
        vec!["examples/arguments.tk"],
    )
}

#[test]
fn compile_code_reuse() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_code_reuse.cc".to_string()),
        vec!["examples/code_reuse.tk"],
    )
}

#[test]
fn compile_comment() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_comment.cc".to_string()),
        vec!["examples/comment.tk"],
    )
}

#[test]
fn compile_div() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_div.cc".to_string()),
        vec!["examples/div.tk"],
    )
}

#[test]
fn compile_empty_def_args() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_empty_def_args.cc".to_string()),
        vec!["examples/empty_def_args.tk"],
    )
}

#[test]
fn compile_hello_name() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_hello_name.cc".to_string()),
        vec!["examples/hello_name.tk"],
    )
}

#[test]
fn compile_higher_order() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_higher_order.cc".to_string()),
        vec!["examples/higher_order.tk"],
    )
}

#[test]
fn compile_ignored_let() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_ignored_let.cc".to_string()),
        vec!["examples/ignored_let.tk"],
    )
}

#[test]
fn compile_lambda() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_lambda.cc".to_string()),
        vec!["examples/lambda.tk"],
    )
}

#[test]
fn compile_multi_comment() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_multi_comment.cc".to_string()),
        vec!["examples/multi_comment.tk"],
    )
}

#[test]
fn compile_multi_comment_nested() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_multi_comment_nested.cc".to_string()),
        vec!["examples/multi_comment_nested.tk"],
    )
}

#[test]
fn compile_neg() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_neg.cc".to_string()),
        vec!["examples/neg.tk"],
    )
}

#[test]
fn compile_nested() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_nested.cc".to_string()),
        vec!["examples/nested.tk"],
    )
}

#[test]
fn compile_nested_as_function() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_nested_as_function.cc".to_string()),
        vec!["examples/nested_as_function.tk"],
    )
}

#[test]
fn compile_nested_explicit() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_nested_explicit.cc".to_string()),
        vec!["examples/nested_explicit.tk"],
    )
}

#[test]
fn compile_non_overlapping_anons() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_non_overlapping_anons.cc".to_string()),
        vec!["examples/non_overlapping_anons.tk"],
    )
}

#[test]
fn compile_not() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_not.cc".to_string()),
        vec!["examples/not.tk"],
    )
}

#[test]
fn compile_optional_semis() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_optional_semis.cc".to_string()),
        vec!["examples/optional_semis.tk"],
    )
}

#[test]
fn compile_order_of_ops() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_order_of_ops.cc".to_string()),
        vec!["examples/order_of_ops.tk"],
    )
}

#[test]
fn compile_paren() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_paren.cc".to_string()),
        vec!["examples/paren.tk"],
    )
}

#[test]
fn compile_pow() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_pow.cc".to_string()),
        vec!["examples/pow.tk"],
    )
}

#[test]
fn compile_pow_twice() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_pow_twice.cc".to_string()),
        vec!["examples/pow_twice.tk"],
    )
}

#[test]
fn compile_shadowing() -> Test {
    test_with_expectation(Error, vec!["counter_examples/shadowing.tk"])
}

#[test]
fn compile_simple() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_simple.cc".to_string()),
        vec!["examples/simple.tk"],
    )
}

#[test]
fn compile_simple_call() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_simple_call.cc".to_string()),
        vec!["examples/simple_call.tk"],
    )
}

#[test]
fn compile_sub() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_sub.cc".to_string()),
        vec!["examples/sub.tk"],
    )
}

#[test]
fn compile_three_vars() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_three_vars.cc".to_string()),
        vec!["examples/three_vars.tk"],
    )
}

#[test]
fn compile_tmp() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_tmp.cc".to_string()),
        vec!["examples/tmp.tk"],
    )
}

#[test]
fn compile_x_plus_1() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_x_plus_1.cc".to_string()),
        vec!["examples/x_plus_1.tk"],
    )
}

#[test]
fn defaulting() -> Test {
    interpret_with_success("examples/defaulting.tk")
}

#[test]
fn div() -> Test {
    interpret_with_success("examples/div.tk")
}

#[test]
fn dupe_alt() -> Test {
    test_with_expectation(Error, vec!["--run", "counter_examples/dupe_alt.tk"])
}

#[test]
fn empty_args() -> Test {
    interpret_with_success("examples/empty_args.tk")
}

#[test]
fn empty_def_args() -> Test {
    interpret_with_success("examples/empty_def_args.tk")
}

#[test]
fn extension() -> Test {
    interpret_with_success("examples/extension.weird")
}

#[test]
fn fac() -> Test {
    interpret_with_success("examples/fac.tk")
}

#[test]
fn hello_name() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_hello_name.txt".to_string()),
        vec!["--run", "examples/hello_name.tk", "--", "Peanut", "Jay"],
    )
}

#[test]
fn higher_order() -> Test {
    interpret_with_success("examples/higher_order.tk")
}

#[test]
fn if_statement() -> Test {
    interpret_with_success("examples/if.tk")
}

#[test]
fn if_using_implicit_kw() -> Test {
    interpret_with_success("examples/if_using_implicit_kw.tk")
}

#[test]
fn ignored_let() -> Test {
    interpret_with_success("examples/ignored_let.tk")
}

#[test]
fn lambda() -> Test {
    interpret_with_success("examples/lambda.tk")
}

#[test]
fn r#loop() -> Test {
    interpret_with_success("examples/loop.tk")
}

#[test]
fn missing_arguments() -> Test {
    test_with_expectation(
        Error,
        vec!["--run", "counter_examples/missing_arguments.tk"],
    )
}

#[test]
fn multi_comment() -> Test {
    interpret_with_success("examples/multi_comment.tk")
}

#[test]
fn multi_comment_nested() -> Test {
    interpret_with_success("examples/multi_comment_nested.tk")
}

#[test]
fn mutual_recursion() -> Test {
    interpret_with_success("examples/mutual_recursion.tk")
}

#[test]
fn neg() -> Test {
    interpret_with_success("examples/neg.tk")
}

#[test]
fn negative_bool() -> Test {
    test_with_expectation(Error, vec!["--run", "counter_examples/negative_bool.tk"])
}

#[test]
fn nested() -> Test {
    test_with_expectation(Error, vec!["--run", "counter_examples/nested.tk"])
}

#[test]
fn nested_as_function() -> Test {
    interpret_with_success("examples/nested_as_function.tk")
}

#[test]
fn nested_explicit() -> Test {
    interpret_with_success("examples/nested_explicit.tk")
}

#[test]
fn nested_explicit_side_effect() -> Test {
    test_with_expectation(
        Error,
        vec!["--run", "counter_examples/nested_explicit_side_effect.tk"],
    )
}

#[test]
fn non_lambda() -> Test {
    test_with_expectation(Error, vec!["--run", "counter_examples/non_lambda.tk"])
}

#[test]
fn non_overlapping_anons() -> Test {
    interpret_with_success("examples/non_overlapping_anons.tk")
}

#[test]
fn not() -> Test {
    interpret_with_success("examples/not.tk")
}

#[test]
fn not_int() -> Test {
    test_with_expectation(Error, vec!["--run", "counter_examples/not_int.tk"])
}

#[test]
fn not_string() -> Test {
    test_with_expectation(Error, vec!["--run", "counter_examples/not_string.tk"])
}

#[test]
fn optional_semis() -> Test {
    interpret_with_success("examples/optional_semis.tk")
}

#[test]
fn order_of_ops() -> Test {
    interpret_with_success("examples/order_of_ops.tk")
}

#[test]
fn override_print() -> Test {
    test_with_expectation(
        OutputFile("goldens/examples_override_print.txt".to_string()),
        vec!["--run", "examples/override_print.tk", "--"],
    )
}

#[test]
fn paren() -> Test {
    interpret_with_success("examples/paren.tk")
}

#[test]
fn pow() -> Test {
    interpret_with_success("examples/pow.tk")
}

#[test]
fn pow_twice() -> Test {
    interpret_with_success("examples/pow_twice.tk")
}

#[test]
fn printing() -> Test {
    compile_with_success("examples/printing.tk")
}

#[test]
fn error_printing() -> Test {
    interpret_with_success("examples/error_printing.tk")
}

#[test]
fn compile_error_printing() -> Test {
    compile_with_success("examples/error_printing.tk")
}

#[test]
fn compile_parse_i32() -> Test {
    compile_with_success("examples/parse_i32.tk")
}

#[test]
fn requirement() -> Test {
    test_with_expectation(Error, vec!["--run", "counter_examples/requirement.tk"])
}

#[test]
fn shadowing() -> Test {
    interpret_with_success("counter_examples/shadowing.tk")
}

#[test]
fn simple() -> Test {
    interpret_with_success("examples/simple.tk")
}

#[test]
fn simple_call() -> Test {
    interpret_with_success("examples/simple_call.tk")
}

#[test]
fn sub() -> Test {
    interpret_with_success("examples/sub.tk")
}

#[test]
fn sym_op() -> Test {
    interpret_with_error("counter_examples/sym_op.tk")
}

#[test]
fn three_vars() -> Test {
    interpret_with_success("examples/three_vars.tk")
}

#[test]
fn tmp() -> Test {
    interpret_with_success("examples/tmp.tk")
}

#[test]
fn type_bool() -> Test {
    interpret_with_success("examples/type_bool.tk")
}

#[test]
fn type_i32() -> Test {
    interpret_with_success("examples/type_i32.tk")
}

#[test]
fn type_str() -> Test {
    interpret_with_success("examples/type_str.tk")
}

#[test]
fn x_plus_1() -> Test {
    interpret_with_success("examples/x_plus_1.tk")
}
