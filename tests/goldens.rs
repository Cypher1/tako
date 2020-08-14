use std::fs::read_to_string;

use takolib::database::{{DB, Compiler}};
use takolib::cli_options::Options;
use pretty_assertions::assert_eq;

#[derive(Debug, PartialEq)]
pub enum TestResult {
    Error,
    Success, // With an unspecified value
    Output(String), // With an expected value
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
    let options = Options::new(options.to_vec());
    let mut db = DB::default();
    db.set_options(options);

    let mut stdout: Vec<String> = vec![];
    let result = {
        use takolib::ast::{Prim::{I32, Str}, Info};
        use takolib::interpreter::Res;
        let mut print_impl = |_: &dyn Compiler, args: Vec<&dyn Fn() -> takolib::interpreter::Res>, _: takolib::ast::Info| -> Res {
            stdout.push(match args[0]()? {
                Str(s,_)=>s,
                s=>format!("{:?}", s)
            });
            return Ok(I32(0, Info::default()))
        };
        let files = db.files();
        if files.len() != 1 {
            panic!("Tests currently only support a single file.");
        }
        takolib::work(&mut db, &files[0], Some(&mut print_impl)).expect("failed")
    };
    eprintln!("Result:\n{}", result);

    let expected_result = match expected {
        Output(s) => Some(s),
        OutputFile(gold) => {
            eprintln!("Loading golden file {}", gold);
            let contents = read_to_string(&gold);
            Some(contents.expect(format!("golden file {} could not be read", gold).as_str()))
        },
        _ => None,
    };

    if let Some(golden) = expected_result {
        assert_eq!(
            golden.replace("\r\n", "\n"),
            format!("{}{}", stdout.join(""), result)
        );
    }
}

#[test]
fn fn_name() {
    test_with_expectation(Success, vec!["examples/hello_name.tk", "-r"]);
}
