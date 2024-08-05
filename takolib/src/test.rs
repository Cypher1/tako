// Integration tests
use log::{debug, info};
use std::fs;
use std::path::Path;
use test_each_file::test_each_path;

use crate::error::TError;

const TEST_CONFIG_PREFIX: &str = "// test: ";

#[derive(Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
enum TestResult {
    #[default]
    All, // Pass all stages
    TypeError,
    ParseError,
    LexError,
    InternalError,
}

#[derive(Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
struct TestConfig {
    todo: bool,
    expect: TestResult,
}
impl TestConfig {
    fn from_strings(ctx: &Path, tags: Vec<&str>) -> Self {
        let mut todo = false;
        let mut expect = TestResult::default();
        for tag in tags {
            match &*tag.to_lowercase() {
                "todo" => todo = true,
                "fix" | "error" => { /* Ignore these tags to support readable descriptions */ }
                "type" => expect = TestResult::TypeError,
                "lex" => expect = TestResult::LexError,
                "parse" => expect = TestResult::ParseError,
                "internal" => expect = TestResult::InternalError,
                "all" => expect = TestResult::All,
                _ => panic!("Unknown test tag {tag:?} in {ctx:#?}"),
            }
        }
        Self { todo, expect }
    }
}

fn file_and_options(file: &Path) -> (String, TestConfig) {
    let contents = fs::read_to_string(file).expect("Should have been able to read the file");
    let header = contents.splitn(3, '\n');
    let mut setting = TestConfig::default();

    for line in header {
        if !line.starts_with(TEST_CONFIG_PREFIX) {
            continue;
        }
        if let Some(value) = line.strip_prefix(TEST_CONFIG_PREFIX).map(|s| s.to_owned()) {
            setting = TestConfig::from_strings(file, value.split(' ').collect());
        }
    }

    (contents, setting)
}

test_each_path! { in "examples" => parse_example_files }
fn parse_example_files(file: &Path) {
    crate::ensure_initialized();

    let (contents, setting) = file_and_options(file);
    if setting.todo {
        // TODO: Support the todo tag to expect failure.
    }
    info!("Start: {file:#?} ({setting:#?})");

    let tokens = match crate::parser::tokens::lex(&contents) {
        Err(e) => {
            assert_eq!(
                TestResult::LexError,
                setting.expect,
                "Unexpected failure for: {file:?}\n{e:?}"
            );
            return;
        }
        Ok(tokens) => {
            assert_ne!(
                TestResult::LexError,
                setting.expect,
                "Expected lex failure for: {file:?}\n{tokens:?}"
            );
            tokens
        }
    };

    // TODO: Macro or helper?
    let _ast = match crate::parser::parse(file, &contents, &tokens) {
        Err(TError::InternalError { message, location }) => {
            let st = location.map(|l| l.start.into()).unwrap_or(0);
            let end = std::cmp::min(st + 50, contents.len());
            debug!("ERROR AT:\n{}", &contents[st..end]);
            assert_eq!(
                TestResult::InternalError,
                setting.expect,
                "Unexpected failure for: {file:?}\n{message:?}"
            );
            return;
        }
        Err(TError::ParseError(e)) => {
            let location = e.location();
            let st = location.map(|l| l.start.into()).unwrap_or(0);
            let end = std::cmp::min(st + 50, contents.len());
            debug!("ERROR AT:\n{}", &contents[st..end]);
            assert_eq!(
                TestResult::ParseError,
                setting.expect,
                "Unexpected failure for: {file:?}\n{e:?}"
            );
            return;
        }
        Err(e) => {
            assert_eq!(
                TestResult::ParseError,
                setting.expect,
                "Unexpected failure for: {file:?}\n{e:?}"
            );
            return;
        }
        Ok(ast) => {
            assert_ne!(
                TestResult::ParseError,
                setting.expect,
                "Expected parse failure for: {file:?}\n{ast:?}"
            );
            ast
        }
    };

    // TODO: Interpret
    // TODO: Type check
    // TODO: Compile
    // TODO: Run
    // TODO: Use tasks to get 'decorate_error:
    info!("Done: {file:#?}");
    assert_eq!(
        TestResult::All,
        setting.expect,
        "Performed all stages on: {file:?} but did not expect to?"
    );
}
