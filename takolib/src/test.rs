// Integration tests
use log::info;
use std::fs;
use std::path::PathBuf;
use test_each;

const TEST_CONFIG_PREFIX: &str = "// test: ";

#[derive(Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
enum TestResult {
    #[default]
    All, // Pass all stages
    TypeError,
    ParseError,
    LexError,
}

#[derive(Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
struct TestConfig {
    todo: bool,
    expect: TestResult,
}
impl TestConfig {
    fn from_strings(ctx: &PathBuf, tags: Vec<&str>) -> Self {
        let mut todo = false;
        let mut expect = TestResult::default();
        for tag in tags {
            match &*tag.to_lowercase() {
                "todo" => todo = true,
                "fix" | "error" => { /* Ignore these tags to support readable descriptions */ }
                "type" => expect = TestResult::TypeError,
                "lex" => expect = TestResult::LexError,
                "parse" => expect = TestResult::ParseError,
                "all" => expect = TestResult::All,
                _ => panic!("Unknown test tag {tag:?} in {ctx:#?}"),
            }
        }
        Self { todo, expect }
    }
}

fn file_and_options(file: &PathBuf) -> (String, TestConfig) {
    let contents = fs::read_to_string(&file).expect("Should have been able to read the file");
    let header = contents.splitn(3, '\n');
    let mut setting = TestConfig::default();

    for line in header {
        if !line.starts_with(TEST_CONFIG_PREFIX) {
            continue;
        }
        if let Some(value) = line.strip_prefix(TEST_CONFIG_PREFIX).map(|s| s.to_owned()) {
            setting = TestConfig::from_strings(file, value.split(" ").collect());
        }
    }

    (contents, setting)
}

#[test_each::path(glob = "examples/*.tk")]
fn parse_example_files(file: PathBuf) {
    let (contents, setting) = file_and_options(&file);
    if false && setting.todo {
        info!("Skipping todo file: {file:#?}");
        return;
    }
    info!("Start: {file:#?} ({setting:#?})");

    let tokens = match crate::parser::tokens::lex(&contents) {
        Err(e) => {
            assert_eq!(
                setting.expect,
                TestResult::LexError,
                "Unexpected failure for: {file:?}\n{e:?}"
            );
            return;
        }
        Ok(tokens) => {
            assert_ne!(
                setting.expect,
                TestResult::LexError,
                "Expected lex failure for: {file:?}\n{tokens:?}"
            );
            tokens
        }
    };

    // TODO: Macro or helper?
    let _ast = match crate::parser::parse(&file, &contents, &tokens) {
        Err(e) => {
            assert_eq!(
                setting.expect,
                TestResult::ParseError,
                "Unexpected failure for: {file:?}\n{e:?}"
            );
            return;
        }
        Ok(ast) => {
            assert_ne!(
                setting.expect,
                TestResult::ParseError,
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
        setting.expect,
        TestResult::All,
        "Performed all stages on: {file:?} but did not expect to?"
    );
}
