use std::fs::{self, DirEntry};
use std::io;
use std::io::prelude::*;
use std::path::Path;
use std::str::FromStr;
use std::num::ParseIntError;

mod cli_options;
use cli_options::Options;
use cli_options::parseArgs;

// one possible implementation of walking a directory only visiting files
fn visit_dirs(dir: &Path, cb: &mut dyn FnMut(&DirEntry)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, cb)?;
            } else {
                cb(&entry);
            }
        }
    }
    Ok(())
}

#[derive(Debug, FromStr, PartialEq)]
enum TestResult {
    Panic,
    Success, // With an unspecified value
    ReturnValue(i32),
}

#![feature(str_strip)]
impl FromStr for TestResult {
    type Err = ParseIntError;

    fn from_str(res_: &str) -> Result<Self, Self::Err> {
        let res = res_.trim_matches(')');
        if res == "Panic" {
            return Ok(Panic);
        }
        if res == "Success" {
            return Ok(Success);
        }
        let arg = str.strip_prefix("ReturnValue(").expect("Unexpected test result value.");
        let arg_as_i32 = arg.parse::<i32>()?;
        Ok(ReturnValue(arg_as_i32))
    }
}


#[derive(Debug, PartialEq)]
struct TestOptions {
    opts: Options,
    expected: TestResult
}

impl FromStr for TestOptions {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let args = s.split("\n");
        let opts = parseArgs(args[1..]);
        let result = TestResult::from_str(args[0]);
    }
}


fn build_test(mut f: &std::fs::File, path: String, test: TestOptions) {
    build_test(&mut f, p, "\n#[should_panic]", interactive);

    let fn_name = path
        .replace("/", "_")
        .replace("\\", "_")
        .replace(".tk", "")
        .replace("._", "");
    write!(
        f,
        "
#[test]{test_type}
fn {fn_name}() {{
    let file = \"{name}\".to_string();
    let opts = {opts:?};
    super::work(&file, &opts).expect(\"failed to interpret\");
}}",
        name = path.replace("\\", "/"),
        fn_name = fn_name,
        test_type = test_type,
        opts = opts,
    )
    .unwrap();
}

fn files_from(path: &str) -> Vec<String> {
    let mut params: Vec<String> = vec![];
    visit_dirs(Path::new(path), &mut |filename| {
        let pth = filename.path();
        match pth.to_str() {
            Some(s) => {
                if s.ends_with(".tk") {
                    &mut params.push(s.to_string());
                }
            }
            _ => panic!(),
        }
    })
    .expect("Failed finding test files.");
    params
}

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let destination = std::path::Path::new(&out_dir).join("test.rs");
    let mut f = std::fs::File::create(&destination).unwrap();

    for p in files_from("examples") {
        build_test(&mut f, p, TestOptions {
            Options{interactive: true, wasm: false, ..Options::default()},
            expected: Success
        });
    }

    for p in files_from("counter_examples") {
        build_test(&mut f, p, TestOptions {
            Options{interactive: true, wasm: false, ..Options::default()},
            expected: Panic
        });
    }

    for p in files_from("compiled_examples_wasm") {
        build_test(&mut f, p, TestOptions {
            Options{interactive: false, wasm: true, ..Options::default()},
            expected: Success
        });
    }

    for p in files_from("compiled_examples_c") {
        build_test(&mut f, p, TestOptions {
            Options{interactive: false, wasm: false, ..Options::default()},
            expected: Success
        });
    }
}
