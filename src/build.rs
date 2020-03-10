use std::fs::{self, DirEntry};
use std::io;
use std::io::prelude::*;
use std::path::Path;
use std::str::FromStr;

mod cli_options;
mod test_options;
use test_options::TestOptions;
use test_options::TestResult;

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

fn build_test(mut f: &std::fs::File, path: String, test: &str) {
    let opts = TestOptions::from_str(test).expect("Couldn't read test options");
    let test_type = if opts.expected == TestResult::Panic {
        "\n#[should_panic]"
    } else { "" };

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
    let opts = TestOptions::from_str(\"{opts}\").expect(\"Couldn't read test options\");
    super::work(&file, &opts.opts).expect(\"failed to interpret\");
}}",
        name = path.replace("\\", "/"),
        fn_name = fn_name,
        test_type = test_type,
        opts = test,
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

fn main() -> std::io::Result<()> {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let destination = std::path::Path::new(&out_dir).join("test.rs");
    let mut f = std::fs::File::create(&destination).unwrap();

    writeln!(f, "use super::test_options::TestOptions;")?;
    writeln!(f, "use std::str::FromStr;")?;

    for p in files_from("examples") {
        build_test(&mut f, p, "Success\n--interactive");
    }

    for p in files_from("counter_examples") {
        build_test(&mut f, p, "Panic\n--interactive");
    }

    for p in files_from("compiled_examples_wasm") {
        build_test(&mut f, p, "Success\n--wasm");
    }

    for p in files_from("compiled_examples_c") {
        build_test(&mut f, p, "Success");
    }
    Ok(())
}
