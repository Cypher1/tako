// Integration tests
use std::path::PathBuf;
use std::fs;
use walkdir::{DirEntry, WalkDir};
use log::info;

const EXAMPLES_DIR: &'static str = "../examples";

const EXCEPTIONS: &'static [&'static str] = &[
    "contexts.tk",
    "enums.tk",
    "fib.tk",
    "fib_acc.tk",
    "generic_abstract_data_types.tk",
    "instances.tk",
    "vector_transpose.tk",
    "vector_transpose_failing.tk",
];

fn is_hidden(entry: &DirEntry) -> bool {
    entry.file_name()
         .to_str()
         .map(|s| s.starts_with("."))
         .unwrap_or(false)
}

fn find_files() -> Result<Vec<PathBuf>, walkdir::Error> {
    let walker = WalkDir::new(EXAMPLES_DIR).into_iter();
    let mut paths = vec![];
    for entry in walker.filter_entry(|e| !is_hidden(e)) {
        let entry = entry?;
        let meta = entry.metadata()?;
        if !meta.is_file() {
            continue;
        }
        let path = entry.path();
        let name = path
            .file_name()
            .expect("Require that file names can be printed")
            .to_str()
            .expect("OSString to str");
        if EXCEPTIONS.contains(&name) {
            info!("Skipping exception: {name}");
            continue;
        }
        paths.push(path.to_path_buf());
    }
    Ok(paths)
}

#[test]
fn find_example_files() {
    let files = find_files().expect("Should be able to walk files");
    info!("Files: {files:#?}");
    let num_files = files.len();
    assert_eq!(
        num_files,
        1,
        "Example files appear to be missing, found {num_files}"
    );
}

#[test]
fn parse_example_files() {
    let mut files = find_files().expect("Should be able to walk files");
    files.sort();

    for file in files {
        info!("Start: {file:#?}");
        let contents = fs::read_to_string(&file)
            .expect("Should have been able to read the file");

        let tokens = crate::parser::tokens::lex(&contents).expect(&format!("Should be able to lex: {file:?}"));

        info!("Tokens: {tokens:#?}");

        let _ast = crate::parser::parse(&file, &contents, &tokens).expect(&format!("Should be able to parse: {file:?}"));
        info!("Done: {file:#?}");
    }
}
