use std::fs::{self, DirEntry};
use std::io;
use std::io::prelude::*;
use std::path::Path;

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

fn build_test(mut f: &std::fs::File, p: String, test_type: &str, opts: &str) {
    let nm = p
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
    let{muted} opts = super::Options::default();
    {opts}
    super::work(&file, &opts).expect(\"failed to interpret\");
}}",
        name = p.replace("\\", "/"),
        fn_name = nm,
        test_type = test_type,
        opts = opts,
        muted = if opts == "" { "" } else { " mut" }
    )
    .unwrap();
}

fn files_from(path: &str) -> Vec<String> {
    let mut params: Vec<String> = vec![];
    visit_dirs(Path::new(path), &mut |filename| {
        let pth = filename.path();
        match pth.to_str() {
            Some(s) => {
                &mut params.push(s.to_string());
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

    let interactive = "opts.interactive = true;";
    for p in files_from("examples") {
        build_test(&mut f, p, "", interactive);
    }

    for p in files_from("counter_examples") {
        build_test(&mut f, p, "\n#[should_panic]", interactive);
    }

    for p in files_from("compiled_examples") {
        build_test(&mut f, p, "", "");
    }

}
