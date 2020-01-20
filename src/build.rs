use std::io;
use std::fs::{self, DirEntry};
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


fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let destination = std::path::Path::new(&out_dir).join("test.rs");
    let mut f = std::fs::File::create(&destination).unwrap();

    let mut params: Vec<String> = vec![];
    visit_dirs(Path::new("examples"), &mut |filename| {
        let pth = filename.path();
        match pth.to_str() {
            Some(s) => {
                &mut params.push(s.to_string());
            },
            _ => panic!(),
        }
    }).expect("Failed finding test files.");

    for p in params {
        let nm = p.replace("/", "_").replace("\\", "_").replace(".tk", "").replace("._", "");
        write!(
            f,
            "
#[test]
fn {fn_name}() {{
    let mut contents = String::new();
    let mut file = File::open(\"{name}\").expect(\"File missing\");
    file.read_to_string(&mut contents).expect(\"Couldnt read file\");

    let ast = parser::parse_file(\"{name}\".to_string(), contents);
    let mut interp = Interpreter::default();
    interp.visit_root(&ast).expect(\"Failed to evaluate ast\");
}}",
            name = p.replace("\\", "/"),
            fn_name = nm
        ).unwrap();
    }

    let mut params: Vec<String> = vec![];
    visit_dirs(Path::new("counter_examples"), &mut |filename| {
        let pth = filename.path();
        match pth.to_str() {
            Some(s) => {
                &mut params.push(s.to_string());
            },
            _ => panic!(),
        }
    }).expect("Failed finding test files.");

    for p in params {
        let nm = p.replace("/", "_").replace("\\", "_").replace(".tk", "").replace("._", "");
        write!(
            f,
            "
#[test]
#[should_panic]
fn {fn_name}() {{
    let mut contents = String::new();
    let mut file = File::open(\"{name}\").expect(\"File missing\");
    file.read_to_string(&mut contents).expect(\"Couldnt read file\");

    let ast = parser::parse_file(\"{name}\".to_string(), contents);
    let mut interp = Interpreter::default();
    interp.visit_root(&ast).expect(\"Failed to evaluate ast\");
}}",
            name = p.replace("\\", "/"),
            fn_name = nm
        ).unwrap();
    }
}
