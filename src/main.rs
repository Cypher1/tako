use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;

#[macro_use]
mod map_macros;

mod ast;
mod location;
mod parser;
mod tokens;
mod tree;
mod types;

mod interpreter;
mod pretty_print;
mod rescoper;
mod to_c;
mod wasm;
mod cli_options;
// Declared to make tests build, not used in main
mod test_options;

use ast::Visitor;
use interpreter::Interpreter;
use pretty_print::PrettyPrint;
use rescoper::ReScoper;

use cli_options::Options;
use cli_options::parse_args;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let opts = parse_args(&args[1..]);
    for f in opts.files.iter() {
        work(&f, &opts)?
    }
    Ok(())
}

fn work(filename: &str, opts: &Options) -> std::io::Result<()> {
    let mut contents = String::new();
    let mut file = File::open(filename.to_string())?;

    file.read_to_string(&mut contents)?;

    let program = parser::parse_file(filename.to_string(), contents);

    let mut scoper = ReScoper::default();
    scoper.debug = opts.debug;
    let scoped = scoper.visit_root(&program).expect("failed scoping");

    if opts.show_full_ast {
        eprintln!("debug ast: {:#?}", scoped);
    }
    if opts.show_ast {
        eprintln!("ast: {}", scoped);
    }

    if opts.interactive {
        let mut interp = Interpreter::default();
        interp.debug = opts.debug;
        let res = interp
            .visit_root(&scoped)
            .expect("could not interpret program");
        let mut ppr = PrettyPrint::default();
        use ast::ToNode;
        match ppr.visit_root(&res.to_node().to_root()) {
            Ok(res) => {
                eprintln!(">> {}", res);
            }
            Err(err) => {
                eprintln!("{:#?}", err);
            }
        }
        return Ok(());
    }

    if opts.wasm {
        let mut comp = wasm::Compiler::default();
        let res = comp.visit_root(&scoped).expect("could not compile program");
        println!("{}", res);
        return Ok(());
    }

    let mut comp = to_c::Compiler::default();
    let res = comp.visit_root(&scoped).expect("could not compile program");
    // println!("{}", res);

    let start_of_name = filename.rfind('/').unwrap_or(0);
    let dir = &filename[..start_of_name];
    let name = filename.trim_end_matches(".tk");

    std::fs::create_dir_all(format!("build/{}", dir))?;

    let outf = format!("build/{}.c", name);
    let execf = format!("build/{}", name);
    let destination = std::path::Path::new(&outf);
    let mut f = std::fs::File::create(&destination).expect("could not open output file");
    writeln!(f, "{}", res)?;

    let output = Command::new("gcc")
        .arg("-lm")
        .arg("-Wall")
        .arg("-Werror")
        .arg(outf)
        .arg("-o")
        .arg(execf)
        .output()?;
    if !output.status.success() {
        let s = String::from_utf8(output.stderr).unwrap();
        eprintln!("{}", s);
        panic!("Command executed with failing error code");
    }
    let s = String::from_utf8(output.stdout).unwrap();
    eprintln!("{}", s);
    Ok(())
}

#[cfg(test)]
mod tests {
    include!(concat!(env!("OUT_DIR"), "/test.rs"));
}
