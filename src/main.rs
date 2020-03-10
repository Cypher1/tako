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

use ast::Visitor;
use interpreter::Interpreter;
use pretty_print::PrettyPrint;
use rescoper::ReScoper;

use cli_options::Options;
use cli_options::USAGE;
use cli_options::TITLE;
use cli_options::VERSION;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let opts = parseArgs(args[1..].to_vec())?;
    for f in opts.files.iter() {
        work(&f, &opts)?
    }
    Ok(())
}

fn work(filename: &str, opts: &Options) -> std::io::Result<()> {
    let mut contents = String::new();
    let mut file = File::open(filename.to_string())?;
    eprintln!("Filename: '{}'", filename);

    file.read_to_string(&mut contents)?;
    // eprintln!("Content: '\n{}'", contents);

    let program = parser::parse_file(filename.to_string(), contents);

    let mut scoper = ReScoper::default();
    scoper.debug = opts.debug;
    let scoped = scoper.visit_root(&program).expect("failed scoping");

    if opts.show_full_ast {
        eprintln!("debug ast: {:#?}", scoped);
    }
    if opts.show_ast {
        eprintln!("{}", scoped);
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

    let outf = format!("{}.c", filename);
    let destination = std::path::Path::new(&outf);
    let mut f = std::fs::File::create(&destination).unwrap();
    writeln!(f, "{}", res)?;

    let output = Command::new("gcc")
        .arg("-lm")
        .arg("-Wall")
        .arg("-Werror")
        .arg(outf)
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
