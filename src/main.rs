use std::env;
use std::fs::File;
use std::io::prelude::*;
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
mod wasm;

use ast::Visitor;
use interpreter::Interpreter;
use pretty_print::PrettyPrint;
use rescoper::ReScoper;
use wasm::Compiler;

#[derive(Debug)]
struct Options {
    files: Vec<String>,
    interactive: bool,
    show_ast: bool,
    show_full_ast: bool,
    debug: i32,
}

impl Default for Options {
    fn default() -> Options {
        Options {
            files: vec![],
            interactive: true,
            show_ast: false,
            show_full_ast: false,
            debug: 0,
        }
    }
}

const TITLE: &'static str = "tako v";

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

const USAGE: &'static str =
    "An experimental programming language for ergonomic software verification.

Usage:
  tako [-i|-r] [-d <level>] [--ast] [--full-ast] <files>...
  tako (-h | --help)
  tako --version

Options:
  -i --interactive    Run as a repl (interactive mode).
  -r --run            Run files in interpreter.
  -d --debug=<level>  Level of debug logging to use [default: 0].
  --ast               Pretty print an abstract syntax tree of the code.
  --full_ast          Debug print an abstract syntax tree of the code.
  -h --help           Show this screen.
  --version           Show compiler version.
";

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let mut opts = Options::default();
    for f in args[1..].to_vec() {
        if &f.chars().next() != &Some('-') {
            opts.files.push(f);
        } else {
            match f.as_str() {
                "-i" => {
                    opts.interactive = true;
                    opts.files.push("/dev/stdin".to_string());
                }
                "-r" => opts.interactive = true,
                "--wasm" => opts.interactive = false,
                "-d" => opts.debug += 1,
                "--ast" => opts.show_ast = true,
                "--full-ast" => opts.show_full_ast = true,
                "--version" => {
                    println!("{}{}", TITLE, VERSION);
                    return Ok(());
                }
                arg => {
                    if arg != "-h" && arg != "--help" {
                        println!("unexpected flag '{}'", f);
                    }
                    println!("{}{}\n{}", TITLE, VERSION, USAGE);
                    return Ok(());
                }
            }
        }
    }
    for f in opts.files.iter() {
        work(&f, &opts)?
    }
    Ok(())
}

fn work(filename: &String, opts: &Options) -> std::io::Result<()> {
    let mut contents = String::new();
    let mut file = File::open(filename.clone())?;
    println!("Filename: '{}'", filename);

    file.read_to_string(&mut contents)?;
    // println!("Content: '\n{}'", contents);

    let program = parser::parse_file(filename.clone(), contents);

    let mut scoper = ReScoper::default();
    scoper.debug = opts.debug;
    let scoped = scoper.visit_root(&program).expect("failed scoping");

    if opts.show_full_ast {
        println!("debug ast: {:#?}", scoped);
    }
    if opts.show_ast {
        println!("{}", scoped);
    }

    if opts.interactive {
        let mut interp = Interpreter::default();
        interp.debug = opts.debug;
        let res = interp
            .visit_root(&scoped)
            .expect("couldnt not interpret program");
        let mut ppr = PrettyPrint::default();
        use ast::ToNode;
        match ppr.visit_root(&res.to_node().to_root()) {
            Ok(res) => {
                println!(">> {}", res);
            }
            Err(err) => {
                println!("{:#?}", err);
            }
        }
        return Ok(());
    }
    let mut comp = Compiler::default();
    let res = comp
        .visit_root(&scoped)
        .expect("couldnt not compile program");
    println!("{}", res);
    return Ok(());
}

#[cfg(test)]
mod tests {
    include!(concat!(env!("OUT_DIR"), "/test.rs"));
}
