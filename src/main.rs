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
mod wasm;
mod to_c;

use ast::Visitor;
use interpreter::Interpreter;
use pretty_print::PrettyPrint;
use rescoper::ReScoper;

#[derive(Debug)]
struct Options {
    files: Vec<String>,
    interactive: bool,
    wasm: bool,
    show_ast: bool,
    show_full_ast: bool,
    debug: i32,
}

impl Default for Options {
    fn default() -> Options {
        Options {
            files: vec![],
            interactive: false,
            wasm: false,
            show_ast: false,
            show_full_ast: false,
            debug: 0,
        }
    }
}

const TITLE: &str = "tako v";

const VERSION: &str = env!("CARGO_PKG_VERSION");

const USAGE: &str = "An experimental programming language for ergonomic software verification.

Usage:
  tako [-i|-r] [-d <level>] [--ast] [--full-ast] <files>...
  tako (-h | --help)
  tako --version

Options:
  -i --interactive    Run as a repl (interactive mode).
  -r --run            Run files in interpreter.
  -d --debug=<level>  Level of debug logging to use [default: 0].
  --wasm              Compile to wasm [default: false].
  --ast               Pretty print an abstract syntax tree of the code.
  --full_ast          Debug print an abstract syntax tree of the code.
  -h --help           Show this screen.
  --version           Show compiler version.
";

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let mut opts = Options::default();
    for f in args[1..].to_vec() {
        if !f.starts_with('-') {
            opts.files.push(f);
        } else {
            match f.as_str() {
                "-i" | "--interactive" => {
                    opts.interactive = true;
                    opts.files.push("/dev/stdin".to_string());
                }
                "-r" | "--run" => opts.interactive = true,
                "-d" => opts.debug += 1,
                "--ast" => opts.show_ast = true,
                "--wasm" => opts.wasm = true,
                "--full-ast" => opts.show_full_ast = true,
                "--version" => {
                    println!("{}{}", TITLE, VERSION);
                    return Ok(());
                }
                arg => {
                    if arg != "-h" && arg != "--help" {
                        eprintln!("unexpected flag '{}'", f);
                    }
                    eprintln!("{}{}\n{}", TITLE, VERSION, USAGE);
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
        let res = comp
            .visit_root(&scoped)
            .expect("could not compile program");
        println!("{}", res);
        return Ok(());
    }

    let mut comp = to_c::Compiler::default();
    let res = comp
        .visit_root(&scoped)
        .expect("could not compile program");
    // println!("{}", res);

    let outf = format!("{}.c", filename);
    let destination = std::path::Path::new(&outf);
    let mut f = std::fs::File::create(&destination).unwrap();
    write!(f, "{}\n", res)?;

    let output = Command::new("gcc").arg("-lm").arg("-Wall").arg("-Werror").arg(outf).output()?;
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
