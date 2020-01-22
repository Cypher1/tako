use std::env;
use std::fs::File;
use std::io::prelude::*;
#[macro_use]
mod map_macros;

mod ast;
mod parser;
mod tokens;
mod tree;
mod location;
mod types;

mod wasm;
mod interpreter;
mod pretty_print;
mod rescoper;

use ast::Visitor;
use wasm::Compiler;
use interpreter::Interpreter;
use pretty_print::PrettyPrint;
use rescoper::ReScoper;

fn main() -> std::io::Result<()> {
    let all_args: Vec<String> = env::args().collect();
    let args: Vec<String> = all_args[1..].to_vec();
    let mut files: Vec<String> = vec![];
    let mut interactive = false;
    let mut show_ast = false;
    for f in args {
        match f.as_str() {
            "-i" => {
                interactive = true;
                files.push("/dev/stdin".to_string());
            }
            "-r" => interactive = true,
            "--ast" => show_ast = true,
            _ => files.push(f),
        }
    }
    for f in files {
        work(f, interactive, show_ast)?
    }
    Ok(())
}

fn work(filename: String, interactive: bool, show_ast: bool) -> std::io::Result<()> {
    let mut contents = String::new();
    let mut file = File::open(filename.clone())?;
    println!("Filename: '{}'", filename);

    file.read_to_string(&mut contents)?;
    // println!("Content: '\n{}'", contents);

    let ast = parser::parse_file(filename, contents);

    if show_ast {
        println!("R: {:#?}", ast);
        let mut ppr = PrettyPrint::default();
        match ppr.visit_root(&ast) {
            Ok(res) => {
                println!("R: {}", res);
            },
            Err(err) => {
                println!("{:#?}", err);
            }
        }
    }

    let mut rescoper = ReScoper::default();
    let scoped_ast = match rescoper.visit_root(&ast) {
        Ok(res) => res,
        Err(err) => panic!(format!("{:#?}", err)),
    };

    if interactive {
        let mut interp = Interpreter::default();
        match interp.visit_root(&scoped_ast) {
            Ok(res) => {
                println!("{:#?}", res);
            },
            Err(err) => {
                println!("{:#?}", err);
            }
        }
        return Ok(());
    }
    let mut comp = Compiler::default();
    match comp.visit_root(&scoped_ast) {
        Ok(res) => {
            println!("{}", res);
        },
        Err(err) => {
            println!("{:#?}", err);
        }
    }
    return Ok(());
}

#[cfg(test)]
    mod tests {
    use std::fs::File;
    use std::io::prelude::*;

    use super::ast::Visitor;
    use super::interpreter::Interpreter;
    use super::parser;

    include!(concat!(env!("OUT_DIR"), "/test.rs"));
}

