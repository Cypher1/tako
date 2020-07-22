#![deny(clippy::all)]

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

mod cli_options;
mod database;
mod definition_finder;
mod errors;
mod interpreter;
mod pretty_print;
mod symbol_table_builder;
mod to_c;

// The following are only for tests
#[cfg(test)]
mod test_options;
#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

use ast::Visitor;
use definition_finder::DefinitionFinder;
use interpreter::Interpreter;
use pretty_print::PrettyPrint;
use symbol_table_builder::SymbolTableBuilder;

use cli_options::parse_args;
use cli_options::Options;
use database::{Compiler, DB};

use std::sync::Arc;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let mut db = DB::default();
    db.set_options(Arc::new(parse_args(&args[1..])));

    for f in db.options().files.iter() {
        let result = work(&mut db, &f)?; // discard the result (used for testing).
        eprintln!("{}", result);
    }
    Ok(())
}

fn work(db: &mut DB, filename: &str) -> std::io::Result<String> {
    let mut contents = String::new();
    let mut file = File::open(filename.to_string())?;
    file.read_to_string(&mut contents)?;

    let contents = Arc::new(contents);

    db.set_file(filename.to_string(), contents.clone());

    if db.options().interactive {
        let scoped = db.look_up_definitions(filename.to_string());

        use ast::Root;
        use ast::ToNode;
        let res = Interpreter::process(&scoped, &db.options().clone()).expect("could not interpret program");
        let res = PrettyPrint::process(&Root::new(res.to_node()), &db.options().clone())
            .or_else(|_| panic!("Pretty print failed"));
        return res;
    }

    let (res, flags) = db.compile_to_c(filename.to_string());

    let start_of_name = filename.rfind('/').unwrap_or(0);
    let dir = &filename[..start_of_name];
    let name = filename.trim_end_matches(".tk");

    std::fs::create_dir_all(format!("build/{}", dir))?;

    let outf = format!("build/{}.cc", name);
    let execf = format!("build/{}", name);
    let destination = std::path::Path::new(&outf);
    let mut f = std::fs::File::create(&destination).expect("could not open output file");
    write!(f, "{}", res)?;

    let mut cmd = Command::new("g++");
    for arg in flags.iter() {
        cmd.arg(arg);
    }
    let output = cmd
        .arg("-std=c++14")
        .arg("-Wall")
        .arg("-Werror")
        .arg("-O3")
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
    Ok(res)
}

#[cfg(test)]
mod tests {
    include!(concat!(env!("OUT_DIR"), "/test.rs"));
}
