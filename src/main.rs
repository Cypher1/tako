#![deny(clippy::all)]

use std::env;

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
mod to_cpp;

// The following are only for tests
#[cfg(test)]
mod test_options;
#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

use ast::Visitor;
use interpreter::Interpreter;
use pretty_print::PrettyPrint;

use cli_options::parse_args;
use database::{Compiler, DB};

use std::fs::File;
use std::io::prelude::*;
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

    db.set_file(filename.to_string(), contents);

    if db.options().interactive {
        let scoped = db.look_up_definitions(filename.to_string());

        let res =
            Interpreter::process(&scoped, &db.options()).expect("could not interpret program");
        use ast::{Root, ToNode};
        PrettyPrint::process(&Root::new(res.to_node()), &db.options())
            .or_else(|_| panic!("Pretty print failed"))
    } else {
        Ok(db.build_with_gpp(filename.to_string()))
    }
}

#[cfg(test)]
mod tests {
    include!(concat!(env!("OUT_DIR"), "/test.rs"));
}
