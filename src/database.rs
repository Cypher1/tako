use std::sync::Arc;

use std::collections::HashSet;
use std::collections::VecDeque;

use std::io::prelude::*;
use std::process::Command;

use crate::ast::{Node, Root, Visitor, Symbol};
use crate::cli_options::Options;
use crate::tokens::Token;

#[salsa::query_group(CompilerStorage)]
pub trait Compiler: salsa::Database {
    #[salsa::input]
    fn file(&self, filename: String) -> Arc<String>;

    #[salsa::input]
    fn options(&self) -> Options;
    fn debug(&self) -> i32;

    fn lex_file(&self, filename: String) -> VecDeque<Token>;
    fn parse_file(&self, filename: String) -> Node;
    fn build_symbol_table(&self, filename: String) -> Root;

    fn look_up_symbol(&self, filename: String, context: Vec<Symbol>, name: Vec<Symbol>) -> Option<Tree>;
    fn look_up_definitions(&self, filename: String) -> Root;

    fn compile_to_cpp(&self, filename: String) -> (String, HashSet<String>);
    fn build_with_gpp(&self, filename: String) -> String;
}

fn debug(db: &dyn Compiler) -> i32 {
    db.options().debug
}

fn lex_file(db: &dyn Compiler, filename: String) -> VecDeque<Token> {
    use crate::parser;
    if db.debug() > 0 {
        eprintln!("lexing file... {}", &filename);
    }
    parser::lex(Some(filename.to_string()), db.file(filename))
}

fn parse_file(db: &dyn Compiler, filename: String) -> Node {
    use crate::parser;
    parser::parse(&filename, db)
}

fn build_symbol_table(db: &dyn Compiler, filename: String) -> Root {
    use crate::symbol_table_builder::SymbolTableBuilder;
    SymbolTableBuilder::process(&filename, db).expect("failed building symbol table")
}

fn look_up_symbol(db: &dyn Compiler, filename: String, mut context: Vec<Symbol>, name: Vec<Symbol>) -> Option<Tree> {
    if db.debug() > 1 {
        eprintln!("looking up sym {:?} {:?}", &context, &name);
    }
    let root = db.build_symbol_table(filename.to_string());
    loop {
        while let Some(Symbol::Anon()) = context.last() {
            context.pop(); // Cannot look inside an 'anon'.
        }
        let mut search: Vec<Symbol> = context.clone();
        search.extend(name.clone());
        let node = root.table.find(&search);
        match node {
            Some(node) => {
                node.value.uses.push(context.clone());
                if db.debug() > 1 {
                    eprintln!("FOUND {:?} at {:?}\n", &name, &search);
                }
                return Some(node);
            }
            None => {
                if db.debug() > 1 {
                    eprintln!("   not found {} at {:?}", expr.name.clone(), search.clone());
                }
                if search.is_empty() {
                    return None;
                }
                context.pop(); // Up one, go again.
            }
        }
    }


}

fn look_up_definitions(db: &dyn Compiler, filename: String) -> Root {
    use crate::definition_finder::DefinitionFinder;
    DefinitionFinder::process(&filename, db).expect("failed looking up symbols")
}

fn compile_to_cpp(db: &dyn Compiler, filename: String) -> (String, HashSet<String>) {
    use crate::to_cpp::CodeGenerator;
    if db.debug() > 0 {
        eprintln!("generating code for file ... {}", &filename);
    }
    CodeGenerator::process(&filename, db).expect("could not compile program")
}

fn build_with_gpp(db: &dyn Compiler, filename: String) -> String {
    if db.debug() > 0 {
        eprintln!("building file with g++ ... {}", &filename);
    }
    let (res, flags) = db.compile_to_cpp(filename.to_string());

    let start_of_name = filename.rfind('/').unwrap_or(0);
    let dir = &filename[..start_of_name];
    let name = filename.trim_end_matches(".tk");

    std::fs::create_dir_all(format!("build/{}", dir)).expect("could not create build directory");

    let outf = format!("build/{}.cc", name);
    let execf = format!("build/{}", name);
    let destination = std::path::Path::new(&outf);
    let mut f = std::fs::File::create(&destination).expect("could not open output file");
    write!(f, "{}", res).expect("couldn't write to file");

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
        .output()
        .expect("could not run g++");
    if !output.status.success() {
        let s = String::from_utf8(output.stderr).unwrap();
        eprintln!("{}", s);
        panic!("Command executed with failing error code");
    }
    let s = String::from_utf8(output.stdout).unwrap();
    eprintln!("{}", s);
    res
}

#[salsa::database(CompilerStorage)]
#[derive(Default)]
pub struct DB {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DB {}
