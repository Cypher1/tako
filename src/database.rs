use std::sync::Arc;

use std::collections::HashSet;
use std::collections::VecDeque;

use std::io::prelude::*;
use std::process::Command;

use crate::ast::Root;
use crate::ast::Visitor;
use crate::cli_options::Options;
use crate::tokens::Token;

#[salsa::query_group(CompilerStorage)]
pub trait Compiler: salsa::Database {
    #[salsa::input]
    fn file(&self, filename: String) -> Arc<String>;

    #[salsa::input]
    fn options(&self) -> Arc<Options>;
    // TODO: Make each option an input.

    fn lex_file(&self, filename: String) -> VecDeque<Token>;
    fn parse_file(&self, filename: String) -> Root;
    fn build_symbol_table(&self, filename: String) -> Root;
    fn look_up_definitions(&self, filename: String) -> Root;
    fn compile_to_c(&self, filename: String) -> (String, HashSet<String>);
    fn build_with_gpp(&self, filename: String) -> String;
}

fn lex_file(db: &dyn Compiler, filename: String) -> VecDeque<Token> {
    use crate::parser;
    if db.options().debug > 0 {
        eprintln!("lexing file... {}", &filename);
    }
    parser::lex(Some(filename.to_string()), db.file(filename))
}

fn parse_file(db: &dyn Compiler, filename: String) -> Root {
    use crate::parser;
    if db.options().debug > 0 {
        eprintln!("parsing file... {}", &filename);
    }
    let ast = parser::parse(db.lex_file(filename));
    if db.options().show_ast {
        eprintln!("ast: {}", ast);
    }
    Root::new(ast)
}

fn build_symbol_table(db: &dyn Compiler, filename: String) -> Root {
    use crate::symbol_table_builder::SymbolTableBuilder;
    if db.options().debug > 0 {
        eprintln!("building symbol table for file... {}", &filename);
    }
    let with_symbols = SymbolTableBuilder::process(&db.parse_file(filename), &db.options())
        .expect("failed building symbol table");

    if db.options().show_ast {
        eprintln!("table {:?}", with_symbols.table.clone());
    }

    if db.options().show_full_ast {
        eprintln!("debug ast: {:#?}", with_symbols.ast);
    }
    with_symbols
}

fn look_up_definitions(db: &dyn Compiler, filename: String) -> Root {
    use crate::definition_finder::DefinitionFinder;
    if db.options().debug > 0 {
        eprintln!("looking up definitions in file... {}", &filename);
    }
    DefinitionFinder::process(&db.build_symbol_table(filename), &db.options())
        .expect("failed looking up symbols")
}

fn compile_to_c(db: &dyn Compiler, filename: String) -> (String, HashSet<String>) {
    use crate::to_c::Compiler;
    if db.options().debug > 0 {
        eprintln!("generating code for file ... {}", &filename);
    }
    Compiler::process(&db.look_up_definitions(filename), &db.options())
        .expect("could not compile program")
}

fn build_with_gpp(db: &dyn Compiler, filename: String) -> String {
    if db.options().debug > 0 {
        eprintln!("building file with g++ ... {}", &filename);
    }
    let (res, flags) = db.compile_to_c(filename.to_string());

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
