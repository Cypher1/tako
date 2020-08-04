use std::sync::Arc;

use std::collections::HashSet;
use std::collections::VecDeque;

use std::io::prelude::*;
use std::{path::MAIN_SEPARATOR, process::Command};

use crate::ast::{Node, Path, Root, Symbol, Table, Visitor};
use crate::cli_options::Options;
use crate::tokens::Token;

#[salsa::query_group(CompilerStorage)]
pub trait Compiler: salsa::Database {
    #[salsa::input]
    fn file(&self, filename: String) -> Arc<String>;

    #[salsa::input]
    fn options(&self) -> Options;
    fn debug(&self) -> i32;

    fn module_name(&self, filename: String) -> Path;
    fn filename(&self, module: Path) -> String;

    fn lex_file(&self, filename: String, module: Path) -> VecDeque<Token>;
    fn parse_file(&self, module: Path) -> Node;
    fn build_symbol_table(&self, module: Path) -> Root;
    fn find_symbol(&self, mut context: Path, path: Path) -> Option<Table>;
    fn find_symbol_uses(&self, mut context: Path, path: Path) -> Option<HashSet<Path>>;

    fn look_up_definitions(&self, module: Path) -> Root;

    fn compile_to_cpp(&self, module: Path) -> (String, HashSet<String>);
    fn build_with_gpp(&self, module: Path) -> String;
}

fn debug(db: &dyn Compiler) -> i32 {
    db.options().debug
}

pub fn module_name(_db: &dyn Compiler, filename: String) -> Path {
    filename
        .replace(".tk", "")
        .replace("\\", "/")
        .split('/')
        .map(|part| Symbol::Named(part.to_owned()))
        .collect()
}

pub fn filename(_db: &dyn Compiler, module: Path) -> String {
    let parts: Vec<&str> = module
        .iter()
        .map(|sym| match sym {
            Symbol::Named(sym) => sym,
            Symbol::Anon() => "?",
        })
        .collect();
    let file_name = format!("{}.tk", parts.join(&MAIN_SEPARATOR.to_string()));
    eprintln!("Getting filename for {:?}, {:?}", module, file_name);
    file_name
}

fn lex_file(db: &dyn Compiler, filename: String, module: Path) -> VecDeque<Token> {
    use crate::parser;
    if db.debug() > 0 {
        eprintln!("lexing file... {:?}", &module);
    }
    parser::lex(Some(&filename), db.file(db.filename(module)))
}

fn parse_file(db: &dyn Compiler, module: Path) -> Node {
    use crate::parser;
    parser::parse(&module, db)
}

fn build_symbol_table(db: &dyn Compiler, module: Path) -> Root {
    use crate::symbol_table_builder::SymbolTableBuilder;
    SymbolTableBuilder::process(&module, db).expect("failed building symbol table")
}

fn find_symbol(db: &dyn Compiler, mut context: Path, path: Path) -> Option<Table> {
    if db.debug() > 1 {
        eprintln!(">>> looking for symbol in {:?}, {:?}", context, path);
    }
    let table = db.look_up_definitions(context.clone()).table;
    loop {
        if let Some(Symbol::Anon()) = context.last() {
            context.pop(); // Cannot look inside an 'anon'.
        }
        let mut search: Vec<Symbol> = context.clone();
        search.extend(path.clone());
        if let Some(node) = table.find(&search) {
            if db.debug() > 1 {
                eprintln!("FOUND INSIDE {:?} {:?}", context, search);
            }
            return Some(node.clone());
        }
        if db.debug() > 1 {
            eprintln!("   not found {:?} at {:?}", path.clone(), search.clone());
        }
        if context.is_empty() {
            eprintln!("   not found {:?} at {:?}", path, search);
            return None;
        }
        context.pop(); // Up one, go again.
    }
}

fn find_symbol_uses(db: &dyn Compiler, context: Path, path: Path) -> Option<HashSet<Path>> {
    db.find_symbol(context, path).map(|x| x.value.uses)
}

fn look_up_definitions(db: &dyn Compiler, module: Path) -> Root {
    use crate::definition_finder::DefinitionFinder;
    if db.debug() > 0 {
        eprintln!("look up definitions >> {:?}", module);
    }
    DefinitionFinder::process(&module, db).expect("failed looking up symbols")
}

fn compile_to_cpp(db: &dyn Compiler, module: Path) -> (String, HashSet<String>) {
    use crate::to_cpp::CodeGenerator;
    if db.debug() > 0 {
        eprintln!("generating code for file ... {:?}", &module);
    }
    CodeGenerator::process(&module, db).expect("could not compile program")
}

fn build_with_gpp(db: &dyn Compiler, module: Path) -> String {
    let (res, flags) = db.compile_to_cpp(module.clone());
    if db.debug() > 0 {
        eprintln!("building file with g++ ... {:?}", &module);
    }

    let name: String = module
        .iter()
        .map(|s| format!("{}", s))
        .collect::<Vec<String>>()
        .join("_");

    let outf = format!("build/{}.cc", name);
    let execf = format!("build/{}", name);
    let destination = std::path::Path::new(&outf);
    std::fs::create_dir_all("build").expect("could not create build directory");
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
