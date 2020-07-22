use std::sync::Arc;

use crate::cli_options::Options;
use std::collections::HashSet;
use crate::ast::Visitor;

#[salsa::query_group(CompilerStorage)]
pub trait Compiler: salsa::Database {
    #[salsa::input]
    fn file(&self, filename: String) -> Arc<String>;

    #[salsa::input]
    fn options(&self) -> Arc<Options>;

    fn parse_file(&self, filename: String) -> Root;
    fn build_symbol_table(&self, filename: String) -> Root;
    fn look_up_definitions(&self, filename: String) -> Root;
    fn compile_to_c(&self, filename: String) -> (String, HashSet<String>);
}

use crate::ast::Root;
fn parse_file(db: &dyn Compiler, filename: String) -> Root {
    use crate::parser;
    eprintln!("parsing file... {}", &filename);
    let ast = parser::parse_file(filename.to_string(), db.file(filename));
    if db.options().show_ast {
        eprintln!("ast: {}", ast);
    }
    ast
}

fn build_symbol_table(db: &dyn Compiler, filename: String) -> Root {
    use crate::symbol_table_builder::SymbolTableBuilder;
    eprintln!("building symbol table for file... {}", &filename);
    let with_symbols = SymbolTableBuilder::process(&db.parse_file(filename), &db.options()).expect("failed building symbol table");

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
    eprintln!("looking up definitions in file... {}", &filename);
    DefinitionFinder::process(&db.build_symbol_table(filename), &db.options()).expect("failed looking up symbols")
}

fn compile_to_c(db: &dyn Compiler, filename: String) -> (String, HashSet<String>) {

    use crate::to_c::Compiler;
    eprintln!("generating code for file ... {}", &filename);
    Compiler::process(&db.look_up_definitions(filename), &db.options().clone()).expect("could not compile program")
}

#[salsa::database(CompilerStorage)]
#[derive(Default)]
pub struct DB {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DB {}
