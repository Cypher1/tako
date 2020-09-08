use std::sync::Arc;

use std::collections::{HashMap, HashSet, VecDeque};

use std::io::prelude::*;
use std::process::Command;

use super::ast::{Node, Path, Root, Symbol, Table, Visitor};
use super::cli_options::Options;
use super::errors::TError;
use super::externs::{Extern, Semantic};
use super::tokens::Token;

#[salsa::query_group(CompilerStorage)]
pub trait Compiler: salsa::Database {
    #[salsa::input]
    fn file(&self, filename: String) -> Result<Arc<String>, TError>;

    #[salsa::input]
    fn options(&self) -> Options;
    fn debug(&self) -> i32;
    fn files(&self) -> Vec<String>;

    fn get_externs(&self) -> Result<HashMap<String, Extern>, TError>;
    fn get_extern_names(&self) -> Result<Vec<String>, TError>;
    fn get_extern(&self, name: String) -> Result<Option<Extern>, TError>;
    fn get_extern_operator(&self, name: String) -> Result<Semantic, TError>;

    fn module_name(&self, filename: String) -> Path;
    fn filename(&self, module: Path) -> String;

    fn lex_string(&self, module: Path, contents: Arc<String>) -> Result<VecDeque<Token>, TError>;
    fn lex_file(&self, module: Path) -> Result<VecDeque<Token>, TError>;
    fn parse_string(&self, module: Path, contents: Arc<String>) -> Result<Node, TError>;
    fn parse_str(&self, module: Path, contents: &'static str) -> Result<Node, TError>;
    fn parse_file(&self, module: Path) -> Result<Node, TError>;
    fn build_symbol_table(&self, module: Path) -> Result<Root, TError>;
    fn find_symbol(&self, mut context: Path, path: Path) -> Result<Option<Table>, TError>;
    fn find_symbol_uses(
        &self,
        mut context: Path,
        path: Path,
    ) -> Result<Option<HashSet<Path>>, TError>;

    fn look_up_definitions(&self, module: Path) -> Result<Root, TError>;

    fn infer(&self, expr: Node) -> Result<Node, TError>;

    fn compile_to_cpp(&self, module: Path) -> Result<(String, HashSet<String>), TError>;
    fn build_with_gpp(&self, module: Path) -> Result<String, TError>;
}

fn debug(db: &dyn Compiler) -> i32 {
    db.options().debug
}

fn files(db: &dyn Compiler) -> Vec<String> {
    db.options().files
}

pub fn module_name(_db: &dyn Compiler, filename: String) -> Path {
    filename
        .replace("\\", "/")
        .split('/')
        .map(|part| {
            let name: Vec<&str> = part.split('.').collect();
            Symbol::Named(
                name[0].to_owned(),
                if name.len() > 1 {
                    Some(name[1..].join("."))
                } else {
                    None
                },
            )
        })
        .collect()
}

pub fn filename(db: &dyn Compiler, module: Path) -> String {
    let parts: Vec<String> = module
        .iter()
        .map(|sym| match sym {
            Symbol::Named(sym, None) => sym.to_owned(),
            Symbol::Named(sym, Some(ext)) => format!("{}.{}", sym, ext),
            Symbol::Anon() => "?".to_owned(),
        })
        .collect();
    let file_name = parts.join("/");
    if db.debug() > 0 {
        eprintln!("Getting filename for {:?}, {:?}", module, file_name);
    }
    file_name
}

fn get_externs(db: &dyn Compiler) -> Result<HashMap<String, Extern>, TError> {
    crate::externs::get_externs(db)
}

fn get_extern_names(db: &dyn Compiler) -> Result<Vec<String>, TError> {
    Ok(db.get_externs()?.keys().cloned().collect())
}

fn get_extern(db: &dyn Compiler, name: String) -> Result<Option<Extern>, TError> {
    Ok(db.get_externs()?.get(&name).cloned())
}

fn get_extern_operator(db: &dyn Compiler, name: String) -> Result<Semantic, TError> {
    Ok(db.get_extern(name)?.map(|x| x.operator).unwrap_or(Semantic::Func))
}

fn lex_string(
    db: &dyn Compiler,
    module: Path,
    contents: Arc<String>,
) -> Result<VecDeque<Token>, TError> {
    use crate::parser;
    if db.debug() > 0 {
        eprintln!("lexing file... {:?}", &module);
    }
    parser::lex_string(db, &module, &contents)
}

fn lex_file(db: &dyn Compiler, module: Path) -> Result<VecDeque<Token>, TError> {
    use crate::parser;
    if db.debug() > 0 {
        eprintln!("lexing file... {:?}", &module);
    }
    parser::lex(db, &module)
}

fn parse_string(db: &dyn Compiler, module: Path, contents: Arc<String>) -> Result<Node, TError> {
    use crate::parser;
    parser::parse_string(db, &module, &contents)
}

fn parse_str(db: &dyn Compiler, module: Path, contents: &'static str) -> Result<Node, TError> {
    db.parse_string(module, Arc::new(contents.to_string()))
}

fn parse_file(db: &dyn Compiler, module: Path) -> Result<Node, TError> {
    use crate::parser;
    parser::parse(db, &module)
}

fn build_symbol_table(db: &dyn Compiler, module: Path) -> Result<Root, TError> {
    use crate::symbol_table_builder::SymbolTableBuilder;
    SymbolTableBuilder::process(&module, db)
}

fn find_symbol(db: &dyn Compiler, mut context: Path, path: Path) -> Result<Option<Table>, TError> {
    if db.debug() > 1 {
        eprintln!(">>> looking for symbol in {:?}, {:?}", context, path);
    }
    let table = db.look_up_definitions(context.clone())?.table;
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
            return Ok(Some(node.clone()));
        }
        if db.debug() > 1 {
            eprintln!("   not found {:?} at {:?}", path.clone(), search.clone());
        }
        if context.is_empty() {
            eprintln!("   not found {:?} at {:?}", path, search);
            return Ok(None);
        }
        context.pop(); // Up one, go again.
    }
}

fn find_symbol_uses(
    db: &dyn Compiler,
    context: Path,
    path: Path,
) -> Result<Option<HashSet<Path>>, TError> {
    Ok(db.find_symbol(context, path)?.map(|x| x.value.uses))
}

fn look_up_definitions(db: &dyn Compiler, module: Path) -> Result<Root, TError> {
    use crate::definition_finder::DefinitionFinder;
    if db.debug() > 0 {
        eprintln!("look up definitions >> {:?}", module);
    }
    DefinitionFinder::process(&module, db)
}

fn infer(db: &dyn Compiler, expr: Node) -> Result<Node, TError> {
    use crate::type_checker::infer;
    if db.debug() > 0 {
        eprintln!("infering type for ... {:?}", &expr);
    }
    infer(db, &expr)
}

fn compile_to_cpp(db: &dyn Compiler, module: Path) -> Result<(String, HashSet<String>), TError> {
    use crate::to_cpp::CodeGenerator;
    if db.debug() > 0 {
        eprintln!("generating code for file ... {:?}", &module);
    }
    CodeGenerator::process(&module, db)
}

fn build_with_gpp(db: &dyn Compiler, module: Path) -> Result<String, TError> {
    let (res, flags) = db.compile_to_cpp(module.clone())?;
    if db.debug() > 0 {
        eprintln!("building file with g++ ... {:?}", &module);
    }

    let name: String = module
        .iter()
        .map(|s| s.to_name())
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
        return Err(TError::CppCompilerError(s, output.status.code()));
    }
    let s = String::from_utf8(output.stdout).unwrap();
    eprintln!("{}", s);
    Ok(res)
}

#[salsa::database(CompilerStorage)]
#[derive(Default)]
pub struct DB {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DB {}
