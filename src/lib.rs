#![deny(clippy::all)]

mod typed_index;

#[macro_use]
extern crate lazy_static;

#[cfg(test)]
#[macro_use]
mod pretty_assertions;

#[macro_use]
pub mod data_structures;
#[macro_use]
pub mod cpp_ast;

pub mod cli_options;
pub mod jobs;
pub mod tako_jobs;
pub mod errors;
pub mod primitives;
pub mod parser;

mod ast;
mod location;
mod tokens;

use ast::Visitor;
use errors::TError;
use std::fs::File;
use std::io::prelude::*;

static mut LOGS_UNINITIALISED: bool = true;

pub fn build_logger(finish: impl FnOnce(&mut env_logger::Builder)) {
    if unsafe { LOGS_UNINITIALISED } {
        unsafe {
            LOGS_UNINITIALISED = false;
        }
        finish(
            env_logger::Builder::from_env(
                env_logger::Env::default()
                    .filter_or("TAKO_LOG", "warn")
                    .write_style_or("TAKO_LOG_STYLE", "AUTO"),
            )
            .format_timestamp(None),
        );
    }
}

#[cfg(test)]
pub fn ensure_initialized() {
    build_logger(|env| {
        let _ = env.is_test(true).try_init();
    });
}
#[cfg(not(test))]
pub fn ensure_initialized() {
    build_logger(env_logger::Builder::init);
}

pub fn work<'a>(
    storage: &mut DBStorage,
    filename: &str,
    print_impl: Option<ImplFn<'a>>,
) -> Result<String, TError> {
    let mut contents = String::new();
    let mut file = File::open(filename)?;
    file.read_to_string(&mut contents)?;

    work_on_string(storage, contents, filename, print_impl)
}

pub fn work_on_string<'a>(
    storage: &mut DBStorage,
    contents: String,
    filename: &str,
    print_impl: Option<ImplFn<'a>>,
) -> Result<String, TError> {
    use cli_options::Command;

    let module_name = storage.module_name(filename);
    storage.set_file(filename, contents);

    match storage.options.cmd {
        Command::Build => storage.build_with_gpp(&module_name),
        Command::Interpret | Command::Repl => {
            let root = storage.look_up_definitions(&module_name)?;
            let mut interp = Interpreter::default();
            if let Some(print_impl) = print_impl {
                interp.impls.insert("print".to_string(), print_impl);
            }
            let res = interp.visit_root(storage, &root)?;
            PrettyPrint::process(&res.into_node(), storage)
                .or_else(|_| panic!("Pretty print failed"))
        }
        Command::StackInterpret | Command::StackRepl => {
            let _root = storage.look_up_definitions(&module_name)?;
            let root_entity = *storage
                .path_to_entity
                .get(&module_name)
                .expect("Expected an entity for the program");
            let mut interp = crate::passes::stack_interpreter::Interpreter::new(storage);
            if let Some(print_impl) = print_impl {
                interp.default_impls.insert("print".to_string(), print_impl);
            }
            let res = interp.eval(root_entity)?;
            PrettyPrint::process(&res.into_node(), storage)
                .or_else(|_| panic!("Pretty print failed"))
        }
    }
}
