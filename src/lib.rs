#![deny(clippy::all)]

#[macro_use]
pub mod free_standing;

pub mod cli_options;
pub mod compiler_context;
pub mod compiler_tasks;
pub mod concepts;
pub mod error;
pub mod primitives;
pub mod ui;
pub mod parser;
pub mod ast;
pub mod keywords;
pub mod location;
pub mod string_interner;
pub mod tokens;

use compiler_context::CompilerContext;
use error::TError;
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
                    .filter_or("RUST_LOG", "warn")
                    .write_style_or("RUST_LOG_STYLE", "AUTO"),
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

pub fn work(
    storage: &mut CompilerContext,
    filename: &str,
    // print_impl: Option<ImplFn<'a>>,
) -> Result<String, TError> {
    let mut contents = String::new();
    let mut file = File::open(filename)?;
    file.read_to_string(&mut contents)?;

    work_on_string(storage, contents, filename /*, print_impl*/)
}

pub fn work_on_string(
    _storage: &mut CompilerContext,
    _contents: String,
    _filename: &str,
    // print_impl: Option<ImplFn<'a>>,
) -> Result<String, TError> {
    todo!("Dunno...");
    /*
    use cli_options::Command;
    let module_name = storage.module_name(filename);
    storage.set_file(filename, contents);

    match storage.options.cmd {
        Command::Build => storage.build_with_gpp(&module_name),
        Command::Interpret | Command::Repl => {
            let root = storage.look_up_definitions(&module_name)?;
            todo!();
            // let mut interp = Interpreter::default();
            // if let Some(print_impl) = print_impl {
                // interp.impls.insert("print".to_string(), print_impl);
            // }
            // let res = interp.visit_root(storage, &root)?;
            // PrettyPrint::process(&res.into_node(), storage)
                // .or_else(|_| panic!("Pretty print failed"))
        }
    }
    */
}
