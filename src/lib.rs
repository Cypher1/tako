#![deny(clippy::all)]

#[macro_use]
pub mod utils;

pub mod ast;
pub mod cli_options;
pub mod compiler_context;
pub mod error;
pub mod interpreter;
pub mod keywords;
pub mod location;
pub mod parser;
pub mod primitives;
pub mod string_interner;
pub mod tasks;
pub mod tokens;
pub mod ui;

use crate::cli_options::Options;
use crate::compiler_context::Compiler;
use crate::ui::UserInterface;
use log::error;
use std::fs::OpenOptions;

static mut LOGS_UNINITIALISED: bool = true;

fn build_logger(finish: impl FnOnce(&mut env_logger::Builder)) {
    if unsafe { LOGS_UNINITIALISED } {
        unsafe {
            LOGS_UNINITIALISED = false;
        }
        let log_file = OpenOptions::new()
            .write(true)
            .append(true)
            .open(".tako.log")
            .expect("Failed to setup log file.");
        finish(
            env_logger::Builder::from_env(
                env_logger::Env::default()
                    .filter_or("RUST_LOG", "debug")
                    .write_style_or("RUST_LOG_STYLE", "AUTO"),
            )
            .target(env_logger::fmt::Target::Pipe(Box::new(log_file)))
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

pub async fn launch_ui<
    Out: Send + std::fmt::Debug + std::fmt::Display,
    T: UserInterface + Send + 'static,
>(
    compiler: &Compiler,
    options: Options,
) -> T {
    <T as UserInterface>::launch(compiler, options)
        .await
        .unwrap_or_else(|err| {
            error!("Error in UI: {}", err);
            std::process::exit(1);
        })
}

pub async fn start() -> Compiler {
    Compiler::default()
}
