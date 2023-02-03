#![deny(clippy::all)]

#[macro_use]
pub mod utils;

pub mod ast;
pub mod compiler;
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

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

use crate::compiler::Compiler;

static mut LOGS_UNINITIALISED: bool = true;

fn build_logger(finish: impl FnOnce(&mut env_logger::Builder)) {
    if unsafe { LOGS_UNINITIALISED } {
        unsafe {
            LOGS_UNINITIALISED = false;
        }
        finish(
            env_logger::Builder::from_env(
                env_logger::Env::default()
                    .filter_or("RUST_LOG", "debug")
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

#[cfg(target_arch = "wasm32")]
pub fn ensure_initialized() {
    build_logger(|env| {
        let _ = env.try_init();
    });
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(not(test))]
pub fn ensure_initialized() {
    use std::fs::OpenOptions;
    build_logger(|env| {
        let log_file = OpenOptions::new()
            .write(true)
            .append(true)
            .create(true)
            .open(".tako.log")
            .expect("Failed to setup log file.");
        env_logger::Builder::init(env.target(env_logger::fmt::Target::Pipe(Box::new(log_file))))
    });
    build_logger(env_logger::Builder::init);
}

pub async fn start() -> Compiler {
    Compiler::default()
}
