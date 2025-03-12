#![deny(clippy::all)]

pub mod ast;
#[cfg(feature = "backend")]
pub mod codegen;
pub mod compiler;
pub mod desugarer;
pub mod error;
pub mod interpreter;
pub mod lowerer;
pub mod parser;
pub mod primitives;
pub mod tasks;
#[cfg(test)]
pub mod test;
pub mod ui;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

use crate::compiler::Compiler;

static mut LOGS_UNINITIALISED: bool = true;

#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "env_logger")]
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

#[cfg(not(target_arch = "wasm32"))]
#[cfg(test)]
pub fn ensure_initialized() {
    build_logger(|env| {
        let _ = env.is_test(true).try_init();
    });
}

#[cfg(target_arch = "wasm32")]
pub fn ensure_initialized() {
    if unsafe { LOGS_UNINITIALISED } {
        unsafe {
            LOGS_UNINITIALISED = false;
        }
        wasm_logger::init(wasm_logger::Config::new(log::Level::Trace));
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "env_logger")]
#[cfg(not(test))]
pub fn ensure_initialized() {
    use std::fs::OpenOptions;
    build_logger(|env| {
        let log_file = OpenOptions::new()
            .append(true)
            .create(true)
            .open(".tako.log")
            .expect("Failed to setup log file.");
        env_logger::Builder::init(env.target(env_logger::fmt::Target::Pipe(Box::new(log_file))));
    });
    build_logger(env_logger::Builder::init);
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(not(feature = "env_logger"))]
pub fn ensure_initialized() {
    // TODO: Support this?
    unsafe {
        LOGS_UNINITIALISED = false;
    }
}

pub async fn start() -> Compiler {
    Compiler::default()
}
