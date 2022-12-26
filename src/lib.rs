#![deny(clippy::all)]

#[macro_use]
pub mod utils;

pub mod ast;
pub mod cli_options;
pub mod compiler_context;
pub mod error;
pub mod interpreter;
pub mod keywords;
pub mod literal_values;
pub mod location;
pub mod parser;
pub mod primitives;
pub mod tasks;
pub mod tokens;
pub mod ui;

use crate::cli_options::Options;
use crate::compiler_context::Compiler;
use crate::ui::UserInterface;
use log::error;

static mut LOGS_UNINITIALISED: bool = true;

fn build_logger(finish: impl FnOnce(&mut env_logger::Builder)) {
    if unsafe { LOGS_UNINITIALISED } {
        unsafe {
            LOGS_UNINITIALISED = false;
        }
        finish(
            env_logger::Builder::from_env(
                env_logger::Env::default()
                    .filter_or("RUST_LOG", "info")
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
    Compiler::new()
}
