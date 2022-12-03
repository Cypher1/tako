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
use primitives::Prim;
use tasks::{RequestTask, StatusReport};
use tokio::sync::{broadcast, mpsc};

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
    task_manager_stats: mpsc::UnboundedReceiver<StatusReport>,
    request_sender: mpsc::UnboundedSender<(RequestTask, mpsc::UnboundedSender<Prim>)>,
    stats_requester: broadcast::Sender<()>,
    options: Options,
) {
    <T as UserInterface>::launch(task_manager_stats, request_sender, stats_requester, options)
        .await
        .unwrap_or_else(|err| {
            error!("Error in UI: {}", err);
        });
}

pub async fn start(
    request_sender: mpsc::UnboundedReceiver<(RequestTask, mpsc::UnboundedSender<Prim>)>,
    task_manager_stats: mpsc::UnboundedSender<StatusReport>,
    task_manager_stats_requester: broadcast::Sender<()>,
) -> Compiler {
    Compiler::new(
        request_sender,
        task_manager_stats,
        task_manager_stats_requester,
    )
}
