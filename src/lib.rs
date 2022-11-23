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
use crate::tasks::RequestTask;
use crate::ui::UserInterface;
use log::error;
use primitives::Prim;
use tasks::StatusReport;
use tokio::sync::{broadcast, mpsc};

static mut LOGS_UNINITIALISED: bool = true;

pub fn build_logger(finish: impl FnOnce(&mut env_logger::Builder)) {
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
    T: UserInterface<Out> + Send + 'static,
>(
    task_manager_stats: mpsc::UnboundedReceiver<StatusReport>,
    request_sender: Option<mpsc::UnboundedSender<RequestTask>>,
    response_getter: mpsc::UnboundedReceiver<Out>,
    stats_requester: broadcast::Sender<()>,
    options: Options,
) {
    <T as UserInterface<Out>>::launch(
        task_manager_stats,
        request_sender,
        response_getter,
        stats_requester,
        options,
    )
    .await
    .unwrap_or_else(|err| {
        error!("Error in UI: {}", err);
    });
}

pub async fn start(
    task_manager_stats: mpsc::UnboundedSender<StatusReport>,
    request_receiver: mpsc::UnboundedReceiver<RequestTask>,
    task_manager_stats_requester: broadcast::Sender<()>,
) -> mpsc::UnboundedReceiver<Prim> {
    let (result_sender, result_receiver) = mpsc::unbounded_channel();
    let compiler = Compiler::new(
        Some(request_receiver),
        result_sender,
        task_manager_stats,
        task_manager_stats_requester,
    );
    compiler.launch().await; // launches all the jobs.
    result_receiver
}
