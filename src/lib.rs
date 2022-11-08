#![deny(clippy::all)]

#[macro_use]
pub mod utils;

pub mod ast;
pub mod cli_options;
pub mod error;
pub mod keywords;
pub mod location;
pub mod parser;
pub mod primitives;
pub mod tasks;
pub mod tokens;
pub mod ui;

use crate::error::TError;
use crate::tasks::{Request, StatusReport, TaskSet};
use crate::ui::{UserAction, UserInterface};
use log::trace;
use tasks::TaskManagerRegistration;
use std::sync::{Arc, Mutex};
use tokio::sync::mpsc;

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

pub fn launch_ui<T: UserInterface + Send + 'static>(
    mut value: T,
    task_manager_registration: mpsc::UnboundedReceiver<TaskManagerRegistration>,
    user_action_receiver: mpsc::UnboundedReceiver<UserAction>,
    request_sender: mpsc::UnboundedSender<Request>,
) -> Arc<Mutex<dyn UserInterface + Send>> {
    value.launch(task_manager_registration, user_action_receiver, request_sender);
    Arc::new(Mutex::new(value))
}

pub async fn start(
    task_manager_registration: mpsc::UnboundedSender<TaskManagerRegistration>,
    request_receiver: mpsc::UnboundedReceiver<Request>,
) -> Result<(), TError> {
    let mut result_receiver = {
        let (result_sender, result_receiver) = mpsc::unbounded_channel();

        let store = TaskSet::new(request_receiver, result_sender, task_manager_registration); // Setup!
        store.launch().await; // launches all the jobs.
        result_receiver
    };
    // Receive the results...
    trace!("Waiting for 'final' result...");
    while let Some(ast) = result_receiver.recv().await {
        trace!("Receiving 'final' result from compiler: {ast:?}");
        dbg!(ast);
    }
    // All done!
    Ok(())
}
