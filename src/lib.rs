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

use std::sync::{Arc, Mutex};

use crate::ast::Ast;

use crate::tasks::{Request, TaskSet};
use crate::ui::{UserAction, UserInterface};

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

pub async fn launch_ui<T: UserInterface + Send + 'static>(
    task_manager_stats: mpsc::UnboundedReceiver<StatusReport>,
    user_action_receiver: mpsc::UnboundedReceiver<UserAction>,
    request_sender: Option<mpsc::UnboundedSender<Request>>,
    stats_requester: Arc<Mutex<broadcast::Sender<()>>>,
) {
    <T as UserInterface>::launch(
        task_manager_stats,
        user_action_receiver,
        request_sender,
        stats_requester,
    )
    .await;
}

pub async fn start(
    task_manager_stats: mpsc::UnboundedSender<StatusReport>,
    request_receiver: mpsc::UnboundedReceiver<Request>,
    task_manager_stats_requester: Arc<Mutex<broadcast::Sender<()>>>,
) -> mpsc::UnboundedReceiver<Ast> {
    let (result_sender, result_receiver) = mpsc::unbounded_channel();

    let store = {
        let task_manager_stats_requester = task_manager_stats_requester.lock().expect("TODO");
        TaskSet::new(
            request_receiver,
            result_sender,
            task_manager_stats,
            &*task_manager_stats_requester,
        )
    }; // Setup!
    store.launch().await; // launches all the jobs.
    result_receiver
}
