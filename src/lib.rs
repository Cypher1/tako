#![deny(clippy::all)]

#[macro_use]
pub mod free_standing;

pub mod ast;
pub mod cli_options;
pub mod error;
pub mod keywords;
pub mod location;
pub mod parser;
pub mod primitives;
pub mod string_interner;
pub mod tasks;
pub mod tokens;
pub mod ui;

use crate::cli_options::Options;
use crate::error::TError;
use crate::tasks::{LaunchTask, TaskSet};
use crate::ui::UserInterface;
use std::sync::{Arc, Mutex};
use tokio::sync::mpsc;
use log::trace;

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

fn make_ui_arc<T: UserInterface + Send + 'static>(
    value: T,
) -> Arc<Mutex<dyn UserInterface + Send>> {
    Arc::new(Mutex::new(value))
}

pub async fn start(options: Options) -> Result<(), TError> {
    use crate::ui::{UiMode, CLI, TUI};
    let _ui = match options.ui_mode {
        UiMode::Cli => make_ui_arc(CLI::new()),
        UiMode::Tui => make_ui_arc(TUI::new()),
        UiMode::TuiIfAvailable => {
            if false {
                make_ui_arc(CLI::new())
            } else {
                make_ui_arc(TUI::new())
            }
        }
    };
    let files = options.files.clone();
    let options = Arc::new(Mutex::new(options));

    let mut result_receiver = {
        let (request_sender, request_receiver) = mpsc::unbounded_channel();
        let (result_sender, result_receiver) = mpsc::unbounded_channel();

        let store = TaskSet::new(request_receiver, result_sender, options.clone()); // Setup!
        store.launch().await; // launches all the jobs.
        request_sender.send(LaunchTask {
            files,
        }).expect("Should be able to send launch task"); // Launch the cli task.
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
