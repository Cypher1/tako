#![deny(clippy::all)]
use crossterm::Result;
use log::{error, trace};
use std::env;
use tokio::sync::mpsc;

use takolib::cli_options::Options;
use takolib::start;
use takolib::tasks::Request;
use takolib::ui::UserAction;

#[tokio::main]
async fn main() -> Result<()> {
    takolib::ensure_initialized();

    let args: Vec<String> = env::args().collect();
    let options = Options::new(args);
    trace!("Options: {options:#?}");
    if options.files.is_empty() {
        return Ok(());
    }
    let (status_report_sender, status_report_receiver) = mpsc::unbounded_channel();
    let (user_action_sender, user_action_receiver) = mpsc::unbounded_channel();
    let (request_sender, request_receiver) = mpsc::unbounded_channel();
    use takolib::launch_ui;
    use takolib::ui::{Cli, Tui, UiMode};
    let _ui = match options.ui_mode {
        UiMode::Cli => launch_ui(
            Cli::new(),
            status_report_receiver,
            user_action_receiver,
            request_sender.clone(),
        ),
        UiMode::Tui => launch_ui(
            Tui::new(),
            status_report_receiver,
            user_action_receiver,
            request_sender.clone(),
        ),
    };
    let compiler = start(status_report_sender, request_receiver);

    request_sender
        .send(Request::Launch {
            files: options.files,
        })
        .expect("Should be able to send launch task"); // Launch the cli task.
                                                       //
    trace!("Started");

    tokio::spawn(async move {
        trace!("main: Waiting for user actions...");
        user_action_sender
            .send(UserAction::Something)
            .expect("Should be able to send user action to ui");
    });

    match compiler.await {
        Ok(()) => {}
        Err(error) => {
            trace!("Internal error: {error:#?}");
            error!("Compiler finished with internal error: {error}");
        }
    }
    Ok(())
}
