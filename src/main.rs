#![deny(clippy::all)]
use crossterm::Result;
use log::{error, trace};
use std::env;
use tokio::sync::mpsc;

use takolib::cli_options::{Options, Command};
use takolib::start;
use takolib::tasks::Request;
use takolib::ui::UserAction;

#[tokio::main]
async fn main() -> Result<()> {
    takolib::ensure_initialized();

    let args: Vec<String> = env::args().collect();
    let options = Options::new(args);
    trace!("Options: {options:#?}");
    let (task_manager_registration_sender, task_manager_registration_receiver) = mpsc::unbounded_channel();
    let (user_action_sender, user_action_receiver) = mpsc::unbounded_channel();
    let (request_sender, request_receiver) = mpsc::unbounded_channel();
    use takolib::launch_ui;
    use takolib::ui::{Cli, Tui, UiMode};
    match options.ui_mode {
        UiMode::Cli => launch_ui::<Cli>(
            task_manager_registration_receiver,
            user_action_receiver,
            request_sender.clone(),
        ).await,
        UiMode::Tui => launch_ui::<Tui>(
            task_manager_registration_receiver,
            user_action_receiver,
            request_sender.clone(),
        ).await,
    };
    let compiler = start(task_manager_registration_sender, request_receiver);

    // Launch the cli task.
    request_sender.send(Request::Launch { files: options.files })
        .unwrap_or_else(|err| {
        error!("Compiler task has ended: {}", err);
        std::process::exit(1);
    });
    trace!("Started");

    if options.cmd == Command::Repl {
        tokio::spawn(async move {
            trace!("main: Waiting for user actions...");
            user_action_sender.send(UserAction::Something).unwrap_or_else(|err|{
                error!("Ui task has ended: {}", err);
            });
        });
    }

    compiler.await.unwrap_or_else(|err| {
        trace!("Internal error: {err:#?}");
        error!("Compiler finished with internal error: {err}");
        std::process::exit(1);
    });
    Ok(())
}
