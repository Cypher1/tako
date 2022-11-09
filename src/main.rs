#![deny(clippy::all)]
use crossterm::Result;
use log::{debug, error, trace};
use std::env;
use tokio::sync::mpsc;

use takolib::cli_options::{Command, Options};
use takolib::launch_ui;
use takolib::start;
use takolib::tasks::Request;
use takolib::ui::UserAction;
use takolib::ui::{Cli, Tui, UiMode};

#[tokio::main]
async fn main() -> Result<()> {
    takolib::ensure_initialized();

    let args: Vec<String> = env::args().collect();
    let options = Options::new(args);
    debug!("Options: {options:#?}");

    let (task_manager_registration_sender, task_manager_registration_receiver) =
        mpsc::unbounded_channel();
    let (user_action_sender, user_action_receiver) = mpsc::unbounded_channel();
    let (request_sender, request_receiver) = mpsc::unbounded_channel();

    match options.cmd {
        Command::Repl => {
            trace!("main: Waiting for user actions...");
            user_action_sender
                .send(UserAction::Something)
                .unwrap_or_else(|err| {
                    error!("Ui task has ended: {}", err);
                });
        }
        Command::Build => {
            request_sender
                .send(Request::Launch {
                    files: options.files,
                })
                .unwrap_or_else(|err| {
                    error!("Compiler task has ended: {}", err);
                    std::process::exit(1);
                });
        }
        _ => todo!(),
    }
    let ui_task = {
        let ui_mode = options.ui_mode;
        tokio::spawn(async move {
            match ui_mode {
                UiMode::Cli => {
                    launch_ui::<Cli>(
                        task_manager_registration_receiver,
                        user_action_receiver,
                        request_sender,
                    )
                    .await
                }
                UiMode::Tui => {
                    launch_ui::<Tui>(
                        task_manager_registration_receiver,
                        user_action_receiver,
                        request_sender,
                    )
                    .await
                }
            };
        })
    };

    let compiler_task = tokio::spawn(async move {
        let compiler = start(task_manager_registration_sender, request_receiver);
        compiler.await.unwrap_or_else(|err| {
            trace!("Internal error: {err:#?}");
            error!("Compiler finished with internal error: {err}");
            std::process::exit(1);
        });
    });

    // Launch the cli task.
    trace!("Started");
    compiler_task
        .await
        .unwrap_or_else(|err| error!("Compiler finished with internal error: {err}"));
    ui_task
        .await
        .unwrap_or_else(|err| error!("Ui task finished with internal error: {err}"));
    Ok(())
}
