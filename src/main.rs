#![deny(clippy::all)]
use crossterm::Result;
use log::{debug, error, trace};
use std::env;
use std::sync::{Arc, Mutex};
use tokio::sync::{broadcast, mpsc};

use takolib::cli_options::{Command, Options};
use takolib::launch_ui;
use takolib::start;
use takolib::tasks::Request;

use takolib::ui::{Cli, Tui, Http, UiMode};

#[tokio::main]
async fn main() -> Result<()> {
    takolib::ensure_initialized();

    let args: Vec<String> = env::args().collect();
    let options = Options::new(args);
    debug!("Options: {options:#?}");

    let (task_manager_status_sender, task_manager_status_receiver) = mpsc::unbounded_channel();
    let (request_sender, request_receiver) = mpsc::unbounded_channel();
    let stats_requester = Arc::new(Mutex::new(broadcast::channel(1).0));

    let request_sender = match options.cmd {
        Command::Repl => {
            trace!("main: Waiting for user actions...");
            Some(request_sender)
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
            None
        }
        _ => todo!(),
    };
    let _ui_task = {
        let stats_requester = stats_requester.clone();
        let ui_mode = options.ui_mode;
        tokio::spawn(async move {
            match ui_mode {
                UiMode::Cli => {
                    launch_ui::<Cli>(
                        task_manager_status_receiver,
                        request_sender,
                        stats_requester,
                    )
                    .await
                }
                UiMode::Tui => {
                    launch_ui::<Tui>(
                        task_manager_status_receiver,
                        request_sender,
                        stats_requester,
                    )
                    .await
                }
                UiMode::Http => {
                    launch_ui::<Http>(
                        task_manager_status_receiver,
                        request_sender,
                        stats_requester,
                    )
                    .await
                }
            };
        })
    };
    let mut compiler_task = start(
        task_manager_status_sender,
        request_receiver,
        stats_requester.clone(),
    )
    .await;

    // Receive the results...
    trace!("Waiting for 'final' result...");
    while let Some(ast) = compiler_task.recv().await {
        trace!("Receiving 'final' result from compiler: {ast:?}");
        trace!("AST: {:?}", ast);
    }
    compiler_task.close();
    // All done!
    /*
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
    */
    Ok(())
}
