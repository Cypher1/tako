#![deny(clippy::all)]
use crossterm::Result;
use log::{debug, error, trace};
use std::env;
use std::sync::{Arc, Mutex};
use tokio::sync::{broadcast, mpsc};

use takolib::cli_options::{Command, Options};
use takolib::launch_ui;
use takolib::start;
use takolib::tasks::RequestTask;

use takolib::ui::{Http, Tui, UiMode};

type Output = takolib::primitives::Prim;

#[tokio::main]
async fn main() -> Result<()> {
    takolib::ensure_initialized();

    let args: Vec<String> = env::args().collect();
    let options = Options::new(args);
    debug!("Options: {options:#?}");

    let (task_manager_status_sender, task_manager_status_receiver) = mpsc::unbounded_channel();
    let (request_sender, request_receiver) = mpsc::unbounded_channel();
    let stats_requester = Arc::new(Mutex::new(broadcast::channel(1).0));
    let compiler_task = start(
        task_manager_status_sender,
        request_receiver,
        stats_requester.clone(),
    )
    .await;
    let ui_task = {
        let options = options.clone();
        let request_sender = match options.cmd {
            Command::Repl => {
                trace!("main: Waiting for user actions...");
                Some(request_sender.clone())
            }
            _ => None,
        };
        let ui_mode = options.ui_mode;
        tokio::spawn(async move {
            match ui_mode {
                UiMode::Tui => {
                    launch_ui::<Output, Tui<Output>>(
                        task_manager_status_receiver,
                        request_sender,
                        compiler_task,
                        stats_requester,
                        options,
                    )
                    .await
                }
                UiMode::Http => {
                    launch_ui::<Output, Http>(
                        task_manager_status_receiver,
                        request_sender,
                        compiler_task,
                        stats_requester,
                        options,
                    )
                    .await
                }
            };
        })
    };
    if !options.files.is_empty() {
        request_sender
            .send(RequestTask::Launch {
                files: options.files.clone(),
            })
            .unwrap_or_else(|err| {
                error!("Compiler task has ended: {}", err);
                std::process::exit(1);
            });
    }
    ui_task.await.unwrap_or_else(|err| {
        trace!("Internal error: {err:#?}");
        error!("Compiler interface finished with internal error: {err}");
        std::process::exit(1);
    });
    Ok(())
}
