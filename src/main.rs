#![deny(clippy::all)]
use crossterm::Result;
use log::{debug, error, trace};
use std::env;
use takolib::ui::{Http, Tui, UiMode};
use tokio::sync::{broadcast, mpsc};

type Output = takolib::primitives::Prim;

#[tokio::main]
async fn main() -> Result<()> {
    takolib::ensure_initialized();

    let args: Vec<String> = env::args().collect();
    let options = takolib::cli_options::Options::new(args);
    debug!("Options: {options:#?}");

    let (task_manager_status_sender, task_manager_status_receiver) = mpsc::unbounded_channel();
    let (stats_requester, _) = broadcast::channel(1);
    let compiler = takolib::start(task_manager_status_sender, stats_requester.clone()).await;
    let ui_task = {
        let options = options.clone();
        let ui_mode = options.ui_mode;
        tokio::spawn(async move {
            match ui_mode {
                UiMode::Tui => {
                    takolib::launch_ui::<Output, Tui>(
                        task_manager_status_receiver,
                        compiler,
                        stats_requester,
                        options,
                    )
                    .await
                }
                UiMode::Http => {
                    takolib::launch_ui::<Output, Http>(
                        task_manager_status_receiver,
                        compiler,
                        stats_requester,
                        options,
                    )
                    .await
                }
            };
        })
    };
    ui_task.await.unwrap_or_else(|err| {
        trace!("Internal error: {err:#?}");
        error!("Compiler interface finished with internal error: {err}");
        std::process::exit(1);
    });
    Ok(())
}
