#![deny(clippy::all)]

mod cli_options;
mod ui;

use cli_options::{Options, VERSION};
use log::{debug, error, trace};
use tako::{
    start,
    ui::{Client, OptionsTrait, UserInterface},
};
use tokio::sync::{mpsc, oneshot};
use ui::{Http, Mode, Tui};

type Output = tako::primitives::Prim;

pub async fn launch_ui<
    Out: Send + std::fmt::Debug + std::fmt::Display,
    T: UserInterface<Options> + Send + 'static,
>(
    client_launch_request_sender: mpsc::UnboundedSender<(
        oneshot::Sender<Client>,
        Box<dyn OptionsTrait>,
    )>,
    options: Options,
) -> T {
    <T as UserInterface<Options>>::launch(client_launch_request_sender, options)
        .await
        .unwrap_or_else(|err| {
            error!("Error in UI: {err}");
            std::process::exit(1);
        })
}

/*
impl Task for WatchFileTask {
    type Output = LoadFileTask;
    const TASK_KIND: TaskKind = TaskKind::WatchFile;

    fn has_file_path(&self) -> Option<&PathBuf> {
        Some(&self.path)
    }
    async fn perform(self, result_sender: UpdateSenderFor<Self>) {
        let (tx, mut rx) = mpsc::unbounded_channel();
        let mut watcher = {
            notify::recommended_watcher(
                move |res: Result<notify::Event, notify::Error>| match res {
                    Ok(event) => {
                        trace!("event: {:?}", event);
                        for path in event.paths {
                            tx.send(path).expect("File watcher terminated");
                        }
                    }
                    Err(e) => {
                        trace!("watch error: {e:?}");
                    }
                },
            )
            .expect("Watcher failed to register")
        };
        trace!("Waiting on file changes...");
        notify::watcher
            .watch(&self.path, notify::RecursiveMode::Recursive)
            .expect("Should be able to watch files");

        while let Some(path) = rx.recv().await {
            // This avoids dropping the watcher.
            result_sender
                .send((
                    self.clone(),
                    Update::NextResult(LoadFileTask {
                        path,
                        invalidate: Meta(true),
                    }),
                ))
                .expect("Load file task could not be sent");
        }
    }
}
*/

#[tokio::main]
async fn main() -> std::io::Result<()> {
    tako::ensure_initialized();
    trace!("\n>>>>>>>>>>>>>>>>>>>>\nStarting takoc {VERSION}\n>>>>>>>>>>>>>>>>>>>>\n");

    let args: Vec<String> = std::env::args().collect();
    let options = crate::cli_options::Options::new(args);
    debug!("Options: {options:#?}");

    let compiler = start().await;
    let client_launch_request_sender = compiler.client_launch_request_sender.clone();
    tokio::spawn(async move { compiler.run_loop().await });

    let ui_task = match options.ui_mode {
        Mode::Tui => {
            let ui = launch_ui::<Output, Tui>(client_launch_request_sender, options).await;
            tokio::spawn(async move { ui.run_loop().await })
        }
        Mode::Http => {
            let ui = launch_ui::<Output, Http>(client_launch_request_sender, options).await;
            tokio::spawn(async move { ui.run_loop().await })
        }
    };
    ui_task.await.unwrap_or_else(|err| {
        trace!("Internal error: {err:#?}");
        error!("Compiler interface finished with internal error: {err}");
        std::process::exit(1);
    })
}
