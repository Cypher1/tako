#![deny(clippy::all)]

use log::{debug, error, trace};
use takolib::{ui::{Http, Tui, UiMode}, compiler_context::Compiler};

type Output = takolib::primitives::Prim;

#[tokio::main]
async fn main() {
    takolib::ensure_initialized();

    let args: Vec<String> = std::env::args().collect();
    let options = takolib::cli_options::Options::new(args);
    debug!("Options: {options:#?}");

    let compiler = Compiler::new();
    let ui_task = match options.ui_mode {
        UiMode::Tui => {
            let ui = takolib::launch_ui::<Output, Tui>(
                &compiler,
                options,
            )
            .await;
            tokio::spawn(async move {
                ui.run_loop()
            });
        }
        UiMode::Http => {
            let ui = takolib::launch_ui::<Output, Http>(
                &compiler,
                options,
            )
            .await;
            tokio::spawn(async move {
                ui.run_loop()
            });
        }
    };
    tokio::spawn(async move {
        compiler.run_loop()
    });
    ui_task.await.unwrap_or_else(|err| {
        trace!("Internal error: {err:#?}");
        error!("Compiler interface finished with internal error: {err}");
        std::process::exit(1);
    });
}
