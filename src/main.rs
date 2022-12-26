#![deny(clippy::all)]

use log::{debug, error, trace};
use takolib::{
    start,
    ui::{Http, Tui, UiMode, UserInterface},
};

type Output = takolib::primitives::Prim;

#[tokio::main]
async fn main() -> std::io::Result<()> {
    takolib::ensure_initialized();

    let args: Vec<String> = std::env::args().collect();
    let options = takolib::cli_options::Options::new(args);
    debug!("Options: {options:#?}");

    let compiler = start().await;
    let ui_task = match options.ui_mode {
        UiMode::Tui => {
            let ui = takolib::launch_ui::<Output, Tui>(&compiler, options).await;
            tokio::spawn(async move { ui.run_loop().await })
        }
        UiMode::Http => {
            let ui = takolib::launch_ui::<Output, Http>(&compiler, options).await;
            tokio::spawn(async move { ui.run_loop().await })
        }
    };
    tokio::spawn(async move { compiler.run_loop().await });
    ui_task.await.unwrap_or_else(|err| {
        trace!("Internal error: {err:#?}");
        error!("Compiler interface finished with internal error: {err}");
        std::process::exit(1);
    })
}
