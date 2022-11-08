#![deny(clippy::all)]
use crossterm::Result;
use log::{error, trace};
use std::env;

use takolib::cli_options::Options;
use takolib::start;

#[tokio::main]
async fn main() -> Result<()> {
    takolib::ensure_initialized();

    let args: Vec<String> = env::args().collect();
    let options = Options::new(args);
    trace!("Options: {options:#?}");
    if options.files.is_empty() {
        return Ok(());
    }
    let compiler = start(options);
    trace!("Started");
    match compiler.await {
        Ok(()) => {}
        Err(error) => {
            trace!("Internal error: {error:#?}");
            error!("Compiler finished with internal error: {error}");
        }
    }
    Ok(())
}
