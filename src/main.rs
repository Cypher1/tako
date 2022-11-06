#![deny(clippy::all)]
use crossterm::Result;
use log::{trace, error};
use std::env;

use takolib::cli_options::Options;
use takolib::scheduler::Scheduler;

#[tokio::main]
async fn main() -> Result<()> {
    takolib::ensure_initialized();

    let args: Vec<String> = env::args().collect();
    let options = Options::new(args);
    trace!("Options: {options:#?}");
    if options.files.is_empty() {
        return Ok(());
    }
    let compiler = Scheduler::from_options(options);
    trace!("started: {compiler:#?}");
    match compiler.run().await {
        Ok(()) => {},
        Err(error) => {
            error!("Internal error: {error:?}");
            eprintln!("Compiler finished with internal error: {error}");
        }
    }
    trace!("finished: {compiler:#?}");
    Ok(())
}
