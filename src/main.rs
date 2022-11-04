#![deny(clippy::all)]
use crossterm::Result;
use log::trace;
use std::env;

use takolib::cli_options::Options;
use takolib::compiler_context::Scheduler;

#[tokio::main]
async fn main() -> Result<()> {
    takolib::ensure_initialized();

    let args: Vec<String> = env::args().collect();
    let options = Options::new(args);
    trace!("Options: {options:#?}");
    let compiler = Scheduler::from_options(options);
    trace!("started: {compiler:#?}");
    compiler.run_job_loop().await;
    trace!("finished: {compiler:#?}");
    Ok(())
}
