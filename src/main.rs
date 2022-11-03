#![deny(clippy::all)]
use crossterm::Result;
use log::trace;
use std::env;
use tokio;

use takolib::cli_options::Options;
use takolib::compiler_context::CompilerContext;

#[tokio::main]
async fn main() -> Result<()> {
    takolib::ensure_initialized();

    let args: Vec<String> = env::args().collect();
    let options = Options::new(args);
    trace!("Options: {options:?}");
    let compiler = CompilerContext::from_options(options);
    trace!("Compiler empty: {compiler:?}");
    compiler.plan_jobs();
    trace!("Compiler setup: {compiler:?}");
    compiler.run_job_loop().await;
    // trace!("Compiler finished: {compiler:?}");
    Ok(())
}
