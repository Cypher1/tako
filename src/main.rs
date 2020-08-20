#![deny(clippy::all)]

use std::env;

use takolib::cli_options::Options;
use takolib::database::{Compiler, DB};
use takolib::errors::TError;
use takolib::work;

fn main() -> Result<(), TError> {
    let args: Vec<String> = env::args().collect();

    let mut db = DB::default();
    db.set_options(Options::new(&args[1..]));

    for f in db.options().files.iter() {
        eprintln!("{}", work(&mut db, &f, None)?);
    }
    Ok(())
}
