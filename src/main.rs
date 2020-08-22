#![deny(clippy::all)]

use std::env;
use std::error::Error;

use takolib::cli_options::Options;
use takolib::database::{Compiler, DB};
use takolib::errors::TError;
use takolib::work;

fn main() -> Result<(), TError> {
    let args: Vec<String> = env::args().collect();

    let mut db = DB::default();
    db.set_options(Options::new(&args[1..]));

    for f in db.options().files.iter() {
        match work(&mut db, &f, None) {
            Ok(res) => {eprintln!("{}", res);}
            Err(err) => {
                println!("Error: {}", err);
                println!("Caused by: {}", err.source().unwrap());
            }
        }
    }
    Ok(())
}
