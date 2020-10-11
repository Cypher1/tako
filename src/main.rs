#![deny(clippy::all)]

use std::env;
use std::error::Error;

use rustyline::error::ReadlineError;
use rustyline::{Editor, config::Config};
use directories::ProjectDirs;

use takolib::cli_options::Options;
use takolib::database::{Compiler, DB};
use takolib::errors::TError;
use takolib::work;

fn main() -> Result<(), TError> {
    let mut db = DB::default();

    let args: Vec<String> = env::args().collect();
    let project_dirs = ProjectDirs::from("systems", "mimir", "tako");
    db.set_project_dirs(project_dirs);

    std::fs::create_dir_all(&db.config_dir()).unwrap();

    db.set_options(Options::new(&args[1..]));

    for f in db.options().files.iter() {
        match work(&mut db, &f, None) {
            Ok(res) => {
                eprintln!("{}", res);
            }
            Err(err) => {
                println!("Error: {}", err);
                println!("Caused by: {}", err.source().unwrap());
            }
        }
    }

    if db.options().interactive {
        repl(&mut db)
    } else {
        Ok(())
    }
}

fn repl(db: &mut DB) -> Result<(), TError> {
    // `()` can be used when no completer is required
    let rl_config = Config::builder()
        .tab_stop(2)
        .build();

    let mut rl = Editor::<()>::with_config(rl_config);
    if rl.load_history(&db.history_file()).is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                println!("Line: {}", line);
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    rl.save_history(&db.history_file()).unwrap();
    Ok(())
}
