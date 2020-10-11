#![deny(clippy::all)]

use std::env;
use std::error::Error;

use rustyline::error::ReadlineError;
use rustyline::{Editor, config::Config};
use directories::ProjectDirs;

use takolib::cli_options::{Options, print_cli_info};
use takolib::database::{Compiler, DB};
use takolib::errors::TError;
use takolib::{work, work_on_string};

fn handle(res: Result<String, TError>) {
    match res {
        Ok(res) => {
            eprintln!("{}", res);
        }
        Err(err) => {
            println!("Error: {}", err);
            println!("Caused by: {}", err.source().unwrap());
        }
    }
}

fn main() -> Result<(), TError> {
    let mut db = DB::default();

    let args: Vec<String> = env::args().collect();
    let project_dirs = ProjectDirs::from("systems", "mimir", "tako");
    db.set_project_dirs(project_dirs);

    std::fs::create_dir_all(&db.config_dir()).unwrap();

    db.set_options(Options::new(&args[1..]));

    for f in db.options().files.iter() {
        handle(work(&mut db, &f, None));
    }

    if db.options().interactive {
        repl(&mut db)
    } else {
        Ok(())
    }
}

fn repl(db: &mut DB) -> Result<(), TError> {
    print_cli_info();
    // `()` can be used when no completer is required
    let rl_config = Config::builder()
        .tab_stop(2)
        .build();

    let mut rl = Editor::<()>::with_config(rl_config);
    if rl.load_history(&db.history_file()).is_err() {
        println!("No previous history.");
    }
    let mut last_cmd_was_interrupt = false;
    loop {
        let readline = rl.readline("> ");
        let mut cmd_was_interrupt = false;
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                handle(work_on_string(db, line, "repl", None));
            },
            Err(ReadlineError::Interrupted) => {
                if last_cmd_was_interrupt {
                    break;
                }
                println!("(To exit, press ^C again or type .exit)");
                cmd_was_interrupt = true;
            },
            Err(ReadlineError::Eof) => {
                break
            },
            Err(err) => {
                println!("Readline Error: {:?}", err);
                break
            }
        }
        last_cmd_was_interrupt = cmd_was_interrupt;
    }
    rl.save_history(&db.history_file()).unwrap();
    Ok(())
}
