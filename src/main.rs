#![deny(clippy::all)]

use std::env;
use std::error::Error;

use directories::ProjectDirs;
use rustyline::error::ReadlineError;
use rustyline::{config::Config, Editor};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use takolib::cli_options::{print_cli_info, Options};
use takolib::database::{Compiler, DB};
use takolib::errors::TError;
use takolib::{work, work_on_string};

fn handle(res: Result<String, TError>) {
    match res {
        Ok(res) => {
            eprintln!("{}", res);
        }
        Err(err) => {
            let mut stderr = StandardStream::stderr(ColorChoice::Auto);
            stderr
                .set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))
                .expect("Could not set termcolor");
            eprintln!("Error: {}", err);
            if let Some(source) = err.source() {
                eprintln!("Caused by: {}", source);
            }

            stderr.set_color(&ColorSpec::new()).expect("Could not set termcolor");
        }
    }
}

fn main() -> Result<(), TError> {
    let mut db = DB::default();

    let args: Vec<String> = env::args().collect();
    let project_dirs = ProjectDirs::from("systems", "mimir", "tako");
    db.set_project_dirs(project_dirs);

    std::fs::create_dir_all(&db.config_dir()).expect("Could not create config directory");

    db.set_options(Options::new(&args[1..]));

    for f in db.options().files.iter() {
        handle(work(&mut db, &f, None));
    }

    use takolib::cli_options::Command;
    if db.options().cmd == Command::Repl {
        repl(&mut db)
    } else {
        Ok(())
    }
}

fn repl(db: &mut DB) -> Result<(), TError> {
    print_cli_info();
    // `()` can be used when no completer is required
    let rl_config = Config::builder().tab_stop(2).build();

    let mut rl = Editor::<()>::with_config(rl_config);
    if let Err(err) = rl.load_history(&db.history_file()) {
        eprintln!("{:?}", err);
    }
    let mut last_cmd_was_interrupt = false;
    loop {
        let readline = rl.readline("> ");
        let mut cmd_was_interrupt = false;
        match readline {
            Ok(line) => {
                if !line.is_empty() {
                    if line == ":exit" {
                        break;
                    }
                    rl.add_history_entry(line.as_str());
                    handle(work_on_string(db, line, "repl.tk", None));
                }
            }
            Err(ReadlineError::Interrupted) => {
                if last_cmd_was_interrupt {
                    break;
                }
                eprintln!("(To exit, press ^C again or type :exit)");
                cmd_was_interrupt = true;
            }
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("Readline Error: {:?}", err);
                break;
            }
        }
        last_cmd_was_interrupt = cmd_was_interrupt;
    }
    rl.save_history(&db.history_file()).expect("Could not save history");
    Ok(())
}
