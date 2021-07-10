#![deny(clippy::all)]

use rustyline::error::ReadlineError;
use rustyline::{config::Config, Editor};
use std::env;
use std::error::Error;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use takolib::cli_options::{print_cli_info, Options};
use takolib::database::DBStorage;
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

            stderr
                .set_color(&ColorSpec::new())
                .expect("Could not set termcolor");
        }
    }
}

fn main() -> Result<(), TError> {
    let mut storage = DBStorage::default();
    {
        let args: Vec<String> = env::args().collect();
        storage.options = Options::new(&args[1..]); // replace options
        std::fs::create_dir_all(&storage.config_dir()).expect("Could not create config directory");
    }

    let files = storage.options.files.clone();

    for f in files.iter() {
        handle(work(&mut storage, f, None));
    }

    use takolib::cli_options::Command;
    if storage.options.cmd == Command::Repl {
        repl(&mut storage)
    } else {
        Ok(())
    }
}

fn repl(storage: &mut DBStorage) -> Result<(), TError> {
    print_cli_info();
    // `()` can be used when no completer is required
    let rl_config = Config::builder().tab_stop(2).build();

    let mut rl = Editor::<()>::with_config(rl_config);
    if let Err(err) = rl.load_history(&storage.history_file()) {
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
                    handle(work_on_string(storage, line, "repl.tk", None));
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
    rl.save_history(&storage.history_file())
        .expect("Could not save history");
    Ok(())
}
