#![deny(clippy::all)]

use log::error;
use rustyline::error::ReadlineError;
use rustyline::{config::Config, Editor};
use std::env;
use std::error::Error;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use takolib::cli_options::{print_cli_info, Command, Options};
use takolib::compiler_context::CompilerContext;
use takolib::error::TError;
use takolib::{work, work_on_string};

fn handle(res: Result<String, TError>) {
    match res {
        Ok(res) => {
            println!("{}", res);
        }
        Err(err) => {
            let mut stderr = StandardStream::stderr(ColorChoice::Auto);
            stderr
                .set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))
                .expect("Could not set termcolor");
            error!("Error: {}", err);
            if let Some(source) = err.source() {
                error!("Caused by: {}", source);
            }

            stderr
                .set_color(&ColorSpec::new())
                .expect("Could not set termcolor");
        }
    }
}

fn main() {
    takolib::ensure_initialized();

    let mut storage = CompilerContext::new();
    {
        let args: Vec<String> = env::args().collect();
        storage.options = Options::new(&args[1..]); // replace options
        std::fs::create_dir_all(&storage.config_dir()).expect("Could not create config directory");
    }

    let files = storage.options.files.clone();

    for f in &files {
        handle(work(&mut storage, f, None));
    }

    if storage.options.cmd == Command::Repl || storage.options.cmd == Command::StackRepl {
        repl(&mut storage);
    }
}

fn repl(storage: &mut CompilerContext) {
    print_cli_info();
    // `()` can be used when no completer is required
    let rl_config = Config::builder().tab_stop(2).build();

    let mut rl = Editor::<()>::with_config(rl_config);
    if let Err(err) = rl.load_history(&storage.history_file()) {
        error!("{:?}", err);
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
                error!("(To exit, press ^C again or type :exit)");
                cmd_was_interrupt = true;
            }
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                error!("Readline Error: {:?}", err);
                break;
            }
        }
        last_cmd_was_interrupt = cmd_was_interrupt;
    }
    rl.save_history(&storage.history_file())
        .expect("Could not save history");
}
