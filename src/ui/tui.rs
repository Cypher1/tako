use super::UserInterface;
use crate::{tasks::{TaskStats, TaskKind, StatusReport}, Request, UserAction};
use async_trait::async_trait;
use crossterm::terminal::{disable_raw_mode, enable_raw_mode};
use shutdown_hooks::add_shutdown_hook;
use tokio::sync::mpsc;

extern "C" fn shutdown() {
    let _discard = disable_raw_mode();
}

#[derive(Debug)]
pub struct Tui {}

#[async_trait]
impl UserInterface for Tui {
    async fn launch(
        task_manager_status_receiver: mpsc::UnboundedReceiver<StatusReport>,
        _user_action_receiver: mpsc::UnboundedReceiver<UserAction>,
        _request_sender: mpsc::UnboundedSender<Request>,
    ) {
        add_shutdown_hook(shutdown);
        enable_raw_mode().expect("TUI failed to enable raw mode");
        // TODO: Setup the render tick...
    }
    /*
    fn report_error(&mut self, _error_id: ErrorId, error: &Error) {
        error!("Error: {error:?}");
    }
    fn report_progress(&mut self, progress: Progress) {
        let mut state = self.state.lock().expect("Get state");
        state.progress.push(progress);
        // rerender
        state.rerender = true;
    }
    fn report_job_counts(
        &mut self,
        _num_successful: usize,
        _num_finished: usize,
        _num_total: usize,
    ) {
        let mut state = self.state.lock().expect("Get state");
        state.progress_stats = Some(ProgressStats {
            _num_successful,
            _num_finished,
            _num_total,
        });
        state.rerender = true;
    }
    */
}

/*
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use std::error::Error;
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

    std::fs::create_dir_all(&storage.config_dir())
        .expect("Could not create config directory");
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
    */
