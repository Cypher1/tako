use std::collections::HashMap;

use super::UserInterface;
use crate::{tasks::StatusReport, Request, UserAction};
use async_trait::async_trait;
use std::sync::{Arc, Mutex};

use tokio::time;
const TICK: Duration = Duration::from_millis(1000);
use crossterm::{
    cursor::{MoveTo, RestorePosition, SavePosition},
    event::{self, Event, KeyEvent},
    style::{Color, Print, ResetColor, SetBackgroundColor, SetForegroundColor},
    terminal::{disable_raw_mode, enable_raw_mode, size, Clear, ClearType},
    QueueableCommand, Result,
};
use shutdown_hooks::add_shutdown_hook;

use crokey::{key, KeyEventFormat};
use std::{
    io::{stdout, Write},
    time::Duration,
};
use tokio::{
    self,
    sync::{
        broadcast,
        mpsc::{self, error::TryRecvError},
    },
};

extern "C" fn shutdown() {
    let _discard = disable_raw_mode();
}

#[derive(Debug)]
pub struct Tui {}

#[async_trait]
impl UserInterface for Tui {
    async fn launch(
        mut task_manager_status_receiver: mpsc::UnboundedReceiver<StatusReport>,
        mut user_action_receiver: mpsc::UnboundedReceiver<UserAction>,
        // User control of the compiler
        _request_sender: Option<mpsc::UnboundedSender<Request>>,
        stats_requester: Arc<Mutex<broadcast::Sender<()>>>,
    ) -> std::io::Result<()> {
        add_shutdown_hook(shutdown);
        enable_raw_mode().expect("TUI failed to enable raw mode");
        let mut manager_status = HashMap::new();
        let mut ticker = time::interval(TICK);

        let (tx_event, mut rx_event) = mpsc::channel(100);

        tokio::spawn(async move {
            loop {
                let event = event::read().expect("User event read should not fail");
                if tx_event.send(event).await.is_err() {
                    panic!("receiver dropped")
                }
            }
        });

        let key_fmt = KeyEventFormat::default();
        let mut should_exit = false;
        loop {
            tokio::select! {
                Some(StatusReport { kind, stats }) = task_manager_status_receiver.recv() => {
                    eprintln!("TaskManager stats: {kind:?} => {stats:?}");
                    manager_status.insert(kind, stats);
                },
                Some(action) = user_action_receiver.recv() => {
                    eprintln!("User action: {action:?}");
                },
                _ = ticker.tick() => {
                    let stats_requester = stats_requester.lock().expect("stats requester lock");
                    stats_requester.send(()).expect("TODO");

                    let mut chars = "".to_string();
                    loop {
                        match rx_event.try_recv() {
                            Ok(Event::Key(key_event)) => {
                                match key_event {
                                    key!(ctrl-c) | key!(ctrl-q) => should_exit = true,
                                    _ => {}
                                };
                                chars = format!("{}{}", chars, key_fmt.to_string(key_event));
                            }
                            Ok(other) => eprint!("Event: {:?}", other),
                            Err(TryRecvError::Empty) => break,
                            Err(TryRecvError::Disconnected) => panic!("Key event sender disconnected!?"),
                        }
                    }
                    eprintln!("Chars: {}", chars);
                    if should_exit {
                        stdout().queue(Clear(ClearType::All))?;
                        stdout().flush()?;
                        std::process::exit(0)
                    }
                }
                else => break,
            }
        }
        Ok(())
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
