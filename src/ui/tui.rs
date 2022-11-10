use log::trace;
use std::collections::BTreeMap;

use super::UserInterface;
use crate::{
    tasks::{StatusReport, TaskKind, TaskStats},
    Request,
};
use async_trait::async_trait;
use std::sync::{Arc, Mutex};

use crossterm::{
    cursor::MoveTo,
    event::{Event, EventStream, KeyCode, KeyEvent},
    style::{Color, Print, ResetColor, SetBackgroundColor, SetForegroundColor},
    terminal::{disable_raw_mode, enable_raw_mode, size, Clear, ClearType},
    QueueableCommand,
};
use futures::{future::FutureExt, StreamExt};
use shutdown_hooks::add_shutdown_hook;
use tokio::time;

use crokey::{key, KeyEventFormat};
use std::{
    io::{stdout, Write},
    time::{Duration, Instant},
};
use tokio::{
    self,
    sync::{
        broadcast,
        mpsc::{self},
    },
};

const TICK: Duration = Duration::from_millis(100);

extern "C" fn shutdown() {
    let _discard = disable_raw_mode();
}

#[derive(Debug, Default)]
pub struct Tui {
    manager_status: BTreeMap<TaskKind, TaskStats>,
    key_fmt: KeyEventFormat,
    should_exit: bool,
    history: Vec<String>,
    input: String,
    input_after_cursor: String,
    characters: String,
}

impl Tui {
    fn render(&self) -> std::io::Result<()> {
        stdout().queue(Clear(ClearType::All))?;
        let (cols, rows) = size()?;

        let mut row = 0;
        stdout()
            .queue(MoveTo(0, row))?
            .queue(Print(&"Stats".to_string()))?;
        row += 1;
        for (task_kind, stats) in &self.manager_status {
            let s = format!("{:?}: {}", task_kind, stats);
            let len = s.len() as u16;
            stdout()
                .queue(MoveTo(cols - len - 1, row))?
                .queue(Print(&s))?;
            row += 1;
        }

        let count_lines = |s: &str| {
            s.chars().filter(|c| *c == '\n').count() as u16
        };
        let mut content = "".to_string();
        for hist_entry in &self.history {
            content += hist_entry;
            content += "\n";
        }
        content += &format!(">> {}{}", self.input, self.input_after_cursor);
        let lines = count_lines(&content);
        let mut row = rows - 1 - lines;
        // TODO: Split into lines...
            //.queue(SetForegroundColor(Color::Red))?
            //.queue(SetBackgroundColor(Color::Blue))?

        let mut col = 0;
        for line in content.lines() {
            stdout()
                .queue(MoveTo(0, row))?
                .queue(Print(line))?;
            row+=1;
            col = line.len();
        }
            stdout()
            .queue(MoveTo(col as u16, row))?;
        stdout().queue(ResetColor)?.flush()?;
        if self.should_exit {
            stdout().queue(Clear(ClearType::All))?;
            stdout().flush()?;
            std::process::exit(0)
        }
        Ok(())
    }

    fn handle_event(&mut self, event: Event) -> std::io::Result<()> {
        let mut characters = "".to_string();
        match event {
            Event::Key(key_event) => {
                match key_event {
                    key!(ctrl - c) | key!(ctrl - q) => self.should_exit = true,
                    key!(Backspace) => {
                        self.input.pop(); // Discard
                    }
                    key!(ctrl - d) => {
                        if self.input.is_empty() {
                            self.should_exit = true;
                        }
                        self.input = "".to_string(); // Discard
                        self.input_after_cursor = "".to_string(); // Discard
                    }
                    key!(ctrl - u) => {
                        self.input = "".to_string(); // Discard
                    }
                    key!(ctrl - k) => {
                        self.input_after_cursor = "".to_string(); // Discard
                    }
                    key!(ctrl - w) => {
                        let last_space = self.input.rfind(' ').unwrap_or(0);
                        self.input = self.input[0..last_space].to_string();
                    }
                    key!(Shift - Enter) => {
                        self.input.push('\n');
                    }
                    key!(Enter) => {
                        // Submit the expression
                        if true {
                            // accepted
                            let mut line = "".to_string();
                            std::mem::swap(&mut self.input, &mut line);
                            line += &self.input_after_cursor;
                            if !line.is_empty() {
                                self.history.push(line);
                            }
                            self.input_after_cursor = "".to_string();
                        } else {
                            // show errr?
                        }
                    }
                    KeyEvent {
                        code: KeyCode::Char(letter),
                        modifiers: _,
                    } => {
                        self.input.push(letter);
                    }
                    _ => {
                        // discard
                    }
                };
                characters = format!("{}{}", characters, self.key_fmt.to_string(key_event));
            }
            other => trace!("Event: {:?}", other),
        }
        if !characters.is_empty() {
            self.characters = characters;
        }
        Ok(())
    }
}

#[async_trait]
impl UserInterface for Tui {
    async fn launch(
        mut task_manager_status_receiver: mpsc::UnboundedReceiver<StatusReport>,
        // User control of the compiler
        _request_sender: Option<mpsc::UnboundedSender<Request>>,
        stats_requester: Arc<Mutex<broadcast::Sender<()>>>,
    ) -> std::io::Result<()> {
        let _start_time = Instant::now();
        let mut tui = Self::default();
        add_shutdown_hook(shutdown);
        enable_raw_mode().expect("TUI failed to enable raw mode");
        let mut stats_ticker = time::interval(TICK);
        let mut reader = EventStream::new();

        loop {
            let event = reader.next().fuse();

            tokio::select! {
                Some(StatusReport { kind, stats }) = task_manager_status_receiver.recv() => {
                    trace!("TaskManager stats: {kind:?} => {stats}");
                    tui.manager_status.insert(kind, stats);
                },
                _ = stats_ticker.tick() => {
                    let stats_requester = stats_requester.lock().expect("stats requester lock");
                    stats_requester.send(()).expect("TODO");
                }
                Some(maybe_event) = event => {
                    match maybe_event {
                        Ok(event) => tui.handle_event(event)?,
                        Err(err) => {
                            trace!("Event stream error: {}", err);
                            break
                        }
                    }
                }
                else => break,
            }
            // tui.cursor_blink = (start_time.elapsed().as_secs() % 2) == 0;
            tui.render()?;
        }
        Ok(())
    }
}
