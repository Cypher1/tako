use super::UserInterface;
use crate::cli_options::Options;
use crate::tasks::{RequestTask, StatusReport, TaskKind, TaskStats};
use async_trait::async_trait;
use crokey::{key, KeyEventFormat};
use crossterm::{
    cursor::MoveTo,
    event::{Event, EventStream, KeyCode, KeyEvent},
    style::{/*Color,*/ Print, ResetColor /*, SetBackgroundColor, SetForegroundColor*/},
    terminal::{disable_raw_mode, enable_raw_mode, size, Clear, ClearType},
    QueueableCommand,
};
use futures::{future::FutureExt, StreamExt};
use log::{debug, trace};
use shutdown_hooks::add_shutdown_hook;
use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};
use std::{
    io::{stdout, Write},
    time::{Duration, Instant},
};
use tokio::time;
use tokio::{
    self,
    sync::{broadcast, mpsc},
};

const TICK: Duration = Duration::from_millis(100);

extern "C" fn shutdown() {
    let _discard = disable_raw_mode();
}

#[derive(Debug)]
pub struct Tui<Out: Send + std::fmt::Debug + std::fmt::Display> {
    manager_status: BTreeMap<TaskKind, TaskStats>,
    request_sender: Option<mpsc::UnboundedSender<RequestTask>>,
    response_getter: Option<mpsc::UnboundedReceiver<Out>>,
    key_fmt: KeyEventFormat,
    should_exit: bool,
    history: Vec<String>, // TODO: Mark Input v output.
    input: String,
    input_after_cursor: String,
    characters: String,
    options: Options,
}

impl<Out: Send + std::fmt::Debug + std::fmt::Display> Default for Tui<Out> {
    fn default() -> Self {
        Self {
            manager_status: BTreeMap::default(),
            request_sender: None,
            response_getter: None,
            key_fmt: KeyEventFormat::default(),
            should_exit: false,
            history: Vec::default(),
            input: "".to_string(),
            input_after_cursor: "".to_string(),
            characters: "".to_string(),
            options: Options::default(),
        }
    }
}

impl<Out: Send + std::fmt::Debug + std::fmt::Display> Tui<Out> {
    fn render(&self) -> std::io::Result<()> {
        stdout().queue(Clear(ClearType::All))?;
        let (cols, rows) = size()?;

        let mut row = 0;
        row += 1;
        for (task_kind, stats) in &self.manager_status {
            let line = format!("{task_kind:?}: {stats}");
            let len = line.len() as u16;
            stdout()
                .queue(MoveTo(cols - len - 1, row))?
                .queue(Print(line))?;
            row += 1;
        }

        let count_lines = |s: &str| s.chars().filter(|c| *c == '\n').count();
        let mut content = "".to_string();
        for hist_entry in &self.history {
            content += hist_entry;
            content += "\n";
        }
        content += &format!(">> {}{}", self.input, self.input_after_cursor);
        let lines = count_lines(&content);
        let lines_after_cursor = count_lines(&self.input_after_cursor);
        let chars_after_cursor = self
            .input_after_cursor
            .find('\n')
            .unwrap_or(self.input_after_cursor.len());
        let mut row = (rows as usize).saturating_sub(lines + 1); // TODO: Workout how to drop old lines.
        let missed_rows = lines.saturating_sub(rows as usize - 1); // TODO: Workout how to drop old lines.
                                                                   // TODO: Split into lines...
                                                                   //.queue(SetForegroundColor(Color::Red))?
                                                                   //.queue(SetBackgroundColor(Color::Blue))?

        let mut col = 0;
        for line in content.lines().skip(missed_rows) {
            stdout().queue(MoveTo(0, row as u16))?.queue(Print(line))?;
            row += 1;
            col = line.len();
        }
        col -= chars_after_cursor;
        row -= lines_after_cursor;
        stdout().queue(MoveTo(col as u16, row as u16))?;
        stdout().queue(ResetColor)?.flush()?;
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
                    key!(Delete) => {
                        let mut chars = self.input_after_cursor.chars();
                        chars.next();
                        self.input_after_cursor = chars.collect();
                    }
                    key!(ctrl - d) => {
                        if self.input.is_empty() && self.input_after_cursor.is_empty() {
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
                    key!(right) => {
                        let first = &self.input_after_cursor.chars().next();
                        self.input.extend(first);
                        let mut chars = self.input_after_cursor.chars();
                        chars.next();
                        self.input_after_cursor = chars.collect();
                    }
                    key!(left) => {
                        let last = self.input.pop();
                        if let Some(last) = last {
                            self.input_after_cursor = format!("{last}{}", self.input_after_cursor);
                        }
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
                                // TODO: Send the line to the compiler.
                                trace!("Running {line}");
                                if let Some(request_sender) = &self.request_sender {
                                    request_sender
                                        .send(RequestTask::EvalLine(line.to_string()))
                                        .expect("Need a backend");
                                } else {
                                    trace!("No backend?");
                                    self.history
                                        .push("...not connected to a backend".to_string());
                                }
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
                characters = format!("{characters}{}", self.key_fmt.to_string(key_event));
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
impl<Out: Send + std::fmt::Debug + std::fmt::Display> UserInterface<Out> for Tui<Out> {
    async fn launch(
        mut task_manager_status_receiver: mpsc::UnboundedReceiver<StatusReport>,
        // User control of the compiler
        request_sender: Option<mpsc::UnboundedSender<RequestTask>>,
        response_getter: mpsc::UnboundedReceiver<Out>,
        stats_requester: Arc<Mutex<broadcast::Sender<()>>>,
        options: Options,
    ) -> std::io::Result<()> {
        let _start_time = Instant::now();
        let mut tui = Tui {
            request_sender,
            response_getter: Some(response_getter),
            options,
            ..Self::default()
        };
        add_shutdown_hook(shutdown);
        if tui.options.interactive() {
            debug!("Enabling raw mode");
            enable_raw_mode().expect("TUI failed to enable raw mode");
        }
        let mut stats_ticker = time::interval(TICK);
        let mut reader = EventStream::new();

        loop {
            let event = reader.next().fuse();
            let response_getter = tui.response_getter.as_mut().expect("No response_getter");
            tokio::select! {
                Some(StatusReport { kind, stats }) = task_manager_status_receiver.recv() => {
                    trace!("TaskManager stats: {kind:?} => {stats}");
                    tui.manager_status.insert(kind, stats);
                },
                _ = stats_ticker.tick() => {
                    let stats_requester = stats_requester.lock().expect("stats requester lock");
                    stats_requester.send(()).expect("TODO");
                }
                Some(value) = response_getter.recv() => {
                    trace!("Got result value: {value:?}");
                    tui.history.push(format!("{value:#?}"));
                    if tui.options.oneshot() {
                        tui.should_exit = true;
                    }
                },
                Some(maybe_event) = event => {
                    match maybe_event {
                        Ok(event) => {
                            // trace!("Event: {event:?}");
                            tui.handle_event(event)?
                        }
                        Err(err) => {
                            trace!("Event stream error: {}", err);
                            break
                        }
                    }
                }
                else => break,
            }
            if tui.should_exit {
                // stdout().queue(Clear(ClearType::All))?;
                stdout().flush()?;
                std::process::exit(0)
            }
            // tui.cursor_blink = (start_time.elapsed().as_secs() % 2) == 0;
            if tui.options.interactive() {
                tui.render()?;
            }
        }
        Ok(())
    }
}
