use super::UserInterface;
use super::client::Client;
use crate::cli_options::Options;
use crate::compiler_context::Compiler;
use crate::error::Error;
use crate::primitives::Prim;
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
use std::collections::{BTreeSet, HashMap};
use std::path::PathBuf;
use std::{
    io::{stdout, Write},
    time::{Duration, Instant},
};
use tokio::time;
use tokio::{
    self,
    sync::{broadcast, mpsc},
};

const TICK: Duration = Duration::from_millis(1000);

extern "C" fn shutdown() {
    let _discard = disable_raw_mode();
}

#[derive(Debug)]
pub struct Tui {
    key_fmt: KeyEventFormat,
    should_exit: bool,
    input: String,
    input_after_cursor: String,
    characters: String,
    client: Client,
}

impl Tui {
    fn new(compiler: Compiler, options: Options) -> Self {
        let (result_sender, result_receiver) = mpsc::unbounded_channel();
        Self {
            client: Client {
                manager_status: HashMap::default(),
                history: Vec::default(),
                errors_for_file: HashMap::default(),
                compiler,
                options,
                result_receiver,
                result_sender,
            },
            key_fmt: KeyEventFormat::default(),
            should_exit: false,
            input: "".to_string(),
            input_after_cursor: "".to_string(),
            characters: "".to_string(),
        }
    }

    fn render(&self) -> std::io::Result<()> {
        stdout().queue(Clear(ClearType::All))?;
        let (cols, rows) = size()?;

        let mut row = 0;
        row += 1;
        for (task_kind, stats) in &self.client.manager_status {
            let line = format!("{task_kind:?}: {stats}");
            let len = line.len() as u16;
            stdout()
                .queue(MoveTo(cols - len - 1, row))?
                .queue(Print(line))?;
            row += 1;
        }
        for errs in self.client.errors_for_file.values() {
            for err in errs {
                let line = format!("{err}");
                let len = line.len() as u16;
                stdout()
                    .queue(MoveTo(cols - len - 1, row))?
                    .queue(Print(line))?;
                row += 1;
            }
        }

        let count_lines = |s: &str| s.chars().filter(|c| *c == '\n').count();
        let mut content = "".to_string();
        for hist_entry in &self.client.history {
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
                                self.client.compiler.send_command(
                                    RequestTask::EvalLine(line.to_string()),
                                    self.client.result_sender.clone(),
                                );
                                self.client.history.push(line);
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
impl UserInterface for Tui {
    async fn launch(
        mut task_manager_status_receiver: mpsc::UnboundedReceiver<StatusReport>,
        // User control of the compiler
        compiler: Compiler,
        stats_requester: broadcast::Sender<()>,
        options: Options,
    ) -> std::io::Result<()> {
        let _start_time = Instant::now();
        let mut tui = Tui::new(compiler, options);
        add_shutdown_hook(shutdown);
        if tui.client.options.interactive() {
            debug!("Enabling raw mode");
            enable_raw_mode().expect("TUI failed to enable raw mode");
        }

        tui.client.compiler.send_command(
            RequestTask::Launch {
                files: tui.client.options.files.clone(),
            },
            tui.client.result_sender.clone(),
        );

        let mut stats_ticker = time::interval(TICK);
        let mut reader = EventStream::new();

        loop {
            let event = reader.next().fuse();
            let result_receiver = &mut tui.client.result_receiver;
            tokio::select! {
                Some(StatusReport { kind, stats, errors }) = task_manager_status_receiver.recv() => {
                    trace!("TaskManager status: {kind:?} => {stats}\nerrors: {errors:#?}");
                    for (_id, err) in errors {
                        let file = err.location.as_ref().map(|loc| loc.filename.clone());
                        let errs = tui.client.errors_for_file.entry(file).or_insert_with(BTreeSet::new);
                        errs.insert(err);
                    }
                    tui.client.manager_status.insert(kind, stats);
                },
                _ = stats_ticker.tick() => {
                    stats_requester.send(()).expect("TODO");
                }
                Some(value) = result_receiver.recv() => {
                    trace!("Got result value: {value:?}");
                    if !tui.client.options.interactive() {
                        println!("{value:?}");
                    }
                    tui.client.history.push(format!("{value:#?}"));
                    if tui.client.options.oneshot() {
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
            if tui.client.options.interactive() {
                tui.render()?;
            }
        }
        Ok(())
    }
}
