use crate::cli_options::Options;
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
use log::trace;
use shutdown_hooks::add_shutdown_hook;
use std::{
    io::{stdout, Write},
    time::{Duration, Instant},
};
use tako::tasks::RequestTask;
use tako::ui::{Client, OptionsTrait, UserInterface};
use tokio::{
    sync::{mpsc, oneshot},
    time,
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
    fn new(client: Client) -> Self {
        Self {
            client,
            key_fmt: KeyEventFormat::default(),
            should_exit: false,
            input: String::new(),
            input_after_cursor: String::new(),
            characters: String::new(),
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
        let mut content = String::new();
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
        // TODO(usability): Workout how to drop old lines.
        let mut row = (rows as usize).saturating_sub(lines + 1);
        let missed_rows = lines.saturating_sub(rows as usize - 1);

        let mut col = 0;
        for line in content.lines().skip(missed_rows) {
            stdout().queue(MoveTo(0, row as u16))?.queue(Print(line))?;
            //  .queue(SetForegroundColor(Color::Red))?
            //  .queue(SetBackgroundColor(Color::Blue))?
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
        let mut characters = String::new();
        match event {
            Event::Key(key_event) => {
                match key_event {
                    ::crokey::__private::crossterm::event::KeyEvent {
modifiers: ::crokey::__private::MODS_CTRL,
code: ::crokey::__private::crossterm::event::KeyCode::Char('c' | 'q') } => self.should_exit = true,
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
                        self.input = String::new(); // Discard
                        self.input_after_cursor = String::new(); // Discard
                    }
                    key!(ctrl - u) => {
                        self.input = String::new(); // Discard
                    }
                    key!(ctrl - k) => {
                        self.input_after_cursor = String::new(); // Discard
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
                        let mut line = String::new();
                        std::mem::swap(&mut self.input, &mut line);
                        line += &self.input_after_cursor;
                        if !line.is_empty() {
                            trace!("Running {line}");
                            self.client
                                .send_command(RequestTask::EvalLine(line.to_string()));
                        }
                        self.input_after_cursor = String::new();
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
impl UserInterface<Options> for Tui {
    async fn launch(
        mut client_launch_request_sender: mpsc::UnboundedSender<(
            oneshot::Sender<Client>,
            Box<dyn OptionsTrait>,
        )>,
        options: Options,
    ) -> std::io::Result<Self> {
        trace!("Launching TUI");
        let client = Self::get_client(&mut client_launch_request_sender, Box::new(options)).await;
        trace!("Got TUI client");
        add_shutdown_hook(shutdown);
        Ok(Self::new(client))
    }

    async fn run_loop(mut self) -> std::io::Result<()> {
        let _start_time = Instant::now();
        add_shutdown_hook(shutdown);
        if self.client.interactive() {
            trace!("Enabling raw mode");
            enable_raw_mode().expect("TUI failed to enable raw mode");
        }
        self.client.start();

        let mut stats_ticker = time::interval(TICK);
        let mut reader = EventStream::new();

        loop {
            let event = reader.next().fuse();
            tokio::select! {
                got_last_result = self.client.wait_for_updates() => {
                    if got_last_result {
                        self.should_exit = true;
                    }
                }
                _ = stats_ticker.tick() => self.client.get_stats(),
                Some(maybe_event) = event => {
                    match maybe_event {
                        Ok(event) => {
                            // trace!("Event: {event:?}");
                            self.handle_event(event)?;
                        }
                        Err(err) => {
                            trace!("Event stream error: {err}");
                        }
                    }
                }
            }
            if self.should_exit {
                // stdout().queue(Clear(ClearType::All))?;
                stdout().flush()?;
                std::process::exit(0)
            }
            // self.cursor_blink = (start_time.elapsed().as_secs() % 2) == 0;
            if self.client.interactive() {
                self.render()?;
            }
        }
    }
}
