use crate::error::{Error, ErrorId};
use crate::compiler_tasks::Progress;

mod cli;
pub use cli::CLI;

mod tui;
pub use tui::TUI;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UiMode {
    Cli,
    Tui,
    TuiIfAvailable
}

pub trait UserInterface: std::fmt::Debug {
    fn report_error(&mut self, error_id: ErrorId, error: &Error);
    fn report_progress(&mut self, progress: Progress);
    fn report_job_counts(&mut self, num_successful: usize, num_finished: usize, num_total: usize);
}
