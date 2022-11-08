use crate::tasks::{Request, StatusReport};
use tokio::sync::mpsc;

mod cli;
mod tui;

pub use cli::Cli;
pub use tui::Tui;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UserAction {
    Something,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UiMode {
    Cli,
    Tui,
}

pub trait UserInterface: std::fmt::Debug {
    fn launch(
        &mut self,
        _status_report_receiver: mpsc::UnboundedReceiver<StatusReport>,
        _user_action_receiver: mpsc::UnboundedReceiver<UserAction>,
        _request_sender: mpsc::UnboundedSender<Request>,
    ) {
        // noop... bad default but easy to implement.
    }
    // fn report_error(&mut self, error_id: ErrorId, error: &Error);
    // fn report_progress(&mut self, progress: Progress);
    // fn report_job_counts(&mut self, num_successful: usize, num_finished: usize, num_total: usize);
}
