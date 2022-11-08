use crate::tasks::{Request, StatusReport, TaskManagerRegistration};
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
        _task_manager_registration: mpsc::UnboundedReceiver<TaskManagerRegistration>,
        _user_action_receiver: mpsc::UnboundedReceiver<UserAction>,
        _request_sender: mpsc::UnboundedSender<Request>,
    ) -> Self where Self: Sized;
}
