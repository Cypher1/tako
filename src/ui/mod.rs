use crate::tasks::{Request, StatusReport};
use async_trait::async_trait;
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

#[async_trait]
pub trait UserInterface: std::fmt::Debug {
    async fn launch(
        task_manager_status_receiver: mpsc::UnboundedReceiver<StatusReport>,
        _user_action_receiver: mpsc::UnboundedReceiver<UserAction>,
        _request_sender: mpsc::UnboundedSender<Request>,
    ) where
        Self: Sized;
}
