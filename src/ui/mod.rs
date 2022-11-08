use std::collections::HashMap;

use tokio::sync::mpsc;
use async_trait::async_trait;
use crate::tasks::{Request, TaskManagerRegistration, TaskKind};

mod cli;
mod tui;

pub type Registrations = HashMap<TaskKind, TaskManagerRegistration>;

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
        _task_manager_registration: mpsc::UnboundedReceiver<TaskManagerRegistration>,
        _user_action_receiver: mpsc::UnboundedReceiver<UserAction>,
        _request_sender: mpsc::UnboundedSender<Request>,
    ) where Self: Sized;
}
