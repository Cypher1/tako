use std::sync::{Arc, Mutex};

use crate::{tasks::{RequestTask, StatusReport}, ast::Ast};
use async_trait::async_trait;
use tokio::sync::{broadcast, mpsc};

mod cli;
mod http;
mod tui;

pub use cli::Cli;
pub use http::Http;
pub use tui::Tui;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UserAction {
    Something,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UiMode {
    Cli,
    Tui,
    Http,
}

#[async_trait]
pub trait UserInterface: std::fmt::Debug {
    async fn launch(
        task_manager_status_receiver: mpsc::UnboundedReceiver<StatusReport>,
        request_sender: Option<mpsc::UnboundedSender<RequestTask>>,
        response_getter: Option<mpsc::UnboundedReceiver<Ast>>,
        stats_requester: Arc<Mutex<broadcast::Sender<()>>>,
    ) -> std::io::Result<()>
    where
        Self: Sized;
}
