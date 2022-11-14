use std::sync::{Arc, Mutex};

use crate::tasks::{Request, StatusReport};
use async_trait::async_trait;
use tokio::sync::{broadcast, mpsc};

mod cli;
mod tui;
mod http;

pub use cli::Cli;
pub use tui::Tui;
pub use http::Http;

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
        request_sender: Option<mpsc::UnboundedSender<Request>>,
        stats_requester: Arc<Mutex<broadcast::Sender<()>>>,
    ) -> std::io::Result<()>
    where
        Self: Sized;
}
