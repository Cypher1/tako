use crate::tasks::{RequestTask, StatusReport};
use async_trait::async_trait;
use std::sync::{Arc, Mutex};
use tokio::sync::{broadcast, mpsc};
use crate::cli_options::Options;

mod http;
mod tui;

pub use http::Http;
pub use tui::Tui;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UserAction {
    Something,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UiMode {
    Tui,
    Http,
}

#[async_trait]
pub trait UserInterface<Out: Send + std::fmt::Debug + std::fmt::Display>: std::fmt::Debug {
    async fn launch(
        task_manager_status_receiver: mpsc::UnboundedReceiver<StatusReport>,
        request_sender: Option<mpsc::UnboundedSender<RequestTask>>,
        response_getter: mpsc::UnboundedReceiver<Out>,
        stats_requester: Arc<Mutex<broadcast::Sender<()>>>,
        options: Options,
    ) -> std::io::Result<()>
    where
        Self: Sized;
}
