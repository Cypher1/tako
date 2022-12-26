use crate::primitives::Prim;
use crate::tasks::StatusReport;
use crate::{cli_options::Options, tasks::RequestTask};
use async_trait::async_trait;
use tokio::sync::{broadcast, mpsc};

mod client;
mod http;
mod tui;

pub use client::Client;
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
pub trait UserInterface {
    async fn launch(
        task_manager_status_receiver: broadcast::Receiver<StatusReport>,
        request_sender: mpsc::UnboundedSender<(RequestTask, mpsc::UnboundedSender<Prim>)>,
        stats_requester: broadcast::Sender<()>,
        options: Options,
    ) -> std::io::Result<()>
    where
        Self: Sized;
}
