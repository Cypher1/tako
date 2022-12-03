use crate::cli_options::Options;
use crate::compiler_context::Compiler;
use crate::tasks::StatusReport;
use async_trait::async_trait;
use tokio::sync::{broadcast, mpsc};

mod client;
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
pub trait UserInterface {
    async fn launch(
        task_manager_status_receiver: mpsc::UnboundedReceiver<StatusReport>,
        compiler: Compiler,
        stats_requester: broadcast::Sender<()>,
        options: Options,
    ) -> std::io::Result<()>
    where
        Self: Sized;
}
