use std::path::PathBuf;

use async_trait::async_trait;

mod client;
pub use client::Client;
use log::trace;
use tokio::sync::{mpsc, oneshot};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UserAction {
    Something,
}

#[async_trait]
pub trait OptionsTrait: std::fmt::Debug + Send {
    fn files(&self) -> &Vec<PathBuf>;
    fn interactive(&self) -> bool;
    fn oneshot(&self) -> bool;
}

#[async_trait]
pub trait UserInterface<Options: OptionsTrait> {
    async fn get_client(
        client_launch_request_sender: &mut mpsc::UnboundedSender<(
            oneshot::Sender<Client>,
            Box<dyn OptionsTrait>,
        )>,
        options: Box<dyn OptionsTrait>,
    ) -> Client {
        trace!("Requesting client");
        let (tx, rx) = oneshot::channel();
        client_launch_request_sender
            .send((tx, options))
            .expect("Should be able to request a client");
        trace!("Waiting for client");
        rx.await.expect("Should be able to get a client")
    }
    async fn launch(
        client_launch_request_sender: mpsc::UnboundedSender<(
            oneshot::Sender<Client>,
            Box<dyn OptionsTrait>,
        )>,
        options: Options,
    ) -> std::io::Result<Self>
    where
        Self: Sized;

    async fn run_loop(self) -> std::io::Result<()>;
}
