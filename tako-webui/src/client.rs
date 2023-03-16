use async_trait::async_trait;
use std::path::PathBuf;

use tako::tasks::RequestTask;
use tako::ui::{Client, OptionsTrait, UserInterface};
use tokio::spawn;
use tokio::sync::{mpsc, oneshot};

#[derive(Debug)]
struct YewClient {}

#[derive(Default, Debug, Clone)]
struct Options {
    files: Vec<PathBuf>,
}

impl OptionsTrait for Options {
    fn files(&self) -> &Vec<PathBuf> {
        &self.files
    }
    fn interactive(&self) -> bool {
        false
    }
    fn oneshot(&self) -> bool {
        false
    }
    fn interpreter(&self) -> bool {
        true
    }
}

#[async_trait]
impl UserInterface<Options> for YewClient {
    async fn launch(
        _client_launch_request_sender: mpsc::UnboundedSender<(
            oneshot::Sender<Client>,
            Box<dyn OptionsTrait>,
        )>,
        _options: Options,
    ) -> std::io::Result<Self> {
        Ok(YewClient {})
    }
    async fn run_loop(self) -> std::io::Result<()> {
        Ok(())
    }
}

pub async fn interpret(src: &str) -> String {
    tako::ensure_initialized();
    let compiler = tako::start().await;
    let client_launch_request_sender = compiler.client_launch_request_sender.clone();
    spawn(async move { compiler.run_loop().await });
    let (tx, rx) = tokio::sync::oneshot::channel();
    client_launch_request_sender
        .send((tx, Box::<Options>::default()))
        .expect("Send client request failed");
    let mut client = rx.await.expect("Get client failed");
    client.send_command(RequestTask::EvalLine(src.to_string()));
    let result = client.result_receiver.recv();
    if let Some(result) = result.await {
        result.to_string()
    } else {
        "failed?".to_string()
    }
}
