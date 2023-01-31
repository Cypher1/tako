use async_trait::async_trait;
use std::path::PathBuf;
use tako;
use tako::tasks::RequestTask;
use tako::ui::{Client, OptionsTrait, UserInterface};
use tako::utils::spawn;
use tokio::sync::{mpsc, oneshot};

#[derive(Debug)]
struct YewClient {}

const NONE_FILES: Vec<PathBuf> = vec![];

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
}

#[async_trait]
impl UserInterface<Options> for YewClient {
    async fn launch(
        client_launch_request_sender: mpsc::UnboundedSender<(
            oneshot::Sender<Client>,
            Box<dyn OptionsTrait>,
        )>,
        options: Options,
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
        .send((tx, Box::new(Options::default())))
        .expect("Send client request failed");
    let mut client = rx.await.expect("Get client failed");
    client.send_command(RequestTask::EvalLine(src.to_string()));
    "3".to_string()
}
