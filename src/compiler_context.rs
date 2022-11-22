use tokio::sync::{broadcast, mpsc};
use crate::tasks::task_trait::{TaskReceiverFor, TaskSenderFor};
use crate::tasks::*;

#[derive(Debug)]
struct Compiler {
    request_receiver: TaskReceiverFor<RequestTask>,
    result_sender: TaskSenderFor<EvalFileTask>,
    stats_sender: mpsc::UnboundedSender<StatusReport>,
    stats_requester: broadcast::Sender<()>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
        }
    }

    pub async fn watch_file(file: Pathbuf) {
        tokio::spawn(async move {
        });
    }

    pub async fn load_file(path: Pathbuf, results) {
        let contents = std::fs::read_to_string(&path);
        results.send(Ok((path.clone(), contents)));
        tokio::spawn(async move {
            Compiler::watch_file(path).await;
        });
    }

    pub async fn lex_file(path: PathBuf, contents: Option<String>, results) {
        tokio::spawn(async move {
            let contents = contents.unwrap_or_else(||{
                Compiler::load_file(path.clone()).await
            );
            let tokens = crate::tokens::lex_file(&contents);
            results.send(Ok((path, tokens)));
        });
    }

    pub async fn parse_file() {
    }

    pub async fn interpret_file() {
    }
}
