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

    pub async fn load_file() {
        tokio::spawn(async move {
            
        });
    }

    pub async fn lex_file() {
    }

    pub async fn parse_file() {
    }

    pub async fn interpret_file() {
    }
}
