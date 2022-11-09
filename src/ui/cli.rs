use std::collections::HashMap;

use super::UserInterface;
use crate::{tasks::StatusReport, Request, UserAction};
use async_trait::async_trait;
use tokio::sync::{mpsc, broadcast};
use std::sync::{Arc, Mutex};

use std::time::Duration;
use tokio::time;
const TICK: Duration = Duration::from_millis(1000);
// use crate::compiler_tasks::Progress;

#[derive(Debug)]
pub struct Cli {}

#[async_trait]
impl UserInterface for Cli {
    async fn launch(
        mut task_manager_status_receiver: mpsc::UnboundedReceiver<StatusReport>,
        mut user_action_receiver: mpsc::UnboundedReceiver<UserAction>,
        // User control of the compiler
        _request_sender: Option<mpsc::UnboundedSender<Request>>,
        stats_requester: Arc<Mutex<broadcast::Sender<()>>>,
    ) {
        let mut manager_status = HashMap::new();
        let mut ticker = time::interval(TICK);
        loop {
            tokio::select! {
                Some(StatusReport { kind, stats }) = task_manager_status_receiver.recv() => {
                    eprintln!("TaskManager stats: {kind:?} => {stats:?}");
                    manager_status.insert(kind, stats);
                },
                Some(action) = user_action_receiver.recv() => {
                    eprintln!("User action: {action:?}");
                },
                _ = ticker.tick() => {
                    let stats_requester = stats_requester.lock().expect("stats requester lock");
                    stats_requester.send(()).expect("TODO");
                }
                else => break,
            }
        }
    }
    /*
    fn report_error(&mut self, _error_id: ErrorId, error: &Error) {
        eprintln!("Error: {error:?}");
    }
    fn report_progress(&mut self, progress: Progress) {
        eprintln!("{progress}");
    }
    fn report_job_counts(&mut self, num_successful: usize, num_finished: usize, num_total: usize) {
        match (num_successful, num_finished, num_total) {
            (_successful, _finished, /*total*/ 0) => eprintln!("No tasks."),
            (successful, finished, total) => {
                let failed = finished - successful;
                let s = if total == 1 { "" } else { "s" };
                if successful == total {
                    info!("Finished all {total} job{s}.")
                } else if failed == 0 {
                    info!("Finished {successful}/{total} job{s}.")
                } else {
                    info!("Finished {successful}/{total} job{s}. {failed} failed or cancelled.")
                }
            }
        }
    }*/
}
