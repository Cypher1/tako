use tokio::sync::mpsc;
use super::UserInterface;
use crate::{Request, UserAction, tasks::TaskManagerRegistration};

// use crate::compiler_tasks::Progress;

#[derive(Debug)]
pub struct Cli {
    task_manager_registration: mpsc::UnboundedReceiver<TaskManagerRegistration>,
    user_action_receiver: mpsc::UnboundedReceiver<UserAction>,
    request_sender: mpsc::UnboundedSender<Request>,
}

impl Cli {
}

impl UserInterface for Cli {
    fn launch(
        task_manager_registration: mpsc::UnboundedReceiver<TaskManagerRegistration>,
        user_action_receiver: mpsc::UnboundedReceiver<UserAction>,
        request_sender: mpsc::UnboundedSender<Request>,
    ) -> Self {
        Self {
            task_manager_registration,
            user_action_receiver,
            request_sender,
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
