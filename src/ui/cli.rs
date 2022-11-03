use super::UserInterface;
use crate::compiler_tasks::Progress;
use crate::error::{Error, ErrorId};

#[derive(Debug)]
pub struct CLI {}

impl CLI {
    pub fn new() -> Self {
        Self {}
    }
}

impl UserInterface for CLI {
    fn report_error(&mut self, _error_id: ErrorId, error: &Error) {
        eprintln!("Error: {error:?}");
    }
    fn report_progress(&mut self, progress: Progress) {
        eprintln!("{}", progress);
    }
    fn report_job_counts(&mut self, num_successful: usize, num_finished: usize, num_total: usize) {
        match (num_successful, num_finished, num_total) {
            (_successful, _finished, /*total*/ 0) => eprintln!("No tasks."),
            (successful, finished, total) => {
                let failed = finished - successful;
                let s = if total == 1 { "" } else { "s" };
                if successful == total {
                    eprintln!("Finished all {total} job{s}.")
                } else if failed == 0 {
                    eprintln!("Finished {successful}/{total} job{s}.")
                } else {
                    eprintln!("Finished {successful}/{total} job{s}. {failed} failed or cancelled.")
                }
            }
        }
    }
}
