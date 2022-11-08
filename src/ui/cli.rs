use super::UserInterface;
use log::{error, info};
// use crate::compiler_tasks::Progress;

#[derive(Debug, Default)]
pub struct Cli {}

impl Cli {
    pub fn new() -> Self {
        Self {}
    }
}

impl UserInterface for Cli {
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
