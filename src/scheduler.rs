use std::sync::{Arc, Mutex};
use tokio::sync::mpsc;
use crate::cli_options::Options;
use crate::tasks::{
    LaunchTask,
    TaskSet,
};
use crate::error::TError;
use crate::ui::UserInterface;

const MAX_SCHEDULER_LAG: usize = 100;

fn make_ui_arc<T: UserInterface + Send + 'static>(
    value: T,
) -> Arc<Mutex<dyn UserInterface + Send>> {
    Arc::new(Mutex::new(value))
}

#[derive(Debug)]
pub struct Scheduler {
    ui: Arc<Mutex<dyn UserInterface + Send>>,
    options: Options,
}

impl Scheduler {
    async fn run(&self) -> Result<(), TError> {
        let (request_sender, request_receiver) = mpsc::unbounded_channel();
        let (result_sender, mut result_receiver) = mpsc::unbounded_channel();

        let store = TaskSet::new(request_receiver, result_sender); // Setup!
        store.launch().await; // launches all the jobs.
        request_sender.send(Ok(
            LaunchTask {
            options: self.options.clone()
                })); // Launch the cli task.

        // Receive the results...
        while let Some(ast) = result_receiver.recv().await {
            dbg!(ast);
        }
        // All done!
        Ok(())
    }

    pub fn from_options(options: Options) -> Self {
        use crate::ui::{UiMode, CLI, TUI};
        let ui = match options.ui_mode {
            UiMode::Cli => make_ui_arc(CLI::new()),
            UiMode::Tui => make_ui_arc(TUI::new()),
            UiMode::TuiIfAvailable => {
                if false {
                    make_ui_arc(CLI::new())
                } else {
                    make_ui_arc(TUI::new())
                }
            }
        };
        Self {
            options,
            ui,
        }
    }
}
