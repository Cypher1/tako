use crate::tasks::task_trait::{ResultSenderFor, Task, TaskReceiverFor};
use crate::tasks::*;
use tokio::sync::{broadcast, mpsc};

use crate::tasks::manager::TaskManager;
pub use crate::tasks::manager::{StatusReport, TaskStats};
pub use crate::tasks::status::*;
pub use crate::tasks::task_trait::TaskId;
use std::fmt::Debug;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub struct Compiler {
    // TODO: Make a trait...
    watch_file_manager: Arc<Mutex<TaskManager<WatchFileTask>>>,
    load_file_manager: Arc<Mutex<TaskManager<LoadFileTask>>>,
    lex_file_manager: Arc<Mutex<TaskManager<LexFileTask>>>,
    parse_file_manager: Arc<Mutex<TaskManager<ParseFileTask>>>,
    eval_file_manager: Arc<Mutex<TaskManager<EvalFileTask>>>,
}

impl Compiler {
    pub fn new(
        stats_sender: mpsc::UnboundedSender<StatusReport>,
        stats_requester: broadcast::Sender<()>,
    ) -> Self {
        Self {
            watch_file_manager: Self::manager(&stats_sender, &stats_requester),
            load_file_manager: Self::manager(&stats_sender, &stats_requester),
            lex_file_manager: Self::manager(&stats_sender, &stats_requester),
            parse_file_manager: Self::manager(&stats_sender, &stats_requester),
            eval_file_manager: Self::manager(&stats_sender, &stats_requester),
            // TODO: type_check_inside_module: TaskManager<>,
            // Produces type checked (and optimizable) modules **AND**
            // partially type checked (but) mergable-modules.
            // Pair-wise merging of type checking information???
            // TODO: type_check_merge_module_sets: TaskManager<>,
            // Produces type checked (and optimizable) modules **AND**
            // Partially type checked (but) mergable-modules
            // TODO: lowering: TaskManager<>,
            // TODO: optimization: TaskManager<>,
            // TODO: code_generation: TaskManager<>,
            // TODO: binary_generation: TaskManager<>,
            // TODO: load_into_interpreter: TaskManager<>,
            // TODO: run_in_interpreter: TaskManager<>,
        }
    }

    fn manager<T: Task + 'static>(
        stats_sender: &mpsc::UnboundedSender<StatusReport>,
        stats_requester: &broadcast::Sender<()>,
    ) -> Arc<Mutex<TaskManager<T>>> {
        let manager = Arc::new(Mutex::new(TaskManager::<T>::new()));
        {
            let manager = manager.clone();
            let stats_sender = stats_sender.clone();
            let stats_requester = stats_requester.subscribe();
            tokio::spawn(async move {
                TaskManager::report_stats(manager, stats_requester, stats_sender).await;
            });
        }
        manager
    }

    fn with_manager<T: Task + 'static>(
        task_receiver: TaskReceiverFor<T>,
        manager: &Arc<Mutex<TaskManager<T>>>,
        result_sender: ResultSenderFor<T>,
    ) {
        TaskManager::<T>::start(manager, task_receiver, result_sender);
    }

    pub fn watch_file(&self, path: PathBuf, response_sender: ResultSenderFor<WatchFileTask>) {
        let (tx, rx) = mpsc::unbounded_channel();
        if let Err(_) = tx.send(WatchFileTask { path }) {
            return;
        }
        Self::with_manager(rx, &self.watch_file_manager, response_sender);
    }

    pub fn load_file(&self, path: PathBuf, response_sender: ResultSenderFor<LoadFileTask>) {
        let (tx, rx) = mpsc::unbounded_channel();
        self.watch_file(path, tx);
        Self::with_manager(rx, &self.load_file_manager, response_sender);
    }

    pub fn lex(
        &self,
        path: PathBuf,
        contents: Option<String>,
        response_sender: ResultSenderFor<LexFileTask>,
    ) {
        let (tx, rx) = mpsc::unbounded_channel();
        if let Some(contents) = contents {
            if let Err(_) = tx.send(LexFileTask { path, contents }) {
                return;
            }
        } else {
            self.load_file(path, tx);
        }
        // TODO: Look into Streams
        Self::with_manager(rx, &self.lex_file_manager, response_sender);
    }

    pub fn parse(
        &self,
        path: PathBuf,
        contents: Option<String>,
        response_sender: ResultSenderFor<ParseFileTask>,
    ) {
        let (tx, rx) = mpsc::unbounded_channel();
        self.lex(path, contents, tx);
        Self::with_manager(rx, &self.parse_file_manager, response_sender);
    }

    pub fn send_command(&self, _cmd: RequestTask, _response_sender: mpsc::UnboundedSender<()>) {}
}
