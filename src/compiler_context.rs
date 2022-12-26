use crate::cli_options::Options;
use crate::primitives::Prim;
use crate::tasks::manager::TaskManager;
pub use crate::tasks::manager::{StatusReport, TaskStats};
pub use crate::tasks::status::*;
pub use crate::tasks::task_trait::TaskId;
use crate::tasks::task_trait::{ResultSenderFor, Task, TaskReceiverFor};
use crate::tasks::*;
use crate::utils::meta::Meta;
use std::fmt::Debug;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use tokio::sync::{broadcast, mpsc};

#[derive(Debug)]
pub struct Compiler {
    // TODO: Make a trait...
    request_receiver: mpsc::UnboundedReceiver<(RequestTask, mpsc::UnboundedSender<Prim>)>,
    watch_file_manager: Arc<Mutex<TaskManager<WatchFileTask>>>,
    load_file_manager: Arc<Mutex<TaskManager<LoadFileTask>>>,
    lex_file_manager: Arc<Mutex<TaskManager<LexFileTask>>>,
    parse_file_manager: Arc<Mutex<TaskManager<ParseFileTask>>>,
    eval_file_manager: Arc<Mutex<TaskManager<EvalFileTask>>>,
    stats_requester: broadcast::Sender<()>,
    status_sender: broadcast::Sender<StatusReport>,
    result_sender: mpsc::UnboundedSender<Prim>,
    request_sender: mpsc::UnboundedSender<(RequestTask, mpsc::UnboundedSender<Prim>)>,
}

impl Compiler {
    pub fn new(
    ) -> Self {
        let (request_sender, request_receiver) = mpsc::unbounded_channel();
        let (result_sender, result_receiver) = mpsc::unbounded_channel();
        let (stats_sender, stats_receiver) = mpsc::unbounded_channel();
        let (stats_requester, stats_request_receiver) = broadcast::channel(1);
        let (status_sender, status_receiver) = broadcast::channel(1);
        Self {
            request_receiver,
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
            status_sender,
            stats_requester,
            request_sender,
            result_sender,
        }
    }

    fn make_client(&self, options: Options) -> crate::ui::Client {
        use crate::ui::Client;
        Client::new(
            self.stats_requester.clone(),
            self.status_sender.subscribe(),
            self.request_sender.clone(),
            options,
        )
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
        if tx.send(WatchFileTask { path }).is_err() {
            return;
        }
        Self::with_manager(rx, &self.watch_file_manager, response_sender);
    }

    pub fn load_file(&self, path: PathBuf, response_sender: ResultSenderFor<LoadFileTask>) {
        let (tx, rx) = mpsc::unbounded_channel();
        if tx
            .send(LoadFileTask {
                path: path.clone(),
                invalidate: Meta(false),
            })
            .is_err()
        {
            return;
        }
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
            if tx.send(LexFileTask { path, contents }).is_err() {
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

    pub fn eval(
        &self,
        path: PathBuf,
        contents: Option<String>,
        response_sender: ResultSenderFor<EvalFileTask>,
    ) {
        let (tx, rx) = mpsc::unbounded_channel();
        self.parse(path, contents, tx);
        Self::with_manager(rx, &self.eval_file_manager, response_sender);
    }

    pub async fn run_loop(&mut self) {
        while let Some((cmd, response_sender)) = self.request_receiver.recv().await {
            self.start_command(cmd, response_sender);
        }
    }

    pub fn start_command(&self, cmd: RequestTask, response_sender: mpsc::UnboundedSender<Prim>) {
        match cmd {
            RequestTask::EvalLine(line) => {
                self.eval("interpreter.tk".into(), Some(line), response_sender);
            }
            RequestTask::Launch { files } => {
                for file in files {
                    self.eval(file, None, response_sender.clone());
                }
            }
        }
    }
}
