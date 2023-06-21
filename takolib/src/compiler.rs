use super::ui::OptionsTrait;
use crate::primitives::meta::Meta;
use crate::primitives::Prim;
use crate::tasks::manager::TaskManager;
pub use crate::tasks::manager::{StatusReport, TaskStats};
pub use crate::tasks::status::*;
pub use crate::tasks::task_trait::TaskId;
use crate::tasks::task_trait::{ResultSenderFor, Task, TaskReceiverFor};
use crate::tasks::*;
use crate::ui::Client;
use log::{debug, trace};
use std::fmt::Debug;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use tokio::spawn;
use tokio::sync::{broadcast, mpsc, oneshot};

#[derive(Debug)]
pub struct Compiler {
    // IDEA: Make a trait...
    request_receiver: mpsc::UnboundedReceiver<(RequestTask, mpsc::UnboundedSender<Prim>)>,
    load_file_manager: Arc<Mutex<TaskManager<LoadFileTask>>>,
    lex_file_manager: Arc<Mutex<TaskManager<LexFileTask>>>,
    parse_file_manager: Arc<Mutex<TaskManager<ParseFileTask>>>,
    desugar_file_manager: Arc<Mutex<TaskManager<DesugarFileTask>>>,
    eval_file_manager: Arc<Mutex<TaskManager<EvalFileTask>>>,
    codegen_manager: Arc<Mutex<TaskManager<CodegenTask>>>,
    // Broadcast the accumulation to all clients.
    pub stats_requester: broadcast::Sender<()>,
    pub status_sender: broadcast::Sender<StatusReport>,
    pub status_receiver: broadcast::Receiver<StatusReport>,
    request_sender: mpsc::UnboundedSender<(RequestTask, mpsc::UnboundedSender<Prim>)>,
    #[allow(unused)]
    stats_receiver: mpsc::UnboundedReceiver<StatusReport>,
    #[allow(unused)]
    stats_request_receiver: broadcast::Receiver<()>,
    file_watch_request_sender: mpsc::UnboundedSender<PathBuf>,
    #[allow(unused)]
    file_watch_request_receiver: mpsc::UnboundedReceiver<PathBuf>,
    file_update_sender: broadcast::Sender<PathBuf>,
    #[allow(unused)]
    file_update_receiver: broadcast::Receiver<PathBuf>,
    // TODO(clarity): Make pub fields private and add methods.
    pub client_launch_request_sender:
        mpsc::UnboundedSender<(oneshot::Sender<Client>, Box<dyn OptionsTrait>)>,
    client_launch_request_receiver:
        mpsc::UnboundedReceiver<(oneshot::Sender<Client>, Box<dyn OptionsTrait>)>,
}

impl Default for Compiler {
    fn default() -> Self {
        let (request_sender, request_receiver) = mpsc::unbounded_channel();
        let (stats_sender, stats_receiver) = mpsc::unbounded_channel();
        let (stats_requester, stats_request_receiver) = broadcast::channel(1);
        let (status_sender, status_receiver) = broadcast::channel(1);
        let (file_watch_request_sender, file_watch_request_receiver) = mpsc::unbounded_channel();
        let (file_update_sender, file_update_receiver) = broadcast::channel(1000);
        let (client_launch_request_sender, client_launch_request_receiver) =
            mpsc::unbounded_channel();
        Self {
            request_receiver,
            load_file_manager: Self::manager(&stats_sender, &stats_requester),
            lex_file_manager: Self::manager(&stats_sender, &stats_requester),
            parse_file_manager: Self::manager(&stats_sender, &stats_requester),
            desugar_file_manager: Self::manager(&stats_sender, &stats_requester),
            eval_file_manager: Self::manager(&stats_sender, &stats_requester),
            codegen_manager: Self::manager(&stats_sender, &stats_requester),
            // TODO(features): More passes:
            // - type_check_inside_module: TaskManager<>,
            // Produces type checked (and optimizable) modules **AND**
            // partially type checked (but) mergable-modules.
            // Pair-wise merging of type checking information???
            // - type_check_merge_module_sets: TaskManager<>,
            // Produces type checked (and optimizable) modules **AND**
            // Partially type checked (but) mergable-modules
            // - optimization: TaskManager<>,
            // - code_generation: TaskManager<>,
            // - binary_generation: TaskManager<>,
            // - load_into_interpreter: TaskManager<>,
            // - run_in_interpreter: TaskManager<>,
            status_sender,
            status_receiver,
            stats_request_receiver,
            stats_receiver,
            stats_requester,
            request_sender,
            file_watch_request_sender,
            file_watch_request_receiver,
            file_update_sender,
            file_update_receiver,
            client_launch_request_sender,
            client_launch_request_receiver,
        }
    }
}

impl Compiler {
    pub fn make_client(&self, options: Box<dyn OptionsTrait>) -> crate::ui::Client {
        Client::new(
            self.stats_requester.clone(),
            self.status_sender.subscribe(),
            self.request_sender.clone(),
            self.file_watch_request_sender.clone(),
            self.file_update_sender.subscribe(),
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
            spawn(async move {
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

    pub fn watch_file(&self, path: PathBuf) {
        if let Err(e) = self.file_watch_request_sender.send(path) {
            debug!("Error while requesting file watching: {e:?}");
        }
    }

    pub fn load_file(&self, path: PathBuf, response_sender: ResultSenderFor<LoadFileTask>) {
        self.watch_file(path.clone());
        let (tx, rx) = mpsc::unbounded_channel();
        let mut file_update_receiver = self.file_update_sender.subscribe();
        if let Err(e) = tx.send(LoadFileTask {
            path: path.clone(),
            invalidate: Meta(false),
        }) {
            debug!("Error while posting file load task: {e:?}");
        }
        spawn(async move {
            while let Ok(updated_path) = file_update_receiver.recv().await {
                if path != updated_path {
                    continue;
                }
                if let Err(e) = tx.send(LoadFileTask {
                    path: path.clone(),
                    invalidate: Meta(true),
                }) {
                    debug!("Error while posting file load task: {e:?}");
                    return;
                }
            }
        });
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
        // IDEA: Look into Streams
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

    pub fn desugar(
        &self,
        path: PathBuf,
        contents: Option<String>,
        response_sender: ResultSenderFor<DesugarFileTask>,
    ) {
        let (tx, rx) = mpsc::unbounded_channel();
        self.parse(path, contents, tx);
        Self::with_manager(rx, &self.desugar_file_manager, response_sender);
    }

    pub fn eval(
        &self,
        path: PathBuf,
        contents: Option<String>,
        response_sender: ResultSenderFor<EvalFileTask>,
    ) {
        let (tx, rx) = mpsc::unbounded_channel();
        self.desugar(path, contents, tx);
        Self::with_manager(rx, &self.eval_file_manager, response_sender);
    }

    pub fn codegen(
        &self,
        path: PathBuf,
        out_path: PathBuf,
        contents: Option<String>,
        response_sender: ResultSenderFor<CodegenTask>,
    ) {
        let (tx1, mut rx1) = mpsc::unbounded_channel();
        // TODO: Static checking should be here.
        self.desugar(path, contents, tx1);
        let (tx2, rx2) = mpsc::unbounded_channel();
        spawn(async move {
            // TODO: Use a proper map from in paths to out paths.
            while let Some(EvalFileTask { path: _, ast, root }) = rx1.recv().await {
                let mut roots = vec![];
                if let Some(root) = root {
                    // TODO: Support from CLI
                    roots.push(root); // Build only the selected root.
                } else {
                    roots.extend(ast.roots.iter().cloned()); // Build all
                }
                for root in roots {
                    tx2.send(CodegenTask {
                        path: out_path.clone(),
                        ast: ast.clone(),
                        root,
                    })
                    .expect("Should be able to send codegen task");
                }
            }
        });
        Self::with_manager(rx2, &self.codegen_manager, response_sender);
    }

    pub async fn run_loop(mut self) {
        trace!("Starting compiler run loop");
        loop {
            trace!("Waiting in compiler run loop");
            tokio::select! {
                Some((cmd, response_sender)) = self.request_receiver.recv() => {
                    trace!("Got request");
                    self.start_command(cmd, response_sender);
                }
                Some((tx, options)) = self.client_launch_request_receiver.recv() => {
                    trace!("Got client launch");
                    if let Err(e) = tx.send(self.make_client(options)) {
                        trace!("Client request channel closed before client could be sent: {e:?}");
                    }
                }
            }
        }
    }

    pub fn start_command(&self, cmd: RequestTask, response_sender: mpsc::UnboundedSender<Prim>) {
        match cmd {
            RequestTask::EvalLine(line) => {
                self.eval("interpreter.tk".into(), Some(line), response_sender);
            }
            RequestTask::Build { files } => {
                for file in files {
                    let mut file_with_extension = file.to_path_buf();
                    file_with_extension.set_extension("out");
                    self.codegen(file, file_with_extension, None, response_sender.clone());
                }
            }
            RequestTask::RunInterpreter { files } => {
                for file in files {
                    self.eval(file, None, response_sender.clone());
                }
            }
        }
    }
}
