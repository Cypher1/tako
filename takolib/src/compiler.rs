use super::ui::OptionsTrait;
use crate::ast::Ast;
use crate::primitives::meta::Meta;
use crate::primitives::Prim;
use crate::queries::FileRef;
use crate::tasks::manager::TaskManager;
pub use crate::tasks::manager::{StatusReport, TaskStats};
pub use crate::tasks::status::*;
pub use crate::tasks::task_trait::TaskId;
use crate::tasks::task_trait::{ResultSenderFor, Task, TaskReceiverFor};
use crate::tasks::{
    CodegenTask, DesugarFileTask, EvalFileTask, LexFileTask, LoadFileTask, LowerFileTask,
    ParseFileTask, RequestTask,
};
use crate::ui::Client;
use log::{debug, trace};
#[cfg(not(feature = "rocksdb"))]
use qbice::{Config, Engine};
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
    lower_file_manager: Arc<Mutex<TaskManager<LowerFileTask>>>,
    eval_file_manager: Arc<Mutex<TaskManager<EvalFileTask>>>,
    codegen_manager: Arc<Mutex<TaskManager<CodegenTask>>>,
    // Broadcast the accumulation to all clients.
    pub task_stats_requester: broadcast::Sender<()>,
    pub status_sender: broadcast::Sender<StatusReport>,
    pub status_receiver: broadcast::Receiver<StatusReport>,
    request_sender: mpsc::UnboundedSender<(RequestTask, mpsc::UnboundedSender<Prim>)>,
    #[allow(unused)]
    task_stats_receiver: mpsc::UnboundedReceiver<StatusReport>,
    #[allow(unused)]
    task_stats_request_receiver: broadcast::Receiver<()>,
    file_watch_request_sender: mpsc::UnboundedSender<FileRef>,
    #[allow(unused)]
    file_watch_request_receiver: mpsc::UnboundedReceiver<FileRef>,
    file_update_sender: broadcast::Sender<FileRef>,
    #[allow(unused)]
    file_update_receiver: broadcast::Receiver<FileRef>,
    // TODO(clarity): Make pub fields private and add methods.
    pub client_launch_request_sender:
        mpsc::UnboundedSender<(oneshot::Sender<Client>, Box<dyn OptionsTrait>)>,
    client_launch_request_receiver:
        mpsc::UnboundedReceiver<(oneshot::Sender<Client>, Box<dyn OptionsTrait>)>,
    // TODO(wip): Port all to qbice
}

impl Default for Compiler {
    fn default() -> Self {
        let (request_sender, request_receiver) = mpsc::unbounded_channel();
        let (task_stats_sender, task_stats_receiver) = mpsc::unbounded_channel();
        let (task_stats_requester, task_stats_request_receiver) = broadcast::channel(1);
        let (status_sender, status_receiver) = broadcast::channel(1);
        let (file_watch_request_sender, file_watch_request_receiver) = mpsc::unbounded_channel();
        let (file_update_sender, file_update_receiver) = broadcast::channel(1000);
        let (client_launch_request_sender, client_launch_request_receiver) =
            mpsc::unbounded_channel();
        Self {
            request_receiver,
            load_file_manager: Self::manager(&task_stats_sender, &task_stats_requester),
            lex_file_manager: Self::manager(&task_stats_sender, &task_stats_requester),
            parse_file_manager: Self::manager(&task_stats_sender, &task_stats_requester),
            desugar_file_manager: Self::manager(&task_stats_sender, &task_stats_requester),
            lower_file_manager: Self::manager(&task_stats_sender, &task_stats_requester),
            eval_file_manager: Self::manager(&task_stats_sender, &task_stats_requester),
            codegen_manager: Self::manager(&task_stats_sender, &task_stats_requester),
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
            task_stats_request_receiver,
            task_stats_receiver,
            task_stats_requester,
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
    pub async fn get_engine() -> Arc<Engine<impl Config>> {
        use std::sync::Arc;

        // Create and configure the engine
        let mut engine = crate::engine::get_qbice_engine().await;
        engine.register_executor(Arc::new(crate::queries::executors::LoadExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::LexExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::ParseFrontMatterExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::HandleImportExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::ParseExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::ResolveAstExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::MacroExpandExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::FindNodeExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::FindDefinitionExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::GetLocationExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::TypeAtExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::TypeCheckExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::GetTypeExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::CheckProofsExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::ErrorsExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::ErrorsAtExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::ErrorsForNodeExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::PrettyPrintExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::InterpretExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::EvalExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::EvalNodeExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::OptimizeExecutor));
        engine.register_executor(Arc::new(crate::queries::executors::LowerExecutor));
        #[cfg(feature = "codegen")]
        {
            engine.register_executor(Arc::new(crate::queries::executors::CodeGenAllExecutor));
            engine.register_executor(Arc::new(crate::queries::executors::WriteCodeGenAllExecutor));
            engine.register_executor(Arc::new(crate::queries::executors::EnnumerateBinariesExecutor));
            engine.register_executor(Arc::new(crate::queries::executors::WriteCodeGenExecutor));
            engine.register_executor(Arc::new(crate::queries::executors::CodeGenExecutor));
            engine.register_executor(Arc::new(crate::queries::executors::SourceMapGenExecutor));
        }
        Arc::new(engine)
    }

    #[must_use]
    pub fn make_client(&self, options: Box<dyn OptionsTrait>) -> crate::ui::Client {
        Client::new(
            self.task_stats_requester.clone(),
            self.status_sender.subscribe(),
            self.request_sender.clone(),
            self.file_watch_request_sender.clone(),
            self.file_update_sender.subscribe(),
            options,
        )
    }

    fn manager<T: Task + 'static>(
        task_stats_sender: &mpsc::UnboundedSender<StatusReport>,
        task_stats_requester: &broadcast::Sender<()>,
    ) -> Arc<Mutex<TaskManager<T>>> {
        let manager = Arc::new(Mutex::new(TaskManager::<T>::new()));
        {
            let manager = manager.clone();
            let task_stats_sender = task_stats_sender.clone();
            let task_stats_requester = task_stats_requester.subscribe();
            spawn(async move {
                TaskManager::report_stats(manager, task_stats_requester, task_stats_sender).await;
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

    pub fn watch_file(&self, file: FileRef) {
        if let Err(e) = self.file_watch_request_sender.send(file) {
            debug!("Error while requesting file watching: {e:?}");
        }
    }

    pub fn load_file(&self, file: FileRef, response_sender: ResultSenderFor<LoadFileTask>) {
        self.watch_file(file.clone());
        let (tx, rx) = mpsc::unbounded_channel();
        let mut file_update_receiver = self.file_update_sender.subscribe();
        if let Err(e) = tx.send(LoadFileTask {
            file: file.clone(),
            invalidate: Meta(false),
        }) {
            debug!("Error while posting file load task: {e:?}");
        }
        spawn(async move {
            while let Ok(updated_file) = file_update_receiver.recv().await {
                if file != updated_file {
                    continue;
                }
                if let Err(e) = tx.send(LoadFileTask {
                    file: file.clone(),
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
        file: FileRef,
        contents: Option<String>,
        response_sender: ResultSenderFor<LexFileTask>,
    ) {
        let (tx, rx) = mpsc::unbounded_channel();
        if let Some(contents) = contents {
            if tx.send(LexFileTask { file, contents }).is_err() {
                return;
            }
        } else {
            self.load_file(file, tx);
        }
        // IDEA: Look into Streams
        Self::with_manager(rx, &self.lex_file_manager, response_sender);
    }

    pub fn parse(
        &self,
        og_ast: Arc<Ast>,
        og_contents: Option<String>,
        response_sender: ResultSenderFor<ParseFileTask>,
    ) {
        let (tx1, mut rx1) = mpsc::unbounded_channel();
        self.lex(og_ast.fileref.clone(), og_contents, tx1);
        let (tx2, rx2) = mpsc::unbounded_channel();
        spawn(async move {
            // TODO: Use a proper map from in files to out files.
            while let Some(ParseFileTask {
                ast: _,
                contents,
                tokens,
            }) = rx1.recv().await
            {
                tx2.send(ParseFileTask {
                    ast: og_ast.clone(),
                    contents,
                    tokens,
                })
                .expect("Should be able to send codegen task");
            }
        });
        Self::with_manager(rx2, &self.parse_file_manager, response_sender);
    }

    pub fn desugar(
        &self,
        ast: Arc<Ast>,
        contents: Option<String>,
        response_sender: ResultSenderFor<DesugarFileTask>,
    ) {
        let (tx, rx) = mpsc::unbounded_channel();
        self.parse(ast, contents, tx);
        Self::with_manager(rx, &self.desugar_file_manager, response_sender);
    }

    pub fn lower(
        &self,
        og_ast: Arc<Ast>,
        contents: Option<String>,
        response_sender: ResultSenderFor<LowerFileTask>,
    ) {
        let (tx1, mut rx1) = mpsc::unbounded_channel();
        // TODO: Static checking should be here.
        self.desugar(og_ast, contents, tx1);
        let (tx2, rx2) = mpsc::unbounded_channel();
        spawn(async move {
            // TODO: Use a proper map from in files to out files.
            while let Some(EvalFileTask { ast: new_ast, root }) = rx1.recv().await {
                let root = if let Some(root) = root {
                    root
                } else {
                    new_ast.roots[0]
                };
                tx2.send(LowerFileTask {
                    ast: new_ast.clone(),
                    root,
                })
                .expect("Should be able to send codegen task");
            }
        });
        Self::with_manager(rx2, &self.lower_file_manager, response_sender);
    }

    pub fn eval(
        &self,
        ast: Arc<Ast>,
        contents: Option<String>,
        response_sender: ResultSenderFor<EvalFileTask>,
    ) {
        let (tx, rx) = mpsc::unbounded_channel();
        self.desugar(ast, contents, tx);
        Self::with_manager(rx, &self.eval_file_manager, response_sender);
    }

    pub fn codegen(
        &self,
        og_ast: Arc<Ast>,
        out_path: PathBuf,
        contents: Option<String>,
        response_sender: ResultSenderFor<CodegenTask>,
    ) {
        let (tx1, mut rx1) = mpsc::unbounded_channel();
        // TODO: Static checking should be here.
        self.lower(og_ast, contents, tx1);
        let (tx2, rx2) = mpsc::unbounded_channel();
        spawn(async move {
            // TODO: Use a proper map from in files to out files.
            while let Some(CodegenTask {
                out_path: _,
                ast,
                root,
                lowered,
            }) = rx1.recv().await
            {
                tx2.send(CodegenTask {
                    out_path: out_path.clone(),
                    ast,
                    lowered: lowered.clone(),
                    root,
                })
                .expect("Should be able to send codegen task");
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
            RequestTask::Eval { ast, expr } => {
                self.eval(ast, Some(expr), response_sender);
            }
            RequestTask::Build { files } => {
                for file in files {
                    // TODO(correctness): Handle in-memory files
                    let (_source_zip, mut out_path) = file.clone().to_path_buf(); // TODO(correctness): Merge source zip path and out path.
                    let ast = Arc::new(Ast::new(file.clone()));
                    out_path.set_extension("out");
                    self.codegen(ast, out_path, None, response_sender.clone());
                }
            }
            RequestTask::RunInterpreter { files } => {
                for file in files {
                    // TODO(cypher1): Support context / imports.
                    let ast = Arc::new(Ast::new(file));
                    self.eval(ast, None, response_sender.clone());
                }
            }
        }
    }
}
