use super::ui::OptionsTrait;
use crate::ast::Ast;
use crate::primitives::Prim;
use crate::queries::{AnyQuery, FileRef, StatusReport};
use crate::ui::Client;
use log::{debug, trace};
use qbice::{Config, Engine};
use std::fmt::Debug;
use std::sync::Arc;
use tokio::sync::{broadcast, mpsc, oneshot};

#[derive(Debug)]
pub struct Compiler {
    // IDEA: Make a trait...
    query_receiver: mpsc::UnboundedReceiver<(AnyQuery, mpsc::UnboundedSender<Prim>)>,
    // Broadcast the accumulation to all clients.
    pub status_sender: broadcast::Sender<StatusReport>,
    pub status_receiver: broadcast::Receiver<StatusReport>,
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
            query_receiver: request_receiver,
            // TODO(features): More passes:
            // - type_check_inside_module
            // Produces type checked (and optimizable) modules **AND**
            // partially type checked (but) mergable-modules.
            // Pair-wise merging of type checking information???
            // - type_check_merge_module_sets
            // Produces type checked (and optimizable) modules **AND**
            // Partially type checked (but) mergable-modules
            // - optimization
            // - code_generation
            // - binary_generation
            // - load_into_interpreter
            // - run_in_interpreter
            status_sender,
            status_receiver,
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
        use crate::queries::executors::*;
        engine.register_executor(Arc::new(LoadExecutor));
        engine.register_executor(Arc::new(LexExecutor));
        engine.register_executor(Arc::new(ParseFrontMatterExecutor));
        engine.register_executor(Arc::new(HandleImportExecutor));
        engine.register_executor(Arc::new(ParseExecutor));
        engine.register_executor(Arc::new(ResolveAstExecutor));
        engine.register_executor(Arc::new(MacroExpandExecutor));
        engine.register_executor(Arc::new(FindNodeExecutor));
        engine.register_executor(Arc::new(FindDefinitionExecutor));
        engine.register_executor(Arc::new(GetLocationExecutor));
        engine.register_executor(Arc::new(TypeAtExecutor));
        engine.register_executor(Arc::new(TypeCheckExecutor));
        engine.register_executor(Arc::new(GetTypeExecutor));
        engine.register_executor(Arc::new(CheckProofsExecutor));
        engine.register_executor(Arc::new(ErrorsExecutor));
        engine.register_executor(Arc::new(ErrorsAtExecutor));
        engine.register_executor(Arc::new(PrettyPrintExecutor));
        engine.register_executor(Arc::new(InterpretExecutor));
        engine.register_executor(Arc::new(EvalExecutor));
        engine.register_executor(Arc::new(EvalNodeExecutor));
        engine.register_executor(Arc::new(OptimizeExecutor));
        engine.register_executor(Arc::new(LowerExecutor));
        #[cfg(feature = "codegen")]
        {
            use crate::queries::executors::codegen::*;
            engine.register_executor(Arc::new(CodeGenExecutor));
            engine.register_executor(Arc::new(CodeGenAllExecutor));
            engine.register_executor(Arc::new(BuildExecutor));
            engine.register_executor(Arc::new(BuildAllExecutor));
            engine.register_executor(Arc::new(SourceMapGenExecutor));
            engine.register_executor(Arc::new(SourceMapGenAllExecutor));
            engine.register_executor(Arc::new(EnumerateBinariesExecutor));
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

    pub fn watch_file(&self, file: FileRef) {
        if let Err(e) = self.file_watch_request_sender.send(file) {
            debug!("Error while requesting file watching: {e:?}");
        }
    }

    pub async fn run_loop(mut self) {
        trace!("Starting compiler run loop");
        loop {
            trace!("Waiting in compiler run loop");
            tokio::select! {
                Some((cmd, response_sender)) = self.query_receiver.recv() => {
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

    pub fn start_command(&self, cmd: AnyQuery, response_sender: mpsc::UnboundedSender<Prim>) {
        match cmd {
            AnyQuery::Eval { ast, expr } => {
                self.eval(ast, Some(expr), response_sender);
            }
            AnyQuery::Build { files } => {
                for file in files {
                    // TODO(correctness): Handle in-memory files
                    let (_source_zip, mut out_path) = file.clone().to_path_buf(); // TODO(correctness): Merge source zip path and out path.
                    let ast = Arc::new(Ast::new(file.clone()));
                    out_path.set_extension("out");
                    self.codegen(ast, out_path, None, response_sender.clone());
                }
            }
            AnyQuery::RunInterpreter { files } => {
                for file in files {
                    // TODO(compilersEllie): Support context / imports.
                    let ast = Arc::new(Ast::new(file));
                    self.eval(ast, None, response_sender.clone());
                }
            }
        }
    }
}
