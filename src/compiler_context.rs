use crate::tasks::task_trait::{Task, TaskReceiverFor, TaskSenderFor, ResultReceiverFor, ResultSenderFor};
use crate::tasks::*;
use tokio::sync::{broadcast, mpsc};

use crate::tasks::manager::TaskManager;
pub use crate::tasks::manager::{StatusReport, TaskStats};
pub use crate::tasks::status::*;
pub use crate::tasks::task_trait::TaskId;
use std::fmt::Debug;
use std::path::PathBuf;

#[derive(Debug)]
pub struct Compiler { // TODO: Make a trait...
    stats_sender: mpsc::UnboundedSender<StatusReport>,
    stats_requester: broadcast::Sender<()>,

    watch_file_sender: TaskSenderFor<WatchFileTask>,
    load_file_sender: TaskSenderFor<LoadFileTask>,
    lex_file_sender: TaskSenderFor<LexFileTask>,
    parse_file_sender: TaskSenderFor<ParseFileTask>,
    eval_file_sender: TaskSenderFor<EvalFileTask>,

    watch_file_result_receiver: ResultReceiverFor<WatchFileTask>,
    load_file_result_receiver: ResultReceiverFor<LoadFileTask>,
    lex_file_result_receiver: ResultReceiverFor<LexFileTask>,
    parse_file_result_receiver: ResultReceiverFor<ParseFileTask>,
    eval_file_result_receiver: ResultReceiverFor<EvalFileTask>,
}

impl Compiler {
    pub fn new(
        stats_sender: mpsc::UnboundedSender<StatusReport>,
        stats_requester: broadcast::Sender<()>,
    ) -> Self {
        let (watch_file_sender, watch_file_receiver) = mpsc::unbounded_channel();
        let (load_file_sender, load_file_receiver) = mpsc::unbounded_channel();
        let (lex_file_sender, lex_file_receiver) = mpsc::unbounded_channel();
        let (parse_file_sender, parse_file_receiver) = mpsc::unbounded_channel();
        let (eval_file_sender, eval_file_receiver) = mpsc::unbounded_channel();

        let (watch_file_result_sender, watch_file_result_receiver) = mpsc::unbounded_channel();
        let (load_file_result_sender, load_file_result_receiver) = mpsc::unbounded_channel();
        let (lex_file_result_sender, lex_file_result_receiver) = mpsc::unbounded_channel();
        let (parse_file_result_sender, parse_file_result_receiver) = mpsc::unbounded_channel();
        let (eval_file_result_sender, eval_file_result_receiver) = mpsc::unbounded_channel();

        let this = Self {
            stats_sender,
            stats_requester,

            watch_file_sender,
            load_file_sender,
            lex_file_sender,
            parse_file_sender,
            eval_file_sender,

            watch_file_result_receiver,
            load_file_result_receiver,
            lex_file_result_receiver,
            parse_file_result_receiver,
            eval_file_result_receiver,
        };
        this.launch_manager::<WatchFileTask>(watch_file_receiver, watch_file_result_sender);
        this.launch_manager::<LoadFileTask>(load_file_receiver, load_file_result_sender);
        this.launch_manager::<LexFileTask>(lex_file_receiver, lex_file_result_sender);
        this.launch_manager::<ParseFileTask>(parse_file_receiver, parse_file_result_sender);
        this.launch_manager::<EvalFileTask>(eval_file_receiver, eval_file_result_sender);
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

        this
    }

    fn launch_manager<T: Task + 'static>(
        &self,
        in_channel: TaskReceiverFor<T>,
        out_channel: ResultSenderFor<T>,
    ) {
        let mut manager = TaskManager::<T>::new(
            in_channel,
            out_channel,
            self.stats_sender.clone(),
            self.stats_requester.subscribe(),
        );
        tokio::spawn(async move {
            manager.run_loop().await;
        });
    }

    pub fn watch_file(&self, path: PathBuf, response_sender: ResultSenderFor<WatchFileTask>) {
        self.watch_file_sender.send(WatchFileTask {
            path,
        });
    }

    pub fn load_file(&self, path: PathBuf, response_sender: ResultSenderFor<LoadFileTask>) {
        let (tx, mut rx) = mpsc::unbounded_channel();
        self.watch_file(path, tx);
        let load_file_sender = self.load_file_sender.clone();
        tokio::spawn(async move {
            while let Some(task) = rx.recv().await {
                load_file_sender.send(task);
            }
        });
    }

    pub fn lex(&self, path: PathBuf, contents: Option<String>, response_sender: ResultSenderFor<LexFileTask>) {
        let (tx, mut rx) = mpsc::unbounded_channel();
        if let Some(contents) = contents {
            tx.send(contents);
        } else {
            self.load_file(path.clone(), tx);
        }
        let lex_file_sender = self.lex_file_sender.clone();
        tokio::spawn(async move {
            while let Some(contents) = rx.recv().await {
                lex_file_sender.send(LexFileTask {
                    path: path.clone(),
                    contents,
                });
            }
        });
    }
    pub fn parse(&self, path: PathBuf, contents: Option<String>, response_sender: ResultSenderFor<ParseFileTask>) {
        let (tx, mut rx) = mpsc::unbounded_channel();
        self.lex(path, contents, tx);
        let parse_file_sender = self.parse_file_sender.clone();
        tokio::spawn(async move {
            while let Some(task) = rx.recv().await {
                parse_file_sender.send(task);
            }
        });
    }

    pub fn send_command(&self, cmd: RequestTask, response_sender: mpsc::UnboundedSender<()>) {
    }
}
