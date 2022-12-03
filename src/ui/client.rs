use crate::cli_options::Command;
use crate::cli_options::Options;
use crate::compiler_context::Compiler;
use crate::error::Error;
use crate::primitives::Prim;
use crate::tasks::{RequestTask, StatusReport, TaskKind, TaskStats};
use log::trace;
use std::collections::{BTreeSet, HashMap};
use std::path::PathBuf;
use tokio::sync::broadcast;
use tokio::{self, sync::mpsc};

#[derive(Debug)]
pub struct Client {
    pub manager_status: HashMap<TaskKind, TaskStats>,
    pub history: Vec<String>, // TODO: Mark Input v output.
    pub errors_for_file: HashMap<Option<PathBuf>, BTreeSet<Error>>,
    pub options: Options,
    stats_requester: broadcast::Sender<()>,
    task_manager_status_receiver: mpsc::UnboundedReceiver<StatusReport>,
    compiler: Compiler,
    result_receiver: mpsc::UnboundedReceiver<Prim>,
    result_sender: mpsc::UnboundedSender<Prim>,
}

impl Client {
    pub fn new(
        stats_requester: broadcast::Sender<()>,
        task_manager_status_receiver: mpsc::UnboundedReceiver<StatusReport>,
        compiler: Compiler,
        options: Options,
    ) -> Self {
        let (result_sender, result_receiver) = mpsc::unbounded_channel();
        Self {
            stats_requester,
            task_manager_status_receiver,
            manager_status: HashMap::default(),
            history: Vec::default(),
            errors_for_file: HashMap::default(),
            compiler,
            options,
            result_receiver,
            result_sender,
        }
    }

    pub fn start(&mut self) {
        self.send_command(RequestTask::Launch {
            files: self.options.files.clone(),
        });
    }

    pub fn interactive(&self) -> bool {
        // TODO: Build should have an interactive mode?
        self.options.cmd == Command::Repl && !self.options.minimal_ui
    }

    pub fn oneshot(&self) -> bool {
        // TODO: Build should have an interactive mode?
        self.options.cmd == Command::Build || self.options.cmd == Command::Interpret
    }

    pub fn send_command(&mut self, cmd: RequestTask) {
        if let RequestTask::EvalLine(line) = &cmd {
            self.history.push(line.to_string());
        }
        self.compiler.send_command(cmd, self.result_sender.clone());
    }

    pub fn get_stats(&mut self) {
        self.stats_requester.send(()).expect("TODO");
    }

    pub async fn wait_for_updates(&mut self) -> bool {
        let result_receiver = &mut self.result_receiver;
        tokio::select! {
            Some(StatusReport { kind, stats, errors }) = self.task_manager_status_receiver.recv() => {
                trace!("TaskManager status: {kind:?} => {stats}\nerrors: {errors:#?}");
                for (_id, err) in errors {
                    let file = err.location.as_ref().map(|loc| loc.filename.clone());
                    let errs = self.errors_for_file.entry(file).or_insert_with(BTreeSet::new);
                    errs.insert(err);
                }
                self.manager_status.insert(kind, stats);
            },
            Some(value) = result_receiver.recv() => {
                trace!("Got result value: {value:?}");
                if !self.interactive() {
                    println!("{value:?}");
                }
                self.history.push(format!("{value:#?}"));
                if self.oneshot() {
                    return true;
                }
            }
        }
        false
    }
}
