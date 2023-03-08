use super::OptionsTrait;
use crate::error::Error;
use crate::primitives::Prim;
use crate::tasks::{RequestTask, StatusReport, TaskKind, TaskStats};
use log::trace;
use std::collections::{BTreeSet, HashMap};
use std::path::PathBuf;
use tokio::sync::{broadcast, mpsc};

#[derive(Debug)]
pub struct Client {
    pub manager_status: HashMap<TaskKind, TaskStats>,
    pub history: Vec<String>, // TODO(usability): Mark Input v output.
    pub errors_for_file: HashMap<Option<PathBuf>, BTreeSet<Error>>,
    pub options: Box<dyn OptionsTrait>,
    stats_requester: broadcast::Sender<()>,
    task_manager_status_receiver: broadcast::Receiver<StatusReport>,
    request_sender: mpsc::UnboundedSender<(RequestTask, mpsc::UnboundedSender<Prim>)>,
    pub result_receiver: mpsc::UnboundedReceiver<Prim>,
    result_sender: mpsc::UnboundedSender<Prim>,
    #[allow(unused)]
    file_watch_requester: mpsc::UnboundedSender<PathBuf>,
    #[allow(unused)]
    file_updater: broadcast::Receiver<PathBuf>,
}

impl Client {
    pub fn new(
        stats_requester: broadcast::Sender<()>,
        task_manager_status_receiver: broadcast::Receiver<StatusReport>,
        request_sender: mpsc::UnboundedSender<(RequestTask, mpsc::UnboundedSender<Prim>)>,
        file_watch_requester: mpsc::UnboundedSender<PathBuf>,
        file_updater: broadcast::Receiver<PathBuf>,
        options: Box<dyn OptionsTrait>,
    ) -> Self {
        let (result_sender, result_receiver) = mpsc::unbounded_channel();
        Self {
            stats_requester,
            task_manager_status_receiver,
            manager_status: HashMap::default(),
            history: Vec::default(),
            errors_for_file: HashMap::default(),
            request_sender,
            options,
            result_receiver,
            result_sender,
            file_watch_requester,
            file_updater,
        }
    }

    pub fn start(&mut self) {
        if self.options.interpreter() {
            self.send_command(RequestTask::RunInterpreter {
                files: self.options.files().clone(),
            });
        } else {
            self.send_command(RequestTask::Build {
                files: self.options.files().clone(),
            });
        }
    }

    pub fn interactive(&self) -> bool {
        // TODO(usability): Build should have an interactive mode?
        self.options.interactive()
    }

    pub fn oneshot(&self) -> bool {
        // TODO(usability): Build should have an interactive mode?
        self.options.oneshot()
    }

    pub fn send_command(&mut self, cmd: RequestTask) {
        if let RequestTask::EvalLine(line) = &cmd {
            self.history.push(line.to_string());
        }
        self.request_sender
            .send((cmd, self.result_sender.clone()))
            .expect("Request sender closed");
    }

    pub fn get_stats(&mut self) {
        self.stats_requester
            .send(())
            .expect("Stats requester closed");
    }

    pub async fn wait_for_updates(&mut self) -> bool {
        let result_receiver = &mut self.result_receiver;
        tokio::select! {
            Ok(StatusReport { kind, stats, errors }) = self.task_manager_status_receiver.recv() => {
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
