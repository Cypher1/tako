use super::OptionsTrait;
use crate::error::Error;
use crate::primitives::Prim;
use crate::queries::FileRef;
use crate::tasks::{RequestTask, StatusReport, TaskKind, TaskStats};
use log::trace;
use std::collections::{BTreeSet, HashMap};
use tokio::sync::{broadcast, mpsc};

#[derive(Debug)]
pub struct Client {
    pub manager_status: HashMap<TaskKind, TaskStats>,
    pub history: Vec<String>, // TODO(usability): Mark Input v output.
    pub errors_for_file: HashMap<Option<FileRef>, BTreeSet<Error>>,
    pub options: Box<dyn OptionsTrait>,
    stats_requester: broadcast::Sender<()>,
    task_manager_status_receiver: broadcast::Receiver<StatusReport>,
    request_sender: mpsc::UnboundedSender<(RequestTask, mpsc::UnboundedSender<Prim>)>,
    pub result_receiver: mpsc::UnboundedReceiver<Prim>,
    result_sender: mpsc::UnboundedSender<Prim>,
    #[allow(unused)]
    file_watch_requester: mpsc::UnboundedSender<FileRef>,
    #[allow(unused)]
    file_updater: broadcast::Receiver<FileRef>,
}

impl Client {
    #[must_use]
    pub fn new(
        stats_requester: broadcast::Sender<()>,
        task_manager_status_receiver: broadcast::Receiver<StatusReport>,
        request_sender: mpsc::UnboundedSender<(RequestTask, mpsc::UnboundedSender<Prim>)>,
        file_watch_requester: mpsc::UnboundedSender<FileRef>,
        file_updater: broadcast::Receiver<FileRef>,
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
        let files = self.options.files().clone();
        self.send_command(if self.options.interpreter() {
            RequestTask::RunInterpreter { files }
        } else {
            RequestTask::Build { files }
        });
    }

    #[must_use]
    pub fn interactive(&self) -> bool {
        // TODO(usability): Build should have an interactive mode?
        self.options.interactive()
    }

    #[must_use]
    pub fn oneshot(&self) -> bool {
        // TODO(usability): Build should have an interactive mode?
        self.options.oneshot()
    }

    pub fn send_command(&mut self, cmd: RequestTask) {
        if let RequestTask::Eval { ast: _, expr: line } = &cmd {
            self.history.push(line.to_string()); // Maybe assumes a single line?
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
                    let file = err.location.as_ref().map(|loc| loc.file.clone());
                    let errs = self.errors_for_file.entry(file).or_default();
                    errs.insert(err);
                }
                self.manager_status.insert(kind, stats);
            },
            Some(value) = result_receiver.recv() => {
                trace!("Got result value: {value:?}");
                if !self.interactive() {
                    println!("{value:?}");
                }
                self.history.push(format!("> {value:#?}"));
                if self.oneshot() {
                    return true;
                }
            }
        }
        false
    }
}
