use super::OptionsTrait;
use crate::{error::Error, queries::AnyQuery};
use crate::primitives::Prim;
use crate::queries::{Eval, FileRef, Interpret, StatusReport};
use log::trace;
use std::collections::{BTreeSet, HashMap};
use tokio::sync::{broadcast, mpsc};

#[derive(Debug)]
pub struct Client {
    pub history: Vec<String>, // TODO(usability): Mark Input v output.
    pub errors_for_file: HashMap<Option<FileRef>, BTreeSet<Error>>,
    pub options: Box<dyn OptionsTrait>,
    stats_requester: broadcast::Sender<()>,
    request_sender: mpsc::UnboundedSender<(AnyQuery, mpsc::UnboundedSender<Prim>)>,
    #[allow(unused)]
    file_watch_requester: mpsc::UnboundedSender<FileRef>,
    #[allow(unused)]
    file_updater: broadcast::Receiver<FileRef>,
}

impl Client {
    #[must_use]
    pub fn new(
        stats_requester: broadcast::Sender<()>,
        request_sender: mpsc::UnboundedSender<(AnyQuery, mpsc::UnboundedSender<Prim>)>,
        file_watch_requester: mpsc::UnboundedSender<FileRef>,
        file_updater: broadcast::Receiver<FileRef>,
        options: Box<dyn OptionsTrait>,
    ) -> Self {
        Self {
            stats_requester,
            history: Vec::default(),
            errors_for_file: HashMap::default(),
            request_sender,
            options,
            file_watch_requester,
            file_updater,
        }
    }

    pub fn start(&mut self) -> String {
        let entry = self.options.file();
        let start = self.options.start();
        self.send_command(if self.options.interpreter() {
            AnyQuery::InterpretQuery(Interpret { entry, start })
        } else {
            AnyQuery::Build { file: entry }
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

    pub fn send_command(&mut self, cmd: AnyQuery) -> String {
        if let AnyQuery::EvalQuery(Eval { entry, entry_name }) = &cmd {
            let line = format!("{entry}");
            self.history.push(line); // Maybe assumes a single line?
        }
        let (tx, rx) = tokio::sync::oneshot::channel();
        self.request_sender
            .send((cmd, tx))
            .expect("Request sender closed");
        rx.await
            .expect("No response")
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
