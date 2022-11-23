use crate::tasks::task_trait::{TaskReceiverFor, TaskSenderFor, Task};
use crate::tasks::*;
use tokio::sync::{broadcast, mpsc};

pub use crate::tasks::manager::{StatusReport, TaskStats};
pub use crate::tasks::status::*;
use log::{trace, warn};
use notify::{RecursiveMode, Watcher};
use std::fmt::Debug;
use crate::tasks::manager::TaskManager;
pub use crate::tasks::task_trait::TaskId;
use std::path::Path;

#[derive(Debug)]
pub struct Compiler {
    request_receiver: Option<TaskReceiverFor<RequestTask>>,
    result_sender: TaskSenderFor<EvalFileTask>,
    stats_sender: mpsc::UnboundedSender<StatusReport>,
    stats_requester: broadcast::Sender<()>,
}

impl Compiler {
    pub fn new(
        request_receiver: Option<TaskReceiverFor<RequestTask>>,
        result_sender: TaskSenderFor<EvalFileTask>,
        stats_sender: mpsc::UnboundedSender<StatusReport>,
        stats_requester: broadcast::Sender<()>,
    ) -> Self {
        Self {
            request_receiver,
            result_sender,
            stats_sender,
            stats_requester,
        }
    }

    pub async fn launch(mut self) {
        let (any_task_sender, mut any_task_receiver) = mpsc::unbounded_channel();
        let (load_file_sender, load_file_receiver) = mpsc::unbounded_channel();
        let (lex_file_sender, lex_file_receiver) = mpsc::unbounded_channel();
        let (parse_file_sender, parse_file_receiver) = mpsc::unbounded_channel();
        let (eval_file_sender, eval_file_receiver) = mpsc::unbounded_channel();

        {
            let load_file_sender = load_file_sender;
            let lex_file_sender = lex_file_sender.clone();
            let parse_file_sender = parse_file_sender.clone();
            let eval_file_sender = eval_file_sender.clone();
            tokio::spawn(async move {
                let mut watcher = {
                    let load_file_sender = load_file_sender.clone();
                    notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| {
                        match res {
                            Ok(event) => {
                                trace!("event: {:?}", event);
                                for path in event.paths {
                                    load_file_sender
                                        .send(LoadFileTask { path })
                                        .expect("Load file task could not be sent");
                                }
                            }
                            Err(e) => {
                                trace!("watch error: {:?}", e);
                            }
                        }
                    })
                    .expect("Watcher failed to register")
                };
                trace!("Waiting on file changes...");
                watcher
                    .watch(Path::new("test.tk"), RecursiveMode::Recursive)
                    .expect("Should be able to watch files");

                loop {
                    let new_task: Option<AnyTask> = any_task_receiver.recv().await;
                    match new_task {
                        None => break,
                        Some(new_task) => match new_task {
                            AnyTask::Request(_new_task) => {
                                todo!("This shouldn't happen!")
                            }
                            AnyTask::LoadFile(new_task) => {
                                if let Err(e) = load_file_sender.send(new_task) {
                                    warn!("Load file receiver dropped: {}", e);
                                    break;
                                }
                            }
                            AnyTask::LexFile(new_task) => {
                                if let Err(e) = lex_file_sender.send(new_task) {
                                    warn!("Lex file receiver dropped: {}", e);
                                    break;
                                }
                            }
                            AnyTask::ParseFile(new_task) => {
                                if let Err(e) = parse_file_sender.send(new_task) {
                                    warn!("Parse file receiver dropped: {}", e);
                                    break;
                                }
                            }
                            AnyTask::EvalFile(new_task) => {
                                if let Err(e) = eval_file_sender.send(new_task) {
                                    warn!("Eval file receiver dropped: {}", e);
                                    break;
                                }
                            }
                        },
                    }
                }
            });
        }
        let request_receiver = self.request_receiver.take().expect("TODO");
        self.launch_manager::<RequestTask>(request_receiver, any_task_sender);
        self.launch_manager::<LoadFileTask>(load_file_receiver, lex_file_sender);
        self.launch_manager::<LexFileTask>(lex_file_receiver, parse_file_sender);
        self.launch_manager::<ParseFileTask>(parse_file_receiver, eval_file_sender);
        self.launch_manager::<EvalFileTask>(eval_file_receiver, self.result_sender.clone());
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

    fn launch_manager<T: Task + 'static>(&self, in_channel: TaskReceiverFor<T>, out_channel: TaskSenderFor<T>) {
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
}
