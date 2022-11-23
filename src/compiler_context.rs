use crate::tasks::task_trait::{TaskReceiverFor, TaskSenderFor};
use crate::tasks::*;
use tokio::sync::{broadcast, mpsc};

pub use crate::tasks::manager::{StatusReport, TaskStats};
pub use crate::tasks::status::*;
use log::{trace, warn};
use notify::{RecursiveMode, Watcher};
use std::fmt::Debug;

use crate::tasks::manager::{ManagerConfig, TaskManager};
pub use crate::tasks::task_trait::TaskId;
use crate::tasks::task_trait::*;
use std::path::Path;

#[derive(Debug)]
pub struct Compiler {
    // TODO: Track the jobs that are being done...
    // Invalidate these if
    request_tasks: TaskManager<RequestTask>,
    load_file_tasks: TaskManager<LoadFileTask>,
    lex_file_tasks: TaskManager<LexFileTask>,
    parse_file_tasks: TaskManager<ParseFileTask>,
    eval_file_tasks: TaskManager<EvalFileTask>,
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

impl Compiler {
    pub fn new(
        request_receiver: TaskReceiverFor<RequestTask>,
        result_sender: TaskSenderFor<EvalFileTask>,
        stats_sender: mpsc::UnboundedSender<StatusReport>,
        stats_requester: &broadcast::Sender<()>,
    ) -> Self {
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

        let request_tasks = Self::create::<RequestTask>(
            request_receiver,
            any_task_sender,
            stats_sender.clone(),
            stats_requester.subscribe(),
            ManagerConfig::default(),
        );

        let load_file_tasks = Self::create::<LoadFileTask>(
            load_file_receiver,
            lex_file_sender,
            stats_sender.clone(),
            stats_requester.subscribe(),
            ManagerConfig::default(),
        );

        let lex_file_tasks = Self::create::<LexFileTask>(
            lex_file_receiver,
            parse_file_sender,
            stats_sender.clone(),
            stats_requester.subscribe(),
            ManagerConfig::default(),
        );
        let parse_file_tasks = Self::create::<ParseFileTask>(
            parse_file_receiver,
            eval_file_sender,
            stats_sender.clone(),
            stats_requester.subscribe(),
            ManagerConfig::default(),
        );
        let eval_file_tasks = Self::create::<EvalFileTask>(
            eval_file_receiver,
            result_sender,
            stats_sender,
            stats_requester.subscribe(),
            ManagerConfig::default(),
        );

        Self {
            request_tasks,
            load_file_tasks,
            lex_file_tasks,
            parse_file_tasks,
            eval_file_tasks,
        }
    }

    pub fn create<T: Task + 'static>(
        task_receiver: TaskReceiverFor<T>,
        result_sender: TaskSenderFor<T>,
        stats_sender: mpsc::UnboundedSender<StatusReport>,
        stats_requester: broadcast::Receiver<()>,
        config: ManagerConfig,
    ) -> TaskManager<T> {
        TaskManager::<T>::new(
            task_receiver,
            result_sender,
            stats_sender,
            stats_requester,
            config,
        )
    }

    pub async fn launch(self) {
        let Self {
            mut request_tasks,
            mut load_file_tasks,
            mut lex_file_tasks,
            mut parse_file_tasks,
            mut eval_file_tasks,
        } = self;
        // Launch all of the task managers!
        tokio::spawn(async move {
            request_tasks.run_loop().await;
        });
        tokio::spawn(async move {
            load_file_tasks.run_loop().await;
        });
        tokio::spawn(async move {
            lex_file_tasks.run_loop().await;
        });
        tokio::spawn(async move {
            parse_file_tasks.run_loop().await;
        });
        tokio::spawn(async move {
            eval_file_tasks.run_loop().await;
        });
    }
}
