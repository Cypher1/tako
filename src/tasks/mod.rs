mod manager;
mod status;
mod task_trait;
use crate::ast::Ast;
use crate::error::Error;
use crate::tokens::Token;
use async_trait::async_trait;
use enum_kinds::EnumKind;
use log::{trace, warn};
use manager::{ManagerConfig, TaskManager};
pub use manager::{StatusReport, TaskStats};
use notify::{RecursiveMode, Watcher};
pub use status::*;
use std::fmt::Debug;
use std::path::PathBuf;
use std::{collections::HashMap, path::Path};
use task_trait::*;
pub use task_trait::TaskId;
use tokio::sync::{broadcast, mpsc};

// TODO: Add timing information, etc.
// TODO: Support re-running multiple times for stability testing.
// TODO: Store the Tasks and their statuses in a contiguous vec.
// TODO: Still use hashing to look up tasks and their IDs.
// This should be the pre-computed hash, to avoid sending and cloning tasks.
pub type TaskResults<T> = HashMap<T, TaskStatus<<T as Task>::Output, Error>>;

#[derive(EnumKind)]
#[enum_kind(TaskKind, derive(Hash, Ord, PartialOrd))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnyTask {
    Request(RequestTask),
    LoadFile(LoadFileTask),
    LexFile(LexFileTask),
    ParseFile(ParseFileTask),
    EvalFile(EvalFileTask),
}

#[derive(Debug)]
pub struct TaskSet {
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

impl TaskSet {
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
                    notify::recommended_watcher(
                        move |res: Result<notify::Event, notify::Error>| match res {
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
                        },
                    ).expect("Watcher failed to register")
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

/// RequestTask represents the task of responding to a set of command line arguments, a request to
/// a compiler daemon (or possibly a response to a file watcher notifying of a change).
///
/// There's normally only one of these, but it seems elegant to have these fit into the `Task` model.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RequestTask {
    Launch { files: Vec<PathBuf> },
    EvalLine(String),
}

#[async_trait]
impl Task for RequestTask {
    type Output = AnyTask;
    const TASK_KIND: TaskKind = TaskKind::Request;

    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>) {
        match &self {
            RequestTask::EvalLine(line) => {
                result_sender
                    .send((
                        self.clone(),
                        Update::NextResult(AnyTask::LexFile(LexFileTask {
                            path: "interpreter.tk".into(),
                            contents: line.to_string(),
                        })),
                    ))
                    .expect("Should be able to send task result to manager");
            }
            RequestTask::Launch { files } => {
                for path in files {
                    result_sender
                        .send((
                            self.clone(),
                            Update::NextResult(AnyTask::LoadFile(LoadFileTask {
                                path: path.clone(),
                            })),
                        ))
                        .expect("Should be able to send task result to manager");
                }
            }
        }
        result_sender
            .send((self, Update::Complete))
            .expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LoadFileTask {
    path: PathBuf,
}

#[async_trait]
impl Task for LoadFileTask {
    type Output = LexFileTask;
    const TASK_KIND: TaskKind = TaskKind::LoadFile;

    fn has_file_path(&self) -> Option<&PathBuf> {
        Some(&self.path)
    }
    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>) {
        // TODO: Use tokio's async read_to_string.
        let contents = std::fs::read_to_string(&self.path);
        let contents = contents
            .map(|contents| LexFileTask {
                path: self.path.clone(),
                contents,
            })
            .map_err(|err| self.decorate_error(err));
        result_sender
            .send((
                self,
                match contents {
                    Ok(result) => Update::FinalResult(result),
                    Err(err) => Update::Failed(err),
                },
            ))
            .expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LexFileTask {
    path: PathBuf,
    contents: String,
}

#[async_trait]
impl Task for LexFileTask {
    type Output = ParseFileTask;
    const TASK_KIND: TaskKind = TaskKind::LexFile;

    fn has_file_path(&self) -> Option<&PathBuf> {
        Some(&self.path)
    }
    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>) {
        let tokens = crate::tokens::lex(&self.contents);
        let tokens = tokens
            .map(|tokens| ParseFileTask {
                path: self.path.clone(),
                contents: self.contents.clone(),
                tokens,
            })
            .map_err(|err| self.decorate_error(err));
        result_sender
            .send((
                self,
                match tokens {
                    Ok(result) => Update::FinalResult(result),
                    Err(err) => Update::Failed(err),
                },
            ))
            .expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ParseFileTask {
    path: PathBuf,
    contents: String,
    tokens: Vec<Token>,
}

#[async_trait]
impl Task for ParseFileTask {
    type Output = EvalFileTask; // For now, we'll just store the AST itself.
    const TASK_KIND: TaskKind = TaskKind::ParseFile;

    fn has_file_path(&self) -> Option<&PathBuf> {
        Some(&self.path)
    }
    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>) {
        tokio::task::spawn_blocking(move || {
            let ast = crate::parser::parse(&self.path, &self.tokens)
                .map_err(|err| self.decorate_error(err));
            result_sender
                .send((
                    self.clone(),
                    match ast {
                        Ok(result) => Update::FinalResult(EvalFileTask {
                            path: self.path.to_path_buf(),
                            ast: result,
                        }),
                        Err(err) => Update::Failed(err),
                    },
                ))
                .expect("Should be able to send task result to manager");
        });
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct EvalFileTask {
    path: PathBuf,
    ast: Ast,
}

#[async_trait]
impl Task for EvalFileTask {
    type Output = crate::primitives::Prim; // For now, we'll just store an updated AST itself.
    const TASK_KIND: TaskKind = TaskKind::EvalFile;
    const RESULT_IS_CACHABLE: bool = false;

    fn has_file_path(&self) -> Option<&PathBuf> {
        Some(&self.path)
    }
    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>) {
        tokio::task::spawn_blocking(move || {
            let result = crate::interpreter::run(&self.path, &self.ast)
                .map_err(|err| self.decorate_error(err));
            result_sender
                .send((
                    self,
                    match result {
                        Ok(result) => Update::FinalResult(result),
                        Err(err) => Update::Failed(err),
                    },
                ))
                .expect("Should be able to send task result to manager");
        });
    }
}
