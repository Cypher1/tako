mod manager;
mod status;
mod task_trait;
use std::fmt::Debug;
use crate::ast::Ast;
use crate::cli_options::Options;
use crate::error::Error;
use crate::tokens::Token;
use async_trait::async_trait;
use log::trace;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use tokio::sync::{mpsc, RwLock};

use status::*;
use task_trait::*;

// TODO: Add timing information, etc.
// TODO: Support re-running multiple times for stability testing.
// TODO: Store the Tasks and their statuses in a contiguous vec.
// TODO: Still use hashing to look up tasks and their IDs.
// This should be the pre-computed hash, to avoid sending and cloning tasks.
pub type TaskResults<T> = HashMap<T, TaskStatus<<T as Task>::Output, Error>>;

#[derive(Default, Debug)]
pub struct TaskStats {
    num_requests: u32,
    total_num_results: u32,
    num_already_running: u32,
    num_cached: u32,
    num_failed: u32,
    num_succeeded: u32,
}

#[derive(Debug, Default)]
pub struct ManagerConfig {
    disable_caching: bool,
    // TODO: Probably should be able to enable or disable stat & timing collection.
}

#[derive(Debug)]
pub struct TaskManager<T: Debug + Task> {
    result_store: Arc<Mutex<TaskResults<T>>>,
    task_receiver: ReceiverFor<T>,
    result_sender: SenderFor<T>,
    // status_sender: mpsc::Sender<ManagerStats>,
    stats: Arc<Mutex<TaskStats>>,
    config: Arc<RwLock<ManagerConfig>>, // Use a RwLock so that reads don't block.
}

impl<T: Debug + Task + 'static> TaskManager<T> {
    fn task_name() -> &'static str {
        let name = std::any::type_name::<T>();
        let last_lt = name.rfind('<').unwrap_or(name.len());
        let index = name.rfind(':').map(|i|i+1).unwrap_or(1);
        &name[index..last_lt]
    }

    pub fn new(
        task_receiver: ReceiverFor<T>,
        result_sender: SenderFor<T>,
        config: ManagerConfig,
    ) -> Self {
        Self {
            result_store: Arc::new(Mutex::new(TaskResults::new())),
            task_receiver,
            result_sender,
            stats: Arc::new(Mutex::new(TaskStats::default())),
            config: Arc::new(RwLock::new(config)),
        }
    }

    pub async fn run_loop(&mut self) {
        trace!("{} starting run_loop", Self::task_name());
        let (result_or_error_sender, mut result_or_error_receiver) =
            mpsc::unbounded_channel::<(T, Update<T::Output, Error>)>();
        let stats = self.stats.clone();
        let result_store = self.result_store.clone();
        let result_sender = self.result_sender.clone();
        let config = self.config.clone();
        tokio::spawn(async move {
            trace!("{}: Waiting for results...", Self::task_name());
            while let Some((task, update)) = result_or_error_receiver.recv().await {
                trace!(
                    "{} received update from task: {task:#?} {update:#?}",
                    Self::task_name()
                );
                // Reading from an RwLock should be near instant unless there is writing occuring.
                let caching_enabled = !config.read().await.disable_caching;
                let mut result_store = result_store.lock().expect("Should be able to get the result store");
                let mut current_results = result_store.entry(task).or_insert_with(TaskStatus::new);
                let mut is_complete = false;
                let mut error = None;
                {
                    let results_so_far = &mut current_results.results;
                    let mut stats = stats
                        .lock()
                        .expect("Should be able to get task stats store");
                    match update {
                        Update::NextResult(res) => {
                            stats.total_num_results += 1;
                            result_sender
                                .send(res.clone())
                                .expect("Should be able to send results");
                            if caching_enabled {
                                results_so_far.push(res);
                            }
                        }
                        Update::FinalResult(res) => {
                            stats.total_num_results += 1;
                            stats.num_succeeded += 1;
                            is_complete = true;
                            result_sender
                                .send(res.clone())
                                .expect("Should be able to send results");
                            if caching_enabled {
                                results_so_far.push(res);
                            }
                        }
                        Update::Complete => {
                            stats.num_succeeded += 1;
                            is_complete = true;
                        }
                        Update::Failed(err) => {
                            stats.num_failed += 1;
                            is_complete = true; // For completeness...?
                            error = Some(err);
                        }
                    };
                }
                current_results.state = match (error, is_complete) {
                    (Some(err), _is_complete) => TaskState::Failure(err),
                    (None, /*is_complete*/ true) => TaskState::Complete,
                    (None, /*is_complete*/ false) => TaskState::Partial,
                };
            }
            trace!(
                "{} no more results... Finishing listening loop.",
                Self::task_name()
            );
        });

        trace!("{}: Waiting for tasks...", Self::task_name());
        let stats = self.stats.clone();
        'listening: while let Some(task) = self.task_receiver.recv().await {
            // Get a new job from 'upstream'.
            // trace!("{} received task: {task:#?}", Self::task_name());
            {
                let mut stats = stats
                    .lock()
                    .expect("Should be able to get task stats store");
                    stats.num_requests += 1;
            }
            let status = {
                let mut result_store = self
                    .result_store
                    .lock()
                    .expect("Should be able to get the result store");
                // We'll need to forward these on, so we can clone now and drop the result_store lock earlier!
                let status = result_store
                    .entry(task.clone())
                    .or_insert_with(TaskStatus::new);
                if status.state != TaskState::New {
                    let mut stats = stats
                        .lock()
                        .expect("Should be able to get task stats store");
                    stats.num_already_running += 1;
                    continue 'listening; // Already running.
                }
                status.state = TaskState::Running;
                status.clone()
            };
            match (&status.state, T::RESULT_IS_CACHABLE) {
                // TODO: Consider that partial results 'should' still be safe to re-use and could pre-start later work.
                /* TaskState::Partial | */
                (TaskState::Complete, true) => {
                    let mut stats = stats
                        .lock()
                        .expect("Should be able to get task stats store");
                    stats.num_cached += 1;
                    trace!("{} cached task: {task:#?}", Self::task_name());
                    for result in status.results {
                        self.result_sender
                            .send(result)
                            .expect("Should be able to send results");
                    }
                    continue 'listening; // i.e. go look for another task.
                }
                (TaskState::Complete, false) => {
                    trace!(
                        "{} un-cacheable (will re-run): {task:#?}",
                        Self::task_name()
                    );
                }
                // Continue on and re-launch the job, duplicated work should not propagate if completed.
                _ => {
                    trace!(
                        "{} task with status {status:#?}: {task:#?}",
                        Self::task_name()
                    );
                }
            }
            // Launch the job!!!
            let result_or_error_sender = result_or_error_sender.clone();
            tokio::spawn(async move {
                // Tasks will report that they are running. Do not report them here.
                task.perform(result_or_error_sender).await;
            });
        }
        trace!("{} no more tasks... Finishing run_loop: {}", Self::task_name(),
                {
                    let stats = self.stats
                        .lock()
                        .expect("Should be able to get task stats store");
                    format!("{:?}", &stats)
                }
            );
    }
}

#[derive(Debug)]
pub struct TaskSet {
    // TODO: Track the jobs that are being done...
    // Invalidate these if
    request_tasks: TaskManager<LaunchTask>,
    load_file_tasks: TaskManager<LoadFileTask>,
    lex_file_tasks: TaskManager<LexFileTask>,
    parse_file_tasks: TaskManager<ParseFileTask>,
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
    pub fn new(
        launch_receiver: ReceiverFor<LaunchTask>,
        result_sender: SenderFor<ParseFileTask>,
        _options: Arc<Mutex<Options>>,
    ) -> Self {
        let (load_file_sender, load_file_receiver) = mpsc::unbounded_channel();
        let request_tasks =
            TaskManager::<LaunchTask>::new(launch_receiver, load_file_sender, ManagerConfig::default());

        let (lex_file_sender, lex_file_receiver) = mpsc::unbounded_channel();
        let load_file_tasks =
            TaskManager::<LoadFileTask>::new(load_file_receiver, lex_file_sender, ManagerConfig::default());

        let (parse_file_sender, parse_file_receiver) = mpsc::unbounded_channel();
        let lex_file_tasks =
            TaskManager::<LexFileTask>::new(lex_file_receiver, parse_file_sender, ManagerConfig::default());
        let parse_file_tasks =
            TaskManager::<ParseFileTask>::new(parse_file_receiver, result_sender, ManagerConfig::default());

        Self {
            request_tasks,
            load_file_tasks,
            lex_file_tasks,
            parse_file_tasks,
        }
    }

    pub async fn launch(self) {
        let Self {
            mut request_tasks,
            mut load_file_tasks,
            mut lex_file_tasks,
            mut parse_file_tasks,
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
    }
}

/// LaunchTask represents the task of responding to a set of command line arguments, a request to
/// a compiler daemon (or possibly a response to a file watcher notifying of a change).
///
/// There's normally only one of these, but it seems elegant to have these fit into the `Task` model.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LaunchTask {
    pub files: Vec<String>,
}

#[async_trait]
impl Task for LaunchTask {
    type Output = LoadFileTask;
    const TASK_KIND: TaskKind = TaskKind::Launch;

    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>) {
        for path in &self.files {
            result_sender
                .send((
                    self.clone(),
                    Update::NextResult(LoadFileTask { path: path.clone() }),
                ))
                .expect("Should be able to send task result to manager");
        }
        result_sender
            .send((self, Update::Complete))
            .expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LoadFileTask {
    path: String,
}

#[async_trait]
impl Task for LoadFileTask {
    type Output = LexFileTask;
    const TASK_KIND: TaskKind = TaskKind::LoadFile;

    fn has_file_path(&self) -> Option<&str> {
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
    path: String,
    contents: String,
}

#[async_trait]
impl Task for LexFileTask {
    type Output = ParseFileTask;
    const TASK_KIND: TaskKind = TaskKind::LexFile;

    fn has_file_path(&self) -> Option<&str> {
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
    path: String,
    contents: String,
    tokens: Vec<Token>,
}

#[async_trait]
impl Task for ParseFileTask {
    type Output = Ast; // For now, we'll just store the AST itself.
    const TASK_KIND: TaskKind = TaskKind::ParseFile;

    fn has_file_path(&self) -> Option<&str> {
        Some(&self.path)
    }
    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>) {
        let ast =
            crate::parser::parse(&self.path, &self.tokens).map_err(|err| self.decorate_error(err));
        result_sender
            .send((
                self,
                match ast {
                    Ok(result) => Update::FinalResult(result),
                    Err(err) => Update::Failed(err),
                },
            ))
            .expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TaskKind {
    Launch,
    LoadFile,
    LexFile,
    ParseFile,
}
