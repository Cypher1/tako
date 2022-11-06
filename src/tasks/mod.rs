use log::trace;
use crate::ast::Ast;
use crate::cli_options::Options;
use crate::error::{Error, TError};
use crate::tokens::Token;
use async_trait::async_trait;
use std::collections::HashMap;
use tokio::sync::mpsc;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TaskState<E: std::error::Error> {
    /// Place holder to record that a job is already running and shouldn't need to be run again
    /// unless invalidated.
    New,
    Partial, // Include a handle to the result?
    Complete, // Include a handle to the result?
    Failure(E),
    // TODO: Invalidated, // Has previous run correctly, but the previous result is (somehow) 'known' to be stale.
    // TODO: Cancelled,  // Include why it was cancelled?
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TaskStatus<T, E: std::error::Error> {
    state: TaskState<E>,
    results: Vec<T>, // TODO: Avoid wasting this if the task is uncachable?
}

impl<T, E: std::error::Error> TaskStatus<T, E> {
    pub fn new() -> Self {
        Self {
            state: TaskState::New,
            results: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Update<O: Send, E: std::error::Error + Send> {
    NextResult(O),
    FinalResult(O),
    Complete,
    Failed(E),
}


// TODO: Add timing information, etc.
// TODO: Support re-running multiple times for stability testing.
// TODO: Store the Tasks and their statuses in a contiguous vec.
// TODO: Still use hashing to look up tasks and their IDs.
pub type TaskResults<T> = HashMap<TaskId<T>, TaskStatus<<T as Task>::Output, Error>>;

type TaskId<Task> = Task; // This should be the pre-computed hash, to avoid sending and cloning tasks.

/* TODO: Avoid repeatedly hashing tasks.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TaskId {
    kind: TaskKind, // Where to look for the task
    task_hash: u64, // The hash of the task???
}
*/

#[derive(Debug)]
pub struct TaskManager<T: std::fmt::Debug + Task> {
    // TODO: Each task should get its own channel!!!
    // Use https://docs.rs/tokio-stream/latest/tokio_stream/struct.StreamMap.html
    result_store: Arc<Mutex<TaskResults<T>>>,
    task_receiver: ReceiverFor<T>,
    result_sender: SenderFor<T>,
    // status_sender: mpsc::Sender<ManagerStats>,
}

impl<T: std::fmt::Debug + Task + 'static> TaskManager<T> {
    const TYPE_NAME: &str = std::any::type_name::<T>();

    pub fn new(task_receiver: ReceiverFor<T>, result_sender: SenderFor<T>) -> Self {
        Self {
            result_store: Arc::new(Mutex::new(TaskResults::new())),
            task_receiver,
            result_sender,
        }
    }

    pub async fn run_loop(&mut self) {
        trace!("{} starting run_loop", Self::TYPE_NAME);
        let (result_or_error_sender, mut result_or_error_receiver) =
                mpsc::unbounded_channel::<(T, Update<T::Output, Error>)>();
        let result_store = self.result_store.clone();
        let result_sender = self.result_sender.clone();
        tokio::spawn(async move {
            trace!("{}: Waiting for results...", Self::TYPE_NAME);
            while let Some((task, update)) = result_or_error_receiver.recv().await {
                trace!("{} received update from task: {task:?} {update:?}", Self::TYPE_NAME);
                let mut result_store = result_store.lock().expect("Should be able to get result store");
                let mut current_results = result_store.entry(task).or_insert(TaskStatus::new());
                let mut is_complete = false;
                let mut error = None;
                let results_so_far = &mut current_results.results;
                match update {
                    Update::NextResult(res) => {
                        result_sender.send(res.clone()).expect("Should be able to send results");
                        results_so_far.push(res);
                    }
                    Update::FinalResult(res) => {
                        is_complete = true;
                        result_sender.send(res.clone()).expect("Should be able to send results");
                        results_so_far.push(res);
                    }
                    Update::Complete => {
                        is_complete = true;
                    }
                    Update::Failed(err) => {
                        is_complete = true; // For completeness...?
                        error = Some(err);
                    }
                };
                current_results.state = match (error, is_complete) {
                    (Some(err), _is_complete) => TaskState::Failure(err),
                    (None, /*is_complete*/ true) => TaskState::Complete,
                    (None, /*is_complete*/ false) => TaskState::Partial,
                };
            }
            trace!("{} no more results... Finishing listening loop.", Self::TYPE_NAME);
        });

        trace!("{}: Waiting for tasks...", Self::TYPE_NAME);
        while let Some(task) = self.task_receiver.recv().await { // Get a new job from 'upstream'.
            trace!("{} received task: {task:?}", Self::TYPE_NAME);
            let status = {
                let result_store = self.result_store.lock().expect("Should be able to get result store");
                // We'll need to forward these on, so we can clone now and drop the result_store lock earlier!
                result_store.get(&task).cloned()
            };
            let status = status.unwrap_or_else(||TaskStatus::new());
            match (&status.state, T::RESULT_IS_CACHABLE) {
                // TODO: Consider that partial results 'should' still be safe to re-use and could pre-start later work.
                /* TaskState::Partial | */
                (TaskState::Complete, true) => {
                    trace!("{} cached task: {task:?}", Self::TYPE_NAME);
                    for result in status.results {
                        self.result_sender.send(result).expect("Should be able to send results");
                    }
                    continue; // i.e. go look for another task.
                }
                (TaskState::Complete, false) => {
                    trace!("{} un-cacheable: {task:?} (will re-run)", Self::TYPE_NAME);
                }
                // Continue on and re-launch the job, duplicated work should not propagate if completed.
                _ => {
                    trace!("{} task: {task:?} status is {status:?}", Self::TYPE_NAME);
                }
            }
            // Launch the job!!!
            let result_or_error_sender = result_or_error_sender.clone();
            tokio::spawn(async move {
                // Tasks will report that they are running. Do not report them here.
                task.perform(result_or_error_sender).await;
            });
        }
        trace!("{} no more tasks... Finishing run_loop", Self::TYPE_NAME);
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
    // TODO: type_check_inside_module: TaskResults<>,
    // Produces type checked (and optimizable) modules **AND**
    // partially type checked (but) mergable-modules.
    // Pair-wise merging of type checking information???
    // TODO: type_check_merge_module_sets: TaskResults<>,
    // Produces type checked (and optimizable) modules **AND**
    // Partially type checked (but) mergable-modules
    // TODO: lowering: TaskResults<>,
    // TODO: optimization: TaskResults<>,
    // TODO: code_generation: TaskResults<>,
    // TODO: binary_generation: TaskResults<>,
    // TODO: load_into_interpreter: TaskResults<>,
    // TODO: run_in_interpreter: TaskResults<>,
}

impl TaskSet {
    pub fn new(
        launch_receiver: ReceiverFor<LaunchTask>,
        result_sender: SenderFor<ParseFileTask>,
    ) -> Self {
        let (load_file_sender, load_file_receiver) = mpsc::unbounded_channel();
        let request_tasks = TaskManager::<LaunchTask>::new(launch_receiver, load_file_sender);

        let (lex_file_sender, lex_file_receiver) = mpsc::unbounded_channel();
        let load_file_tasks = TaskManager::<LoadFileTask>::new(load_file_receiver, lex_file_sender);

        let (parse_file_sender, parse_file_receiver) = mpsc::unbounded_channel();
        let lex_file_tasks = TaskManager::<LexFileTask>::new(lex_file_receiver, parse_file_sender);
        let parse_file_tasks =
            TaskManager::<ParseFileTask>::new(parse_file_receiver, result_sender);

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

pub type ReceiverFor<T> = mpsc::UnboundedReceiver<T>;
pub type SenderFor<T> = mpsc::UnboundedSender<<T as Task>::Output>;
pub type UpdateSender<T, O> = mpsc::UnboundedSender<(T, Update<O, Error>)>;

#[async_trait]
pub trait Task: Clone + std::hash::Hash + Eq + Sized + Send {
    // TODO: Separate the code that performs the task
    // from the part that generates new tasks.
    // TODO: Store only the options 'relevant' to the task,
    // TODO: Implement a hash table from tasks to results.
    // TODO: Only perform 'new' tasks.

    type Output: std::fmt::Debug + Clone + Send;
    const TASK_KIND: TaskKind;
    const RESULT_IS_CACHABLE: bool = true;

    fn options(&self) -> &Options;

    fn has_file_path(&self) -> Option<&str> {
        None
    }
    // fn has_module(&self) -> Option<&Module> {
    //  None
    //}
    // fn has_tokens(&self) -> Option<&Vec<Token>> {
    //  None
    //}
    // fn has_ast(&self) -> Option<&Ast> {
    //  None
    //}
    // TODO: More...

    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>);

    fn decorate_error<E: Into<TError>>(&self, error: E) -> Error {
        Error::new(error.into(), self.has_file_path(), None, None)
    }
}

/// LaunchTask represents the task of responding to a set of command line arguments, a request to
/// a compiler daemon (or possibly a response to a file watcher notifying of a change).
///
/// There's normally only one of these, but it seems elegant to have these fit into the `Task` model.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LaunchTask {
    pub options: Options,
}

#[async_trait]
impl Task for LaunchTask {
    type Output = LoadFileTask;
    const TASK_KIND: TaskKind = TaskKind::Launch;

    fn options(&self) -> &Options {
        &self.options
    }
    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>) {
        for path in &self.options.files {
            result_sender.send((self.clone(), Update::NextResult(LoadFileTask {
                options: self.options.clone(),
                path: path.clone(),
            }))).expect("Should be able to send task result to manager");
        }
        result_sender.send((self.clone(), Update::Complete)).expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LoadFileTask {
    options: Options,
    path: String,
}

#[async_trait]
impl Task for LoadFileTask {
    type Output = LexFileTask;
    const TASK_KIND: TaskKind = TaskKind::LoadFile;

    fn has_file_path(&self) -> Option<&str> {
        Some(&self.path)
    }
    fn options(&self) -> &Options {
        &self.options
    }
    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>) {
        // TODO: Use tokio's async read_to_string.
        let contents = std::fs::read_to_string(&self.path);
        let contents = contents.map(|contents| LexFileTask {
                options: self.options.clone(),
                path: self.path.clone(),
                contents,
            })
            .map_err(|err| self.decorate_error(err));
        result_sender.send((
                self,
                match contents {
                    Ok(result) => Update::FinalResult(result),
                    Err(err) => Update::Failed(err),
                }
        )).expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LexFileTask {
    options: Options,
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
    fn options(&self) -> &Options {
        &self.options
    }
    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>) {
        let tokens = crate::tokens::lex(&self.contents);
        let tokens = tokens
            .map(|tokens| ParseFileTask {
                options: self.options.clone(),
                path: self.path.clone(),
                contents: self.contents.clone(),
                tokens,
            })
            .map_err(|err| self.decorate_error(err));
        result_sender.send((
                self,
                match tokens {
                    Ok(result) => Update::FinalResult(result),
                    Err(err) => Update::Failed(err),
                }
        )).expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ParseFileTask {
    options: Options,
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
    fn options(&self) -> &Options {
        &self.options
    }
    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>) {
        let ast = crate::parser::parse(&self.path, &self.tokens)
            .map_err(|err| self.decorate_error(err));
        result_sender.send((
                self,
                match ast {
                    Ok(result) => Update::FinalResult(result),
                    Err(err) => Update::Failed(err),
                }
        )).expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TaskKind {
    Launch,
    LoadFile,
    LexFile,
    ParseFile,
}
