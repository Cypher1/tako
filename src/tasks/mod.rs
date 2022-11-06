use crate::ast::Ast;
use crate::cli_options::Options;
use crate::error::{Error, TError};
use crate::tokens::Token;
use async_trait::async_trait;
use std::collections::HashMap;
use tokio::sync::mpsc;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TaskState<T, E: std::error::Error> {
    /// Place holder to record that a job is already running and shouldn't need to be run again
    /// unless invalidated.
    New,
    Running(Vec<T>), // Include a handle to the result?
    Success(Vec<T>), // Include a handle to the result?
    // Uncachable result, e.g. side effecting like saving a file
    SuccessUncachable,
    Failure(E),
    // TODO: Invalidated, // Has previous run correctly, but the previous result is (somehow) 'known' to be stale.
    // TODO: Cancelled,  // Include why it was cancelled?
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Update<O: Send, E: std::error::Error + Send> {
    NextResult(O),
    FinalResult(O),
    Finished,
    Failed(E),
}


// TODO: Add timing information, etc.
// TODO: Support re-running multiple times for stability testing.
// TODO: Store the Tasks and their statuses in a contiguous vec.
// TODO: Still use hashing to look up tasks and their IDs.
pub type TaskResults<T> = HashMap<TaskId<T>, TaskState<<T as Task>::Output, Error>>;

type TaskId<Task> = Task; // This should be the pre-computed hash, to avoid sending and cloning tasks.

/* TODO: Avoid repeatedly hashing tasks.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TaskId {
    kind: TaskKind, // Where to look for the task
    task_hash: u64, // The hash of the task???
}
*/

#[derive(Debug)]
pub struct TaskManager<T: Task> {
    // TODO: Each task should get its own channel!!!
    // Use https://docs.rs/tokio-stream/latest/tokio_stream/struct.StreamMap.html
    result_store: Arc<Mutex<TaskResults<T>>>,
    task_receiver: ReceiverFor<T>,
    result_sender: SenderFor<T>,
    // status_sender: mpsc::Sender<ManagerStats>,
}

impl<T: Task + 'static> TaskManager<T> {
    pub fn new(task_receiver: ReceiverFor<T>, result_sender: SenderFor<T>) -> Self {
        Self {
            result_store: Arc::new(Mutex::new(TaskResults::new())),
            task_receiver,
            result_sender,
        }
    }

    pub async fn run_loop(&mut self) {
        let (result_or_error_sender, mut result_or_error_receiver) =
                mpsc::unbounded_channel::<(T, Update<T::Output, Error>)>();
        let result_store = self.result_store.clone();
        tokio::spawn(async move {
            while let Some((task, result)) = result_or_error_receiver.recv().await {
                let mut result_store = result_store.lock().expect("Should be able to get result store");
                let mut current_results = &mut result_store.entry(task).or_insert(TaskState::New);
                *current_results = match result {
                    // TODO: Accumulate results
                    /*
                    result => {
                        let res = if T::RESULT_IS_CACHABLE {
                            let results_so_far = match **current_results {
                                TaskState::SuccessUncachable => Vec::new(), // How did this happen?
                                TaskState::New | TaskState::Success(_) | TaskState::Failure(_) => Vec::new(), // A new task or a redo...
                                TaskState::Running(partials) => partials, // Continuing...
                            };
                            TaskState::Running(result.clone())
                        } else {
                            // Uncachable result, e.g. side effecting like saving a file
                            TaskState::SuccessUncachable
                        };
                        self.result_sender.send(result);
                        res
                    }
                    Err(error) => todo!(),
                    */
                };
            }
        });

        while let Some(task) = self.task_receiver.recv().await { // Get a new job from 'upstream'.
            let status = {
                let result_store = self.result_store.lock().expect("Should be able to get result store");
                result_store.get(&task).cloned()
            };
            let status = status.unwrap_or(TaskState::New);
            match status {
                TaskState::Success(results) => {
                    for result in results {
                        self.result_sender.send(result);
                    }
                    continue; // i.e. go look for another task.
                }
                TaskState::Running(_partial_results) => {
                    // TODO: Consider...
                    // We 'know' these are good, but there's more work to do
                    // for result in partial_results {
                        // self.result_sender.send(result);
                    // }
                    // Continue on and re-launch the job, duplicated work should not propagate if
                    // completed.
                }
                _ => {}
            }
            // Launch the job!!!
            let result_or_error_sender = result_or_error_sender.clone();
            tokio::spawn(async move {
                // Tasks will report that they are running. Do not report them here.
                task.perform(result_or_error_sender).await;
            });
        }
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
            })));
        }
        result_sender.send((self.clone(), Update::Finished));
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
        ));
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
        ));
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
        ));
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TaskKind {
    Launch,
    LoadFile,
    LexFile,
    ParseFile,
}
