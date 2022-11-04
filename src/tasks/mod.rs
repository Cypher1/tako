use crate::string_interner::{get_new_interner, StrInterner};
use async_trait::async_trait;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
// use std::hash::Hash;
use crate::ast::Ast;
use crate::cli_options::Options;
use crate::error::{Error, TError};
use crate::tokens::Token;
use tokio::sync::mpsc;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TaskState<T, E: std::error::Error> {
    /// Place holder to record that a job is already running and shouldn't need to be run again
    /// unless invalidated.
    Running,
    Success(T), // Include a handle to the result?
    Failure(E),
    // TODO: Invalidated, // Has previous run correctly, but the previous result is (somehow) 'known' to be stale.
    // TODO: Cancelled,  // Include why it was cancelled?
}

// TODO: Add timing information, etc.
// TODO: Support re-running multiple times for stability testing.
// TODO: Store the Tasks and their statuses in a contiguous vec.
// TODO: Still use hashing to look up tasks and their IDs.
pub type TaskResults<T> = HashMap<T, TaskState<<T as Task>::Output, Error>>;

#[derive(Debug)]
pub struct TaskManager<T: Task> {
    tasks: TaskResults<T>,
    task_receiver: mpsc::UnboundedReceiver<T>,
    result_sender: mpsc::UnboundedSender<T::Output>,
}

impl<T: Task> TaskManager<T> {
    pub fn new(task_receiver: mpsc::Receiver<T>, result_sender: mpsc::Sender<T::Output>) -> Self {
        Self {
            tasks: TaskResults::new(),
            task_receiver,
            result_sender,
        }
    }

    pub async fn run_loop(&mut self) {
        todo!();
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
    pub fn new(launch_receiver: Receiver<LaunchTask>, result_sender: mpsc::UnboundedSender<Result<Ast, Error>>) -> Self {
        let (load_file_sender, load_file_receiver) = mpsc::unbounded_channel();
        let request_tasks = TaskManager::<LaunchTask>::new(launch_receiver, load_file_sender);

        let (lex_file_sender, lex_file_receiver) = mpsc::unbounded_channel();
        let load_file_tasks = TaskManager::<LoadFileTask>::new(load_file_receiver, lex_file_sender);

        let (parse_file_sender, parse_file_receiver) = mpsc::unbounded_channel();
        let lex_file_tasks = TaskManager::<LexFileTask>::new(lex_file_receiver, parse_file_sender);
        let parse_file_tasks = TaskManager::<ParseFileTask>::new(parse_file_receiver, result_sender);

        Self {
            request_tasks,
            load_file_tasks,
            lex_file_tasks,
            parse_file_tasks,
        }
    }

    pub async fn launch(self) {
        let Self {
            request_tasks,
            load_file_tasks,
            lex_file_tasks,
            parse_file_tasks,
        } = self;
        // Launch all of the task managers!
        tokio::spawn(async move {
            self.request_tasks.run_loop();
        });
        tokio::spawn(async move {
            self.load_file_tasks.run_loop();
        });
        tokio::spawn(async move {
            self.lex_file_tasks.run_loop();
        });
        tokio::spawn(async move {
            self.parse_file_tasks.run_loop();
        });
    }
}

pub type OptionsRef = Arc<Mutex<Options>>;
pub type Receiver<T> = mpsc::UnboundedReceiver<T>;
pub type Sender<T> = mpsc::UnboundedSender<Result<<T as Task>::Output, Error>>;

#[async_trait]
pub trait Task: Sized + Send /* + Hash */ {
    // TODO: Separate the code that performs the task
    // from the part that generates new tasks.
    // TODO: Store only the options 'relevant' to the task,
    // TODO: Implement a hash table from tasks to results.
    // TODO: Only perform 'new' tasks.

    type Output;
    const TASK_KIND: TaskKind;

    fn options(&self) -> &OptionsRef;

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

    async fn perform(&self, result_sender: Sender<Self>);

    fn decorate_error(&self, error: TError) -> Error {
        Error::new(error, self.has_file_path(), None, None)
    }
}

/// LaunchTask represents the task of responding to a set of command line arguments, a request to
/// a compiler daemon (or possibly a response to a file watcher notifying of a change).
///
/// There's normally only one of these, but it seems elegant to have these fit into the `Task` model.
#[derive(Debug, Clone)]
pub struct LaunchTask {
    options: OptionsRef,
}

#[async_trait]
impl Task for LaunchTask {
    type Output = LoadFileTask;
    const TASK_KIND: TaskKind = TaskKind::Launch;

    fn options(&self) -> &OptionsRef {
        &self.options
    }
    async fn perform(&self, result_sender: Sender<Self>) {
        let options = self.options.lock().expect("Should be able to access options");
        for file in options {
            result_sender.send(
                Ok(LoadFileTask {
                    options: self.options.clone(),
                    path: self.path.clone(),
                })
            ).await;
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoadFileTask {
    options: OptionsRef,
    path: String,
}

#[async_trait]
impl Task for LoadFileTask {
    type Output = LexFileTask;
    const TASK_KIND: TaskKind = TaskKind::LoadFile;

    fn has_file_path(&self) -> Option<&str> {
        Some(&self.path)
    }
    fn options(&self) -> &OptionsRef {
        &self.options
    }
    async fn perform(&self, result_sender: Sender<Self>) {
        // TODO: Use tokio's async read_to_string.
        let contents = std::fs::read_to_string(&self.path)?;

        Ok(LexFileTask {
            options: self.options.clone(),
            path: self.path.clone(),
            contents,
        })
    }
}

#[derive(Debug, Clone)]
pub struct LexFileTask {
    options: OptionsRef,
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
    fn options(&self) -> &OptionsRef {
        &self.options
    }
    async fn perform(&self, result_sender: Sender<Self>) {
        let mut string_interner = get_new_interner();
        let tokens = crate::tokens::lex(&self.contents, &mut string_interner)?;
        Ok(ParseFileTask {
            options: self.options.clone(),
            path: self.path.clone(),
            string_interner,
            tokens,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ParseFileTask {
    options: OptionsRef,
    path: String,
    string_interner: StrInterner,
    tokens: Vec<Token>,
}

#[async_trait]
impl Task for ParseFileTask {
    type Output = Ast; // For now, we'll just store the AST itself.
    const TASK_KIND: TaskKind = TaskKind::ParseFile;

    fn has_file_path(&self) -> Option<&str> {
        Some(&self.path)
    }
    fn options(&self) -> &OptionsRef {
        &self.options
    }
    async fn perform(&self, result_sender: Sender<Self>) {
        let ast = crate::parser::parse(&self.path, &self.tokens)?;
        Ok(ast)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TaskKind {
    Launch,
    LoadFile,
    LexFile,
    ParseFile,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TaskRef {
    kind: TaskKind, // Where to look for the task
    task_hash: u64, // The hash of the task???
}
