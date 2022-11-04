use crate::string_interner::{get_new_interner, StrInterner};
use async_trait::async_trait;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
// use std::hash::Hash;
use crate::ast::Ast;
use crate::cli_options::Options;
use crate::error::{Error, TError};
use crate::tokens::Token;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TaskState<T, E: std::error::Error> {
    Running,
    Success(T), // Include a handle to the result?
    Cancelled,  // Include why it was cancelled?
    Failed(E),
}

// TODO: Add timing information, etc.
// TODO: Support re-running multiple times for stability testing.
// TODO: Store the Tasks and their statuses in a contiguous vec.
// TODO: Still use hashing to look up tasks and their IDs.
pub type TaskResults<T> = HashMap<T, TaskState<<T as Task>::Output, Error>>;

pub struct TaskManager<T: Task> {
    tasks: TaskResults<T>,
    task_reporter: Reciever<T>,
    result_reporter: Sender<T>,
}

#[derive(Debug)]
pub struct TaskStore {
    // TODO: Track the jobs that are being done...
    // Invalidate these if
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

pub type OptionsRef = Arc<Mutex<Options>>;

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

    async fn perform_impl(&self) -> Result<Self::Output, TError>;

    fn decorate_error(&self, error: TError) -> Error {
        Error::new(error, self.has_file_path(), None, None)
    }
    async fn perform(&self) -> Result<Self::Output, Error> {
        let res = self.perform_impl().await;
        res.map_err(|err| self.decorate_error(err))
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
    async fn perform_impl(&self) -> Result<Self::Output, TError> {
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
    async fn perform_impl(&self) -> Result<Self::Output, TError> {
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
    async fn perform_impl(&self) -> Result<Self::Output, TError> {
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
