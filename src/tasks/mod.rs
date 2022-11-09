mod manager;
mod status;
mod task_trait;
use crate::ast::Ast;
use crate::error::Error;
use crate::tokens::Token;
use async_trait::async_trait;
use log::debug;
use std::collections::HashMap;
use std::fmt::Debug;

use tokio::sync::{mpsc, watch};

pub use manager::TaskManagerRegistration;
use manager::{ManagerConfig, StatusReport, TaskManager};
use status::*;
use task_trait::*;

// TODO: Add timing information, etc.
// TODO: Support re-running multiple times for stability testing.
// TODO: Store the Tasks and their statuses in a contiguous vec.
// TODO: Still use hashing to look up tasks and their IDs.
// This should be the pre-computed hash, to avoid sending and cloning tasks.
pub type TaskResults<T> = HashMap<T, TaskStatus<<T as Task>::Output, Error>>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TaskKind {
    Request,
    LoadFile,
    LexFile,
    ParseFile,
}

#[derive(Debug)]
pub struct TaskSet {
    // TODO: Track the jobs that are being done...
    // Invalidate these if
    request_tasks: TaskManager<Request>,
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
    pub fn create<T: Task + 'static>(
        task_receiver: TaskReceiverFor<T>,
        result_sender: TaskSenderFor<T>,
        registration_sender: mpsc::UnboundedSender<TaskManagerRegistration>,
        config: ManagerConfig,
    ) -> TaskManager<T> {
        let (status_report_sender, status_report_receiver) =
            watch::channel(StatusReport::new(<T as Task>::TASK_KIND));
        let manager =
            TaskManager::<T>::new(task_receiver, result_sender, status_report_sender, config);
        if let Err(err) = registration_sender.send(TaskManagerRegistration {
            kind: T::TASK_KIND,
            status_report_receiver,
        }) {
            debug!("Couldn't register task manager to UI: {}", err);
        }
        manager
    }

    pub fn new(
        launch_receiver: TaskReceiverFor<Request>,
        result_sender: TaskSenderFor<ParseFileTask>,
        registration_sender: mpsc::UnboundedSender<TaskManagerRegistration>,
    ) -> Self {
        let (load_file_sender, load_file_receiver) = mpsc::unbounded_channel();
        let request_tasks = Self::create::<Request>(
            launch_receiver,
            load_file_sender,
            registration_sender.clone(),
            ManagerConfig::default(),
        );

        let (lex_file_sender, lex_file_receiver) = mpsc::unbounded_channel();
        let load_file_tasks = Self::create::<LoadFileTask>(
            load_file_receiver,
            lex_file_sender,
            registration_sender.clone(),
            ManagerConfig::default(),
        );

        let (parse_file_sender, parse_file_receiver) = mpsc::unbounded_channel();
        let lex_file_tasks = Self::create::<LexFileTask>(
            lex_file_receiver,
            parse_file_sender,
            registration_sender.clone(),
            ManagerConfig::default(),
        );
        let parse_file_tasks = Self::create::<ParseFileTask>(
            parse_file_receiver,
            result_sender,
            registration_sender,
            ManagerConfig::default(),
        );

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

/// Request represents the task of responding to a set of command line arguments, a request to
/// a compiler daemon (or possibly a response to a file watcher notifying of a change).
///
/// There's normally only one of these, but it seems elegant to have these fit into the `Task` model.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Request {
    Launch { files: Vec<String> },
}

#[async_trait]
impl Task for Request {
    type Output = LoadFileTask;
    const TASK_KIND: TaskKind = TaskKind::Request;

    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>) {
        match &self {
            Request::Launch { files } => {
                for path in files {
                    result_sender
                        .send((
                            self.clone(),
                            Update::NextResult(LoadFileTask { path: path.clone() }),
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
