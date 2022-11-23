pub mod manager;
pub mod status;
pub mod task_trait;
use crate::ast::Ast;
use crate::ast::NodeId;
use crate::error::Error;
use crate::tokens::Token;
use async_trait::async_trait;
use enum_kinds::EnumKind;

pub use manager::{StatusReport, TaskStats};

pub use status::*;
use std::collections::HashMap;
use std::fmt::Debug;
use std::path::PathBuf;
pub use task_trait::TaskId;
use task_trait::*;

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
    pub path: PathBuf,
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
            let ast = crate::parser::parse(&self.path, &self.contents, &self.tokens)
                .map_err(|err| self.decorate_error(err));
            result_sender
                .send((
                    self.clone(),
                    match ast {
                        Ok(result) => Update::FinalResult(EvalFileTask {
                            path: self.path.to_path_buf(),
                            ast: result,
                            root: None, // Dont assume which root to run (yet?)
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
    root: Option<NodeId>,
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
            let result = crate::interpreter::run(&self.path, &self.ast, self.root)
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
