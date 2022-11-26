pub mod manager;
pub mod status;
pub mod task_trait;
use crate::ast::Ast;
use crate::ast::NodeId;
use crate::error::Error;
use crate::tokens::Token;
use crate::utils::meta::Meta;
use async_trait::async_trait;
use enum_kinds::EnumKind;
use log::trace;
pub use manager::{StatusReport, TaskStats};
use notify::{RecursiveMode, Watcher};
pub use status::*;
use tokio::sync::mpsc;
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
    WatchFile(WatchFileTask),
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct WatchFileTask {
    pub path: PathBuf,
}

#[async_trait]
impl Task for WatchFileTask {
    type Output = LoadFileTask;
    const TASK_KIND: TaskKind = TaskKind::WatchFile;

    fn has_file_path(&self) -> Option<&PathBuf> {
        Some(&self.path)
    }
    async fn perform(self, result_sender: UpdateSenderFor<Self>) {
        let (tx, mut rx) = mpsc::unbounded_channel();
        let mut watcher = {
            notify::recommended_watcher(
                move |res: Result<notify::Event, notify::Error>| match res {
                    Ok(event) => {
                        trace!("event: {:?}", event);
                        for path in event.paths {
                            tx.send(path).expect("TODO");
                        }
                    }
                    Err(e) => {
                        trace!("watch error: {:?}", e);
                    }
                },
            )
            .expect("Watcher failed to register")
        };
        trace!("Waiting on file changes...");
        watcher
            .watch(&self.path, RecursiveMode::Recursive)
            .expect("Should be able to watch files");

        while let Some(path) = rx.recv().await { // This avoids dropping the watcher.
            result_sender
                .send((self.clone(), Update::NextResult(LoadFileTask {
                    path,
                    invalidate: Meta(true),
                })))
                .expect("Load file task could not be sent");
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LoadFileTask {
    pub path: PathBuf,
    pub invalidate: Meta<bool>,
}

#[async_trait]
impl Task for LoadFileTask {
    type Output = LexFileTask;
    const TASK_KIND: TaskKind = TaskKind::LoadFile;
    fn invalidate(&self) -> bool {
        *self.invalidate
    }
    fn has_file_path(&self) -> Option<&PathBuf> {
        Some(&self.path)
    }
    async fn perform(self, result_sender: UpdateSenderFor<Self>) {
        // TODO: Use tokio's async read_to_string.
        let contents = std::fs::read_to_string(&self.path);
        let contents = contents.map_err(|err| self.decorate_error(err));
        result_sender
            .send((self.clone(), match contents {
                Ok(result) => Update::FinalResult(LexFileTask {
                    path: self.path,
                    contents: result,
                }),
                Err(err) => Update::Failed(err),
            }))
            .expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LexFileTask {
    pub path: PathBuf,
    pub contents: String,
}

#[async_trait]
impl Task for LexFileTask {
    type Output = ParseFileTask;
    const TASK_KIND: TaskKind = TaskKind::LexFile;

    fn has_file_path(&self) -> Option<&PathBuf> {
        Some(&self.path)
    }
    async fn perform(self, result_sender: UpdateSenderFor<Self>) {
        let tokens = crate::tokens::lex(&self.contents);
        let tokens = tokens
            .map(|tokens| ParseFileTask {
                path: self.path.clone(),
                contents: self.contents.clone(),
                tokens,
            })
            .map_err(|err| self.decorate_error(err));
        result_sender
            .send((self.clone(), match tokens {
                Ok(result) => Update::FinalResult(result),
                Err(err) => Update::Failed(err),
            }))
            .expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ParseFileTask {
    pub path: PathBuf,
    pub contents: String,
    pub tokens: Vec<Token>,
}

#[async_trait]
impl Task for ParseFileTask {
    type Output = EvalFileTask; // For now, we'll just store the AST itself.
    const TASK_KIND: TaskKind = TaskKind::ParseFile;

    fn has_file_path(&self) -> Option<&PathBuf> {
        Some(&self.path)
    }
    async fn perform(self, result_sender: UpdateSenderFor<Self>) {
        tokio::task::spawn_blocking(move || {
            let ast = crate::parser::parse(&self.path, &self.contents, &self.tokens)
                .map_err(|err| self.decorate_error(err));
            result_sender
                .send((self.clone(), match ast {
                    Ok(result) => Update::FinalResult(EvalFileTask {
                        path: self.path.to_path_buf(),
                        ast: result,
                        root: None, // Dont assume which root to run (yet?)
                    }),
                    Err(err) => Update::Failed(err),
                }))
                .expect("Should be able to send task result to manager");
        });
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct EvalFileTask {
    pub path: PathBuf,
    pub ast: Ast,
    pub root: Option<NodeId>,
}

#[async_trait]
impl Task for EvalFileTask {
    type Output = crate::primitives::Prim; // For now, we'll just store an updated AST itself.
    const TASK_KIND: TaskKind = TaskKind::EvalFile;
    const RESULT_IS_CACHABLE: bool = false;

    fn has_file_path(&self) -> Option<&PathBuf> {
        Some(&self.path)
    }
    async fn perform(self, result_sender: UpdateSenderFor<Self>) {
        tokio::task::spawn_blocking(move || {
            let result = crate::interpreter::run(&self.path, &self.ast, self.root)
                .map_err(|err| self.decorate_error(err));
            result_sender
                .send((self.clone(), match result {
                    Ok(result) => Update::FinalResult(result),
                    Err(err) => Update::Failed(err),
                }))
                .expect("Should be able to send task result to manager");
        });
    }
}
