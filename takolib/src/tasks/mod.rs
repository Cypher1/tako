pub mod manager;
pub mod status;
pub mod task_trait;
use crate::ast::Ast;
use crate::ast::NodeId;
use crate::error::Error;
use crate::lowerer::lower;
use crate::parser::tokens::Token;
use crate::primitives::meta::Meta;
use crate::primitives::Prim;
use crate::queries::FileRef;
use async_trait::async_trait;
use enum_kinds::EnumKind;
use llamada::Llamada;
use log::trace;
pub use manager::{StatusReport, TaskStats};
pub use status::*;
use std::collections::HashMap;
use std::fmt::Debug;
use std::path::PathBuf;
use std::sync::Arc;
pub use task_trait::TaskId;
use task_trait::{Task, UpdateSenderFor};

// TODO(debugging): Add timing information, etc.
// TODO(debugging): Support re-running multiple times for stability testing.
// TODO(perf): Store the Tasks and their statuses in a contiguous vec.
// TODO(perf): Still use hashing to look up tasks and their IDs.
// This should be the pre-computed hash, to avoid sending and cloning tasks.
pub type TaskResults<T> = HashMap<T, TaskStatus<<T as Task>::Output, Error>>;

#[derive(EnumKind)]
#[enum_kind(TaskKind, derive(Hash, Ord, PartialOrd))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnyTask {
    LoadFile(LoadFileTask),
    LexFile(LexFileTask),
    ParseFile(ParseFileTask),
    DesugarFile(DesugarFileTask),
    LowerFile(LowerFileTask),
    Codegen(CodegenTask),
    EvalFile(EvalFileTask),
}

/// `RequestTask` represents the task of responding to a set of command line arguments, a request to
/// a compiler daemon (or possibly a response to a file watcher notifying of a change).
///
/// There's normally only one of these, but it seems elegant to have these fit into the `Task` model.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RequestTask {
    Build {
        files: Vec<FileRef>,
    },
    RunInterpreter {
        files: Vec<FileRef>,
    },
    Eval {
        ast: Arc<Ast>, /* Holding all context and state */
        expr: String,  // TODO(cleanup): Remove, move all into Ast
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LoadFileTask {
    pub file: FileRef,
    pub invalidate: Meta<bool>,
}

#[async_trait]
impl Task for LoadFileTask {
    type Output = LexFileTask;
    const TASK_KIND: TaskKind = TaskKind::LoadFile;
    fn invalidate(&self) -> bool {
        *self.invalidate
    }
    fn has_file(&self) -> Option<&FileRef> {
        Some(&self.file)
    }
    async fn perform(self, result_sender: UpdateSenderFor<Self>) {
        trace!("LoadFileTask: {file}", file = self.file);
        // TODO(perf): Use tokio's async read_to_string.
        let contents = std::fs::read_to_string(&self.file.to_path_buf().1);
        let contents = contents.map_err(|err| self.decorate_error(err));
        result_sender
            .send((
                self.clone(),
                match contents {
                    Ok(result) => Update::FinalResult(LexFileTask {
                        file: self.file,
                        contents: result,
                    }),
                    Err(err) => Update::Failed(err),
                },
            ))
            .expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LexFileTask {
    pub file: FileRef,
    pub contents: String,
}

#[async_trait]
impl Task for LexFileTask {
    type Output = ParseFileTask;
    const TASK_KIND: TaskKind = TaskKind::LexFile;

    fn has_file(&self) -> Option<&FileRef> {
        Some(&self.file)
    }
    async fn perform(self, result_sender: UpdateSenderFor<Self>) {
        trace!("LexFileTask: {file}", file = self.file);
        let tokens = crate::parser::tokens::lex(&self.contents);
        let tokens = tokens
            .map(|tokens| ParseFileTask {
                ast: Arc::new(Ast::new(self.file.clone())),
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
    pub ast: Arc<Ast>,
    pub contents: String,
    pub tokens: Vec<Token>,
}

#[async_trait]
impl Task for ParseFileTask {
    type Output = DesugarFileTask; // For now, we'll just store the AST itself.
    const TASK_KIND: TaskKind = TaskKind::ParseFile;

    fn has_file(&self) -> Option<&FileRef> {
        Some(&self.ast.fileref)
    }
    async fn perform(self, result_sender: UpdateSenderFor<Self>) {
        trace!("ParseFileTask: {file}", file = self.ast.fileref);
        let ast = crate::parser::parse(&self.ast, &self.contents, &self.tokens)
            .map_err(|err| self.decorate_error(err));
        result_sender
            .send((
                self.clone(),
                match ast {
                    Ok(result) => Update::FinalResult(DesugarFileTask {
                        ast: result,
                        root: None, // Dont assume which root to run (yet?)
                    }),
                    Err(err) => Update::Failed(err),
                },
            ))
            .expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct DesugarFileTask {
    pub ast: Ast,
    pub root: Option<NodeId>,
}

#[async_trait]
impl Task for DesugarFileTask {
    // Ensure that we can parse and run pre-desugared files.
    type Output = EvalFileTask;

    const TASK_KIND: TaskKind = TaskKind::DesugarFile;

    fn has_file(&self) -> Option<&FileRef> {
        Some(&self.ast.fileref)
    }
    async fn perform(self, result_sender: UpdateSenderFor<Self>) {
        trace!("DesugarFileTask: {file}", file = self.ast.fileref);
        let ast =
            crate::desugarer::desugar(&self.ast, self.root).map_err(|err| self.decorate_error(err));
        result_sender
            .send((
                self.clone(),
                match ast {
                    Ok(result) => Update::FinalResult(EvalFileTask {
                        ast: result,
                        root: None,
                    }),
                    Err(err) => Update::Failed(err),
                },
            ))
            .expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LowerFileTask {
    pub ast: Ast,
    pub root: NodeId,
}

#[async_trait]
impl Task for LowerFileTask {
    // Ensure that we can parse and run pre-desugared files.
    type Output = CodegenTask;

    const TASK_KIND: TaskKind = TaskKind::LowerFile;

    fn has_file(&self) -> Option<&FileRef> {
        Some(&self.ast.fileref)
    }
    async fn perform(self, result_sender: UpdateSenderFor<Self>) {
        trace!("LowerFileTask: {file}", file = self.ast.fileref);
        let result = lower(&self.ast, self.root).map_err(|err| self.decorate_error(err));
        // TODO(correctness): Handle in mem files
        let (_source_zip, mut out_path) = self.ast.fileref.to_path_buf(); // TODO(correctness): Merge source zip path and out path.
        out_path.set_extension(".out");
        result_sender
            .send((
                self.clone(),
                match result {
                    Ok(result) => Update::FinalResult(CodegenTask {
                        ast: self.ast,
                        out_path,
                        lowered: result,
                        root: self.root,
                    }),
                    Err(err) => Update::Failed(err),
                },
            ))
            .expect("Should be able to send task result to manager");
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct EvalFileTask {
    pub ast: Ast,
    pub root: Option<NodeId>,
}

#[async_trait]
impl Task for EvalFileTask {
    type Output = Prim; // For now, we'll just store an updated AST itself.
    const TASK_KIND: TaskKind = TaskKind::EvalFile;
    const RESULT_IS_CACHABLE: bool = false;

    fn has_file(&self) -> Option<&FileRef> {
        Some(&self.ast.fileref)
    }
    async fn perform(self, result_sender: UpdateSenderFor<Self>) {
        trace!("EvalFileTask: {file}", file = self.ast.fileref);
        let result = crate::interpreter::run(self.ast.clone(), self.root)
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
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CodegenTask {
    pub out_path: PathBuf,
    pub ast: Ast,
    pub lowered: Llamada,
    pub root: NodeId,
}

#[async_trait]
impl Task for CodegenTask {
    type Output = Prim; // For now, we'll send back the path as a string.
    const TASK_KIND: TaskKind = TaskKind::Codegen;
    const RESULT_IS_CACHABLE: bool = false;

    fn has_file(&self) -> Option<&FileRef> {
        Some(&self.ast.fileref)
    }

    #[cfg(not(feature = "codegen"))]
    async fn perform(self, result_sender: UpdateSenderFor<Self>) {
        trace!("CodegenTask (nobackend): {file}", file = self.ast.fileref,);
        use crate::error::TError;
        let err = Update::Failed(self.decorate_error(TError::InternalError {
            location: None,
            message: "No backend".to_string(),
        }));
        result_sender
            .send((self, err))
            .expect("Should be able to send task result to manager");
    }
    #[cfg(feature = "codegen")]
    async fn perform(self, result_sender: UpdateSenderFor<Self>) {
        trace!("CodegenTask (backend): {file}", file = self.file);
        let result = crate::codegen::codegen(&self.file, &self.ast, Some(self.root))
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
    }
}
