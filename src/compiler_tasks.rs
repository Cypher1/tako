use crate::concepts::*;
use crate::free_standing::jobs::JobId;
use std::sync::{Arc, Mutex};
use std::collections::VecDeque;
use log::{info, trace};
use tokio::sync::mpsc;
use crate::cli_options::Options;
use crate::compiler_tasks::{
    JobType::{self, *},
    Progress::{self, *},
};
use crate::concepts::*;
use crate::error::{Error, ErrorId, TError};
use crate::free_standing::jobs::{GetJob, FinishType, JobId as BaseJobId, JobStore};
use crate::ui::UserInterface;

type JobId = BaseJobId<JobType>;

#[derive(Default, Debug, Clone)]
pub struct CompilerStorage {
    files: Arc<Mutex<Vec<File>>>,
    errors: Arc<Mutex<Vec<Error>>>,
    jobs: Arc<Mutex<JobStore<JobType>>>,
}

type OptionsRef = Arc<Mutex<Options>>;

trait Context: Send {
    type Output=();
    fn decorate_error(&self, error: TError) -> Error;
    fn options(&self) -> &OptionsRef;
    async fn do_impl(&self) -> Result<Self::Output, TError>;
    async fn do(&self) -> Result<Self::Output, TError> {
        self.run(|self| self.do_impl()).await
    }
    fn run<T, F: Fn(&Self) -> Result<T, TError>>(&self, closure: &F) -> Result<T, Error> {
        closure(self)
            .map_err(|err|self.decorate_error(err))
    }
}

#[derive(Debug, Clone)]
struct LoadFileContext {
    options: OptionsRef,
    path: String,
}

impl Context for LoadFileContext {
    fn decorate_error(&self, error: TError) -> Error {
        Error::new(error, Some(file), None)
    }
    fn options(&self) -> &OptionsRef {
        &self.options
    }
    async fn do_impl(&self) -> Result<(), TError> {
        let contents = std::fs::read_to_string(&self.path)?;
    }
}

#[derive(Debug, Clone)]
struct LexFileContext {
    options: OptionsRef,
    path: String,
    contents: String,
}

impl Context for LexFileContext {
    fn decorate_error(&self, error: TError) -> Error {
        Error::new(error, Some(file), None)
    }
    fn options(&self) -> &OptionsRef {
        &self.options
    }
    async fn do_impl(&self) -> Result<(), TError> {
        let contents = std::fs::read_to_string(&self.path)?;
    }
}




type JobReference = Option<JobId<JobType>>;

#[derive(Debug, Copy, Clone)]
pub enum Progress {
    AllFilesParsed,
    AllFilesRun,
    ReplReady,
    GlobalTypeCheckDone,
    GlobalOptimizeDone,
    GlobalCodeGenDone,
}

impl std::fmt::Display for Progress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Progress::AllFilesParsed => "All files parsed!",
                Progress::AllFilesRun => "All files run!",
                Progress::ReplReady => "REPL ready!",
                Progress::GlobalTypeCheckDone => "Type checking done!",
                Progress::GlobalOptimizeDone => "Optimization done!",
                Progress::GlobalCodeGenDone => "Code generation done!",
            }
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub enum JobType {
    /// General purpose.
    Load(FileId),
    /// General purpose.
    Lex(FileId),
    /// General purpose.
    Parse(FileId),
    /// General purpose.
    ReportProgress(Progress),

    /// Interpreter only.
    LoadIntoInterpreter(FileId),
    /// Interpreter only.
    RunInInterpreter(FileId),

    /// Static + Safe interpreter.
    TypeCheckAllModulesStart {
        file_id: FileId,
        down_stream: JobReference,
    },
    /// Static + Safe interpreter.
    TypeCheck(ModuleId),
    /// Static + Safe interpreter.
    TypeCheckAllModulesDone(FileId),

    /// Static only.
    OptimizeAllModulesStart {
        file_id: FileId,
        optimization_level: u32,
        down_stream: JobReference,
    },
    /// Static only.
    Optimize(ModuleId, u32),
    /// Static only.
    OptimizeAllModulesDone(FileId),
    /// Static only.
    GenerateBinary, // TODO: ???
}
