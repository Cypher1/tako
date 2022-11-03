use crate::concepts::*;
use crate::free_standing::jobs::JobId;

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
