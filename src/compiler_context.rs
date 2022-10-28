use crate::compiler_tasks::JobTypes;
use crate::jobs::JobStore;
use crate::concepts::*;

pub struct CompilerContext {
    files: FileVec,
    modules: ModuleVec,
    errors: ErrorVec,
    jobs: JobStore<JobTypes>,
}
