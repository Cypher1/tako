use crate::compiler_tasks::JobTypes;
use crate::jobs::JobStore;
use crate::concepts::*;

#[cfg_attr(test, derive(Debug))]
pub struct CompilerContext {
    files: FileVec,
    modules: ModuleVec,
    errors: ErrorVec,
    jobs: JobStore<JobTypes>,
}
