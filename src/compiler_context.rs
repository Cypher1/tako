use crate::compiler_tasks::JobTypes;
use crate::concepts::*;
use crate::free_standing::jobs::JobStore;

#[cfg_attr(test, derive(Debug))]
pub struct CompilerContext {
    files: FileVec,
    modules: ModuleVec,
    errors: ErrorVec,
    jobs: JobStore<JobTypes>,
}
