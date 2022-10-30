use crate::compiler_tasks::JobTypes;
use crate::concepts::*;
use crate::free_standing::jobs::JobStore;
use crate::cli_options::Options;

#[derive(Default)]
#[cfg_attr(test, derive(Debug))]
pub struct CompilerContext {
    files: FileVec,
    modules: ModuleVec,
    errors: ErrorVec,
    jobs: JobStore<JobTypes>,
    options: Options,
}

impl CompilerContext {
    pub fn new() -> Self {
        Self::default()
    }
}
