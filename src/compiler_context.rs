use crate::cli_options::Options;
use crate::compiler_tasks::JobTypes;
use crate::concepts::*;
use crate::free_standing::jobs::JobStore;

#[derive(Default, Debug)]
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

    pub fn from_options(options: Options) -> Self {
        Self {
            options,
            ..Self::default()
        }
    }
}
