use crate::cli_options::Options;
use crate::compiler_tasks::JobTypes;
use crate::concepts::*;
use crate::free_standing::jobs::{JobStore, FinishType};
use log::info;

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

    fn plan_build_jobs(&mut self) {
        todo!();
    }

    fn plan_pre_interpret_jobs(&mut self) -> JobId {
        todo!();
    }

    fn plan_interpret_jobs(&mut self) {
        let init_job_id = self.plan_pre_interpret_jobs();

        todo!();
    }
    fn plan_repl_jobs(&mut self) {
        let init_job_id = self.plan_pre_interpret_jobs();

                for file in self.options.files {
                    self.jobs.add_job();
                }
        todo!();
    }
    pub fn plan_jobs(&mut self) {
        use crate::cli_options::Command;
        match self.options.cmd {
            Build => self.plan_build_jobs(),
            Interpret => self.plan_interpret_jobs(),
            Repl => self.plan_repl_jobs(),
        }
    }

    pub fn run_job_loop(&mut self) {
        while let Some((job_id, job)) = self.jobs.get_job() {
            dbg!(&job_id, &job);

            let result = FinishType::Success;
            // Do some stuff
            self.jobs.finish_job(job_id, result);
        }
        info!("All tasks completed.");
    }
}
