use crate::ast::Ast;
use crate::cli_options::Options;
use crate::compiler_tasks::JobTypes::{self, *};
use crate::concepts::*;
use crate::free_standing::jobs::{FinishType, JobId as BaseJobId, JobStore};
use crate::string_interner::get_new_interner;
use log::info;

type JobId = BaseJobId<JobTypes>;

#[derive(Default, Debug)]
pub struct CompilerStorage {
    files: Vec<File>,
    modules: Vec<Module>,
    errors: Vec<Error>,
    jobs: JobStore<JobTypes>,
}

#[derive(Debug)]
pub struct CompilerContext<'opts> {
    store: CompilerStorage,
    options: &'opts Options,
}

impl<'opts> std::ops::Deref for CompilerContext<'opts> {
    type Target = CompilerStorage;
    fn deref(&self) -> &CompilerStorage {
        &self.store
    }
}

impl<'opts> std::ops::DerefMut for CompilerContext<'opts> {
    // type Target = CompilerStorage;
    fn deref_mut(&mut self) -> &mut CompilerStorage {
        &mut self.store
    }
}

impl<'opts> CompilerContext<'opts> {
    pub fn new(options: &'opts Options) -> Self {
        Self::from_options(options)
    }

    pub fn from_options(options: &'opts Options) -> Self {
        Self {
            options,
            store: CompilerStorage::default(),
        }
    }

    fn plan_build_jobs(&mut self) {
        todo!();
    }

    fn plan_parse_file(&mut self, path: &str) -> JobId {
        let fileid = FileId::new(
            &mut self.files,
            File {
                path: path.to_string(),
                string_interner: get_new_interner(),
                root: None,
                contents: None,
                lexed: None,
                ast: Ast::default(),
            },
        )
        .expect("Too many file ids");
        let load_id = self.jobs.add_job(Load(fileid), vec![]);
        let lex_id = self.jobs.add_job(Lex(fileid), vec![load_id]);
        let parse_id = self.jobs.add_job(Parse(fileid), vec![lex_id]);
        parse_id
    }

    fn plan_pre_interpret_jobs(&mut self) -> JobId {
        let mut prep_jobs = vec![];
        for path in &self.options.files {
            let parse_id = self.plan_parse_file(&path);
            prep_jobs.push(parse_id);
        }
        self.jobs.add_job(AllFilesParsed, prep_jobs)
    }

    fn plan_interpret_jobs(&mut self) {
        let init_job_id = self.plan_pre_interpret_jobs();
        todo!();
    }
    fn plan_repl_jobs(&mut self) {
        let init_job_id = self.plan_pre_interpret_jobs();
        todo!();
    }
    pub fn plan_jobs(&mut self) {
        use crate::cli_options::Command::*;
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
