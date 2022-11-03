use crate::cli_options::Options;
use crate::compiler_tasks::{
    JobType::{self, *},
    Progress::*,
};
use crate::concepts::*;
use crate::error::{Error, ErrorId, TError};
use crate::free_standing::jobs::{FinishType, JobId as BaseJobId, JobStore};
use crate::ui::UserInterface;
use log::{info, trace};
use std::sync::{Arc, Mutex};

type JobId = BaseJobId<JobType>;

#[derive(Default, Debug)]
pub struct CompilerStorage {
    files: Vec<File>,
    _modules: Vec<Module>,
    errors: Vec<Error>,
    jobs: JobStore<JobType>,
}

#[derive(Debug)]
pub struct CompilerContext<'opts> {
    store: CompilerStorage,
    ui: Arc<Mutex<dyn UserInterface>>,
    options: &'opts Options,
}

pub struct InContext<'a, 'opts, T>(&'a CompilerContext<'opts>, &'a T);

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

fn make_ui_arc<T: UserInterface + 'static>(value: T) -> Arc<Mutex<dyn UserInterface>> {
    Arc::new(Mutex::new(value))
}

impl<'opts> CompilerContext<'opts> {
    pub fn new(options: &'opts Options) -> Self {
        Self::from_options(options)
    }

    pub fn from_options(options: &'opts Options) -> Self {
        use crate::ui::{UiMode, CLI, TUI};
        let ui = match options.ui_mode {
            UiMode::Cli => make_ui_arc(CLI::new()),
            UiMode::Tui => make_ui_arc(TUI::new()),
            UiMode::TuiIfAvailable => {
                if false {
                    make_ui_arc(CLI::new())
                } else {
                    make_ui_arc(TUI::new())
                }
            }
        };
        Self {
            options,
            ui,
            store: CompilerStorage::default(),
        }
    }

    fn plan_parse_jobs(&mut self, path: &str) -> (FileId, JobId) {
        let fileid = FileId::new(
            &mut self.files,
            File::from_path(path),
        )
        .expect("Too many file ids");
        let load_job = self.jobs.add_job(Load(fileid), vec![]);
        let lex_job = self.jobs.add_job(Lex(fileid), vec![load_job]);
        let parse_job = self.jobs.add_job(Parse(fileid), vec![lex_job]);
        (fileid, parse_job)
    }

    fn plan_build_job(
        &mut self,
        path: &str,
    ) -> (JobId, JobId, Option<JobId>, Option<JobId>, JobId) {
        let (file_id, parse_job) = self.plan_parse_jobs(path);
        let type_check_job_discover = self.jobs.add_job(
            TypeCheckAllModulesStart {
                file_id,
                down_stream: None,
            },
            vec![parse_job],
        );
        let type_check_job = self.jobs.add_job(
            TypeCheckAllModulesDone(file_id),
            vec![type_check_job_discover],
        );
        // Give the 'starter' a way to link up dependencies
        match &mut self.jobs.get(type_check_job_discover).kind {
            TypeCheckAllModulesStart {
                file_id: _,
                ref mut down_stream,
            } => {
                *down_stream = Some(type_check_job);
            }
            _ => panic!("Job ordering changed!?"),
        }

        let optimization_level = self.options.optimization_level;
        let optimize_job = if optimization_level > 0 {
            let optimize_file_discover = self.jobs.add_job(
                OptimizeAllModulesStart {
                    file_id,
                    down_stream: None,
                    optimization_level,
                },
                vec![type_check_job],
            );
            let optimize_job = self.jobs.add_job(
                OptimizeAllModulesDone(file_id),
                vec![optimize_file_discover],
            );
            // Give the 'starter' a way to link up dependencies
            match &mut self.jobs.get(optimize_file_discover).kind {
                OptimizeAllModulesStart {
                    file_id: _,
                    ref mut down_stream,
                    optimization_level: _,
                } => {
                    *down_stream = Some(optimize_job);
                }
                _ => panic!("Job ordering changed!?"),
            }
            Some(optimize_job)
        } else {
            None
        };
        let code_gen = &self.options.code_gen;
        let code_gen_job = if code_gen.is_some() { todo!() } else { None };
        let last_job = if let Some(optimize_job) = &optimize_job {
            *optimize_job
        } else {
            type_check_job
        };
        (
            parse_job,
            type_check_job,
            optimize_job,
            code_gen_job,
            last_job,
        )
    }

    fn plan_build_jobs(&mut self) {
        let mut prep_jobs = Vec::new();
        let mut type_check_jobs = Vec::new();
        let mut optimize_jobs = Vec::new();
        let mut code_gen_jobs = Vec::new();
        for path in &self.options.files {
            let (parse_job, type_check_job, optimize_job, code_gen_job, _file_ready_job) =
                self.plan_build_job(path);
            prep_jobs.push(parse_job);
            type_check_jobs.push(type_check_job);
            if let Some(optimize_job) = optimize_job {
                optimize_jobs.push(optimize_job);
            }
            if let Some(code_gen_job) = code_gen_job {
                code_gen_jobs.push(code_gen_job);
            }
        }
        self.jobs.add_job(ReportProgress(AllFilesParsed), prep_jobs);
        self.jobs
            .add_job(ReportProgress(GlobalTypeCheckDone), type_check_jobs);
        self.jobs
            .add_job(ReportProgress(GlobalOptimizeDone), optimize_jobs);
        self.jobs
            .add_job(ReportProgress(GlobalCodeGenDone), code_gen_jobs);
    }

    fn plan_interpret_jobs(&mut self) {
        let mut runs = Vec::new();
        for path in &self.options.files {
            let (file_id, init_job_id) = self.plan_parse_jobs(path);
            let load_job = self
                .jobs
                .add_job(LoadIntoInterpreter(file_id), vec![init_job_id]);
            runs.push(self.jobs.add_job(RunInInterpreter(file_id), vec![load_job]));
        }
        self.jobs.add_job(ReportProgress(AllFilesRun), runs);
    }
    fn plan_repl_jobs(&mut self) {
        let mut loads = Vec::new();
        for path in &self.options.files {
            let (file_id, init_job_id) = self.plan_parse_jobs(path);
            let load_job = self
                .jobs
                .add_job(LoadIntoInterpreter(file_id), vec![init_job_id]);
            loads.push(load_job);
        }
        self.jobs.add_job(ReportProgress(ReplReady), loads);
    }
    pub fn plan_jobs(&mut self) {
        use crate::cli_options::Command::*;
        match self.options.cmd {
            Build => self.plan_build_jobs(),
            Interpret => self.plan_interpret_jobs(),
            Repl => self.plan_repl_jobs(),
        }
    }

    pub async fn do_job(&mut self, job_id: JobId, job: JobType) -> Result<FinishType, TError> {
        trace!("Running job: {job_id:?} {job:?}");
        match job {
            JobType::ReportProgress(progress) => {
                let mut ui = self.ui.lock().expect("Getting UI");
                ui.report_progress(progress);
            }
            JobType::Load(file_id) => {
                let mut file = &mut file_id.get_mut(&mut self.files);
                file.contents = Some(std::fs::read_to_string(&file.path)?);
            }
            JobType::Lex(file_id) => {
                let file = &mut file_id.get_mut(&mut self.files);
                crate::tokens::lex(file)?;
            }
            JobType::Parse(file_id) => {
                let file = &mut file_id.get_mut(&mut self.files);
                crate::parser::parse(file)?;
            }
            JobType::LoadIntoInterpreter(_) => todo!(),
            JobType::RunInInterpreter(_) => todo!(),
            JobType::TypeCheck(_) => todo!(),
            JobType::TypeCheckAllModulesStart {
                file_id: _,
                down_stream: _,
            } => todo!(),
            JobType::TypeCheckAllModulesDone(_file) => todo!(),
            JobType::OptimizeAllModulesStart {
                file_id: _,
                down_stream: _,
                optimization_level: _,
            } => todo!(),
            JobType::Optimize(_module_id, _optimize_level) => todo!(),
            JobType::OptimizeAllModulesDone(_file_id) => todo!(),
            JobType::GenerateBinary => todo!(),
        }
        Ok(FinishType::Success)
    }

    pub async fn run_job(&mut self, job_id: JobId, job: JobType) -> Result<FinishType, TError> {

    }

    pub fn report_error(&mut self, error: Error) {
        if self.options.early_exit {
            self.jobs.wind_down();
        }
        let err_id = ErrorId::new(&mut self.errors, error).expect("Could not get Error Id");
        let err_ref = err_id.get(&self.errors);
        let mut ui = self.ui.lock().expect("Getting UI");
        ui.report_error(err_id, err_ref);
    }

    pub async fn run_job_loop(&mut self) {
        let errors = channel;
        let stats = channel;
        let job = channel;
        loop {
            let (job_id, job_kind) = if let Some((job_id, job)) = self.jobs.get_job() {
                trace!("Job details: {job_id:?} {job:#?}");
                let job_kind = job.kind;
                info!("Starting job: {}", InContext(&self, &job_kind));
                (job_id, job_kind)
            } else {
                break;
            };
            let result = self.do_job(job_id, job_kind);
            let finish_type = match result {
                Ok(finish_type) => finish_type,
                Err(error) => {
                    self.report_error(Error::new(error, None, None));
                    FinishType::Failed
                }
            };
            // Do some stuff
            self.jobs.finish_job(job_id, finish_type);
            let mut ui = self.ui.lock().expect("Getting UI");
            ui.report_job_counts(
                self.jobs.num_successful(),
                self.jobs.num_finished(),
                self.jobs.num(),
            ); // Maybe???
        }
        let mut ui = self.ui.lock().expect("Getting UI");
        ui.report_job_counts(
            self.jobs.num_successful(),
            self.jobs.num_finished(),
            self.jobs.num(),
        ); // Maybe???
    }
}

impl<'a, 'source> std::fmt::Display for InContext<'a, 'source, JobType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: Use the context (.0) to render the value (.1).
        write!(f, "JOB {:?}", self.1)
    }
}
