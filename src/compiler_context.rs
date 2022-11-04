use async_trait::async_trait;
use std::sync::{Arc, Mutex};
use std::collections::VecDeque;
use log::{info, trace};
use tokio::sync::mpsc;
use crate::cli_options::Options;
use crate::compiler_tasks::{
    Task,
    TaskState,
    TaskStore,
    TaskKind::{self, *},
    OptionsRef,
};
use crate::error::{Error, ErrorId, TError};
use crate::ui::UserInterface;

#[derive(Debug, Clone)]
pub struct CompilerContext {
    store: TaskStore,
    ui: Arc<Mutex<dyn UserInterface + Send>>,
    options: Arc<Mutex<Options>>,
}

#[async_trait]
impl Task for CompilerContext {
    type Output = ();
    const TASK_KIND: TaskKind = TaskKind::Launch;

    fn options(&self) -> &OptionsRef {
        &self.options
    }

    async fn perform_impl(self) -> Result<Self::Output, TError> {
        // TODO: ???
        Ok(())
    }
}
/*

fn make_ui_arc<T: UserInterface + Send + 'static>(value: T) -> Arc<Mutex<dyn UserInterface + Send>> {
    Arc::new(Mutex::new(value))
}

impl CompilerContext {
    pub fn new(options: Options) -> Self {
        Self::from_options(options)
    }

    pub fn from_options(options: Options) -> Self {
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
            options: Arc::new(options),
            ui,
            store: CompilerStorage::default(),
        }
    }

    fn plan_parse_jobs(&self, jobs: &mut JobStore<JobType>, path: &str) -> (FileId, JobId) {
        let mut files = self.files.lock().expect("Plan job should be able to get files");
        let fileid = FileId::new(
            &mut files,
            File::from_path(path),
        )
        .expect("Too many file ids");
        let load_job = jobs.add_job(Load(fileid), vec![]);
        let lex_job = jobs.add_job(Lex(fileid), vec![load_job]);
        let parse_job = jobs.add_job(Parse(fileid), vec![lex_job]);
        (fileid, parse_job)
    }

    fn plan_build_job(
        &self, jobs: &mut JobStore<JobType>,
        path: &str,
    ) -> (JobId, JobId, Option<JobId>, Option<JobId>, JobId) {
        let (file_id, parse_job) = self.plan_parse_jobs(jobs, path);
        let type_check_job_discover = jobs.add_job(
            TypeCheckAllModulesStart {
                file_id,
                down_stream: None,
            },
            vec![parse_job],
        );
        let type_check_job = jobs.add_job(
            TypeCheckAllModulesDone(file_id),
            vec![type_check_job_discover],
        );
        // Give the 'starter' a way to link up dependencies
        match &mut jobs.get(type_check_job_discover).kind {
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
            let optimize_file_discover = jobs.add_job(
                OptimizeAllModulesStart {
                    file_id,
                    down_stream: None,
                    optimization_level,
                },
                vec![type_check_job],
            );
            let optimize_job = jobs.add_job(
                OptimizeAllModulesDone(file_id),
                vec![optimize_file_discover],
            );
            // Give the 'starter' a way to link up dependencies
            match &mut jobs.get(optimize_file_discover).kind {
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

    fn plan_build_jobs(&self, jobs: &mut JobStore<JobType>) {
        let mut prep_jobs = Vec::new();
        let mut type_check_jobs = Vec::new();
        let mut optimize_jobs = Vec::new();
        let mut code_gen_jobs = Vec::new();
        for path in &self.options.files {
            let (parse_job, type_check_job, optimize_job, code_gen_job, _file_ready_job) =
                self.plan_build_job(jobs, path);
            prep_jobs.push(parse_job);
            type_check_jobs.push(type_check_job);
            if let Some(optimize_job) = optimize_job {
                optimize_jobs.push(optimize_job);
            }
            if let Some(code_gen_job) = code_gen_job {
                code_gen_jobs.push(code_gen_job);
            }
        }
        jobs.add_job(ReportProgress(AllFilesParsed), prep_jobs);
        jobs.add_job(ReportProgress(GlobalTypeCheckDone), type_check_jobs);
        jobs.add_job(ReportProgress(GlobalOptimizeDone), optimize_jobs);
        jobs.add_job(ReportProgress(GlobalCodeGenDone), code_gen_jobs);
    }

    fn plan_interpret_jobs(&self, jobs: &mut JobStore<JobType>) {
        let mut runs = Vec::new();
        for path in &self.options.files {
            let (file_id, init_job_id) = self.plan_parse_jobs(jobs, path);
            let load_job = jobs
                .add_job(LoadIntoInterpreter(file_id), vec![init_job_id]);
            runs.push(jobs.add_job(RunInInterpreter(file_id), vec![load_job]));
        }
        jobs.add_job(ReportProgress(AllFilesRun), runs);
    }
    fn plan_repl_jobs(&self, jobs: &mut JobStore<JobType>) {
        let mut loads = Vec::new();
        for path in &self.options.files {
            let (file_id, init_job_id) = self.plan_parse_jobs(jobs, path);
            let load_job = jobs
                .add_job(LoadIntoInterpreter(file_id), vec![init_job_id]);
            loads.push(load_job);
        }
        jobs.add_job(ReportProgress(ReplReady), loads);
    }
    pub fn plan_jobs(&self) {
        let mut jobs = self.jobs.lock().expect("Should be able to get the jobs store");
        use crate::cli_options::Command::*;
        match self.options.cmd {
            Build => self.plan_build_jobs(&mut jobs),
            Interpret => self.plan_interpret_jobs(&mut jobs),
            Repl => self.plan_repl_jobs(&mut jobs),
        }
    }

    pub async fn do_job(&self, progress_sender: &mpsc::Sender<Progress>, job_id: JobId, job: JobType) -> JobResult {
        trace!("Running job: {job_id:?} {job:?}");
        match job {
            JobType::ReportProgress(progress) => {
                progress_sender.send(progress).await.expect("send progress error")
            }
            JobType::Load(file_id) => {
                let mut files = self.files.lock().expect("Do job should be able to get files");
                let mut file = &mut file_id.get_mut(&mut files);
                file.contents = Some(std::fs::read_to_string(&file.path)?);
            }
            JobType::Lex(file_id) => {
                let mut files = self.files.lock().expect("Do job should be able to get files");
                let file = &mut file_id.get_mut(&mut files);
                crate::tokens::lex(file)?;
            }
            JobType::Parse(file_id) => {
                let mut files = self.files.lock().expect("Do job should be able to get files");
                let file = &mut file_id.get_mut(&mut files);
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

    pub async fn run_job_loop(self) {
        // This is pretty arbitrary... make it bigger if the compiler starts waiting for the
        // scheduler.
        const MAX_SCHEDULE_LENGTH: usize = 2;
        // This is pretty arbitrary... make it bigger if the compiler starts waiting for the UI.
        const MAX_RESULT_LAG: usize = 10;
        const MAX_PROGRESS_LAG: usize = 10;
        let job_queue: Arc<Mutex<VecDeque<(JobId, JobType)>>> = Arc::new(Mutex::new(VecDeque::new()));
        let (result_sender, mut result_reader) = mpsc::channel::<JobResultReport>(MAX_RESULT_LAG);
        let (progress_sender, mut progress_reader) = mpsc::channel::<Progress>(MAX_PROGRESS_LAG);
        {
            // Job Runner!
            let job_runner = self.clone();
            let job_queue = job_queue.clone();
            tokio::spawn(async move {
                let progress_sender = progress_sender.clone();
                loop {
                    let (job_id, job_kind) = {
                        let mut job_queue = job_queue.lock().expect("Job Runner should be able to get the job_queue");
                        if let Some((job_id, job_kind)) = job_queue.pop_front() {
                            (job_id, job_kind)
                        } else {
                            continue;
                        }
                    };
                    // info!("Starting job: {}", InContext(&job_runner, &job_kind));
                    let result = job_runner.do_job(&progress_sender, job_id, job_kind).await;
                    result_sender.send((job_id, job_kind, result)).await.expect("send result failed"); // Yield this core...
                }
            });
        }
        {
            // UI Manager
            let ui_manager = self.clone();
            tokio::spawn(async move {
                while let Some(progress) = progress_reader.recv().await {
                    let mut ui = ui_manager.ui.lock().expect("Getting UI");
                    ui.report_progress(progress);
                }
                let mut ui = ui_manager.ui.lock().expect("Getting UI");
                let jobs = ui_manager.jobs.lock().expect("Should be able to get the jobs store");
                ui.report_job_counts(
                    jobs.num_successful(),
                    jobs.num_finished(),
                    jobs.num(),
                ); // Maybe???
            });
        }

        // Scheduler!
        {
        let scheduler = self.clone();
        let job_queue = job_queue.clone();
            loop {
                let mut jobs = scheduler.jobs.lock().expect("Should be able to get the jobs store");
                let mut job_queue = job_queue.lock().expect("Scheduler should be able to get the job_queue");
                let mut errors = scheduler.errors.lock().expect("Scheduler should be able to get the job_queue");
                while let Some((job_id, _job_kind, result)) = result_reader.recv().await {
                    let finish_type = match result {
                        Ok(finish_type) => finish_type,
                        Err(error) => {
                            let error = Error::new(error, None, None);
                            if scheduler.options.early_exit {
                                jobs.wind_down();
                            }
                            let err_id = ErrorId::new(&mut errors, error).expect("Could not get Error Id");
                            let err_ref = err_id.get(&errors);
                            let mut ui = scheduler.ui.lock().expect("Getting UI");
                            ui.report_error(err_id, err_ref);
                            FinishType::Failed
                        }
                    };
                    jobs.finish_job(job_id, finish_type);
                }
                while job_queue.len() < MAX_SCHEDULE_LENGTH {
                    // TODO: Work out if the jobs are all done or if there are none free...
                    let try_next_job = jobs.get_job();
                    let next_job = match try_next_job {
                        GetJob::Job(job_id, job) => {
                            trace!("Scheduling job: {job_id:?} {job:#?}");
                            let job_kind = job.kind;
                            (job_id, job_kind)
                        }
                        GetJob::NoneReady => { break; }
                        GetJob::Finished => { return }
                    };
                    job_queue.push_back(next_job);
                }
                // Yield this core...
            }
        }
    }
}
*/
