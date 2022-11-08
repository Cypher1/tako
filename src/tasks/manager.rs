use crate::error::Error;

use log::trace;
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::{Arc, Mutex};
use tokio::sync::{mpsc, watch, RwLock};

use super::status::*;
use super::task_trait::*;
use super::TaskKind;

// TODO: Add timing information, etc.
// TODO: Support re-running multiple times for stability testing.
// TODO: Store the Tasks and their statuses in a contiguous vec.
// TODO: Still use hashing to look up tasks and their IDs.
// This should be the pre-computed hash, to avoid sending and cloning tasks.
pub type TaskResults<T> = HashMap<T, TaskStatus<<T as Task>::Output, Error>>;

#[derive(Default, Debug, Clone)]
pub struct TaskStats {
    num_requests: u32,
    total_num_results: u32,
    num_already_running: u32,
    num_cached: u32,
    num_failed: u32,
    num_succeeded: u32,
}

#[derive(Debug)]
pub struct TaskManagerRegistration {
    pub kind: TaskKind,
    pub status_report_receiver: watch::Receiver<StatusReport>,
}

#[derive(Debug, Clone)]
pub enum StatusReport {
    StartingUp(TaskKind),
    StatsUpdate { kind: TaskKind, stats: TaskStats },
    ShuttingDown(TaskKind),
}

#[derive(Debug, Default)]
pub struct ManagerConfig {
    pub disable_caching: bool,
    // TODO: Probably should be able to enable or disable stat & timing collection.
}

#[derive(Debug)]
pub struct TaskManager<T: Task> {
    result_store: Arc<Mutex<TaskResults<T>>>,
    task_receiver: TaskReceiverFor<T>,
    status_report_sender: watch::Sender<StatusReport>,
    result_sender: TaskSenderFor<T>,
    // status_sender: mpsc::Sender<ManagerStats>,
    stats: Arc<Mutex<TaskStats>>,
    config: Arc<RwLock<ManagerConfig>>, // Use a RwLock so that reads don't block.
}

impl<T: Debug + Task + 'static> TaskManager<T> {
    fn task_name() -> &'static str {
        let name = std::any::type_name::<T>();
        let last_lt = name.rfind('<').unwrap_or(name.len());
        let index = name.rfind(':').map(|i| i + 1).unwrap_or(1);
        &name[index..last_lt]
    }

    pub fn new(
        task_receiver: TaskReceiverFor<T>,
        result_sender: TaskSenderFor<T>,
        status_report_sender: watch::Sender<StatusReport>,
        config: ManagerConfig,
    ) -> Self {
        Self {
            result_store: Arc::new(Mutex::new(TaskResults::new())),
            task_receiver,
            result_sender,
            status_report_sender,
            stats: Arc::new(Mutex::new(TaskStats::default())),
            config: Arc::new(RwLock::new(config)),
        }
    }

    pub async fn run_loop(&mut self) {
        trace!("{} starting run_loop", Self::task_name());
        let (result_or_error_sender, mut result_or_error_receiver) =
            mpsc::unbounded_channel::<(T, Update<T::Output, Error>)>();
        let stats = self.stats.clone();
        let result_store = self.result_store.clone();
        let result_sender = self.result_sender.clone();
        let config = self.config.clone();
        tokio::spawn(async move {
            trace!("{}: Waiting for results...", Self::task_name());
            while let Some((task, update)) = result_or_error_receiver.recv().await {
                trace!(
                    "{} received update from task: {task:#?} {update:#?}",
                    Self::task_name()
                );
                // Reading from an RwLock should be near instant unless there is writing occuring.
                let caching_enabled = !config.read().await.disable_caching;
                let mut result_store = result_store
                    .lock()
                    .expect("Should be able to get the result store");
                let mut current_results = result_store.entry(task).or_insert_with(TaskStatus::new);
                let mut is_complete = false;
                let mut error = None;
                {
                    let results_so_far = &mut current_results.results;
                    let mut stats = stats
                        .lock()
                        .expect("Should be able to get task stats store");
                    match update {
                        Update::NextResult(res) => {
                            stats.total_num_results += 1;
                            result_sender
                                .send(res.clone())
                                .expect("Should be able to send results");
                            if caching_enabled {
                                results_so_far.push(res);
                            }
                        }
                        Update::FinalResult(res) => {
                            stats.total_num_results += 1;
                            stats.num_succeeded += 1;
                            is_complete = true;
                            result_sender
                                .send(res.clone())
                                .expect("Should be able to send results");
                            if caching_enabled {
                                results_so_far.push(res);
                            }
                        }
                        Update::Complete => {
                            stats.num_succeeded += 1;
                            is_complete = true;
                        }
                        Update::Failed(err) => {
                            stats.num_failed += 1;
                            is_complete = true; // For completeness...?
                            error = Some(err);
                        }
                    };
                }
                current_results.state = match (error, is_complete) {
                    (Some(err), _is_complete) => TaskState::Failure(err),
                    (None, /*is_complete*/ true) => TaskState::Complete,
                    (None, /*is_complete*/ false) => TaskState::Partial,
                };
            }
            trace!(
                "{} no more results... Finishing listening loop.",
                Self::task_name()
            );
        });

        trace!("{}: Waiting for tasks...", Self::task_name());
        let stats = self.stats.clone();
        'listening: while let Some(task) = self.task_receiver.recv().await {
            // Get a new job from 'upstream'.
            // trace!("{} received task: {task:#?}", Self::task_name());
            {
                let mut stats = stats
                    .lock()
                    .expect("Should be able to get task stats store");
                stats.num_requests += 1;
            }
            let status = {
                let mut result_store = self
                    .result_store
                    .lock()
                    .expect("Should be able to get the result store");
                // We'll need to forward these on, so we can clone now and drop the result_store lock earlier!
                let status = result_store
                    .entry(task.clone())
                    .or_insert_with(TaskStatus::new);
                if status.state != TaskState::New {
                    let mut stats = stats
                        .lock()
                        .expect("Should be able to get task stats store");
                    stats.num_already_running += 1;
                    continue 'listening; // Already running.
                }
                status.state = TaskState::Running;
                status.clone()
            };
            match (&status.state, T::RESULT_IS_CACHABLE) {
                // TODO: Consider that partial results 'should' still be safe to re-use and could pre-start later work.
                /* TaskState::Partial | */
                (TaskState::Complete, true) => {
                    let mut stats = stats
                        .lock()
                        .expect("Should be able to get task stats store");
                    stats.num_cached += 1;
                    trace!("{} cached task: {task:#?}", Self::task_name());
                    for result in status.results {
                        self.result_sender
                            .send(result)
                            .expect("Should be able to send results");
                    }
                    continue 'listening; // i.e. go look for another task.
                }
                (TaskState::Complete, false) => {
                    trace!(
                        "{} un-cacheable (will re-run): {task:#?}",
                        Self::task_name()
                    );
                }
                // Continue on and re-launch the job, duplicated work should not propagate if completed.
                _ => {
                    trace!(
                        "{} task with status {status:#?}: {task:#?}",
                        Self::task_name()
                    );
                }
            }
            // Launch the job!!!
            let result_or_error_sender = result_or_error_sender.clone();
            tokio::spawn(async move {
                // Tasks will report that they are running. Do not report them here.
                task.perform(result_or_error_sender).await;
            });
        }
        self.status_report_sender
            .send(StatusReport::ShuttingDown(<T as Task>::TASK_KIND))
            .expect("status_report_sender should shut down last");
        let stats = self
            .stats
            .lock()
            .expect("Should be able to get task stats store");
        self.status_report_sender
            .send(StatusReport::StatsUpdate {
                kind: <T as Task>::TASK_KIND,
                stats: stats.clone(),
            })
            .expect("status_report_sender should shut down last");
        trace!(
            "{} no more tasks... Finishing run_loop: {}",
            Self::task_name(),
            format!("{:?}", &stats)
        );
    }
}
