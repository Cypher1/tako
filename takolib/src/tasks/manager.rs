use crate::error::Error;

use super::status::*;
use super::task_trait::*;
use super::TaskKind;
use log::{debug, trace};
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::Arc;
use std::sync::Mutex;
use tokio::spawn;
use tokio::sync::broadcast;
use tokio::sync::mpsc;

// TODO(debugging): Add timing information, etc.
// TODO(debugging): Support re-running multiple times for stability testing.
// TODO(perf): Store the Tasks and their statuses in a contiguous vec.
// TODO(perf): Still use hashing to look up tasks and their IDs.
// This should be the pre-computed hash, to avoid sending and cloning tasks.
pub type TaskResults<T> = HashMap<TaskId, TaskStatus<<T as Task>::Output, Error>>;

#[derive(Default, Debug, Clone, Copy)]
pub struct TaskStats {
    num_requests: u32,
    total_num_results: u32,
    num_already_running: u32,
    num_cached: u32,
    num_failed: u32,
    num_succeeded: u32,
}

impl std::fmt::Display for TaskStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let TaskStats {
            num_requests,
            total_num_results: _,
            num_already_running,
            num_cached,
            num_failed,
            num_succeeded,
        } = &self;
        if *num_requests == 0 {
            return write!(f, "...");
        }
        let num_real = num_requests - num_already_running;
        let num_done = num_succeeded + num_cached;
        write!(f, "{num_done}/{num_real}")?;
        let items: Vec<String> = vec![(num_cached, "cached"), (num_failed, "failed")]
            .iter()
            .filter(|(n, _label)| **n > 0)
            .map(|(n, label)| format!("{n} {label}"))
            .collect();
        if !items.is_empty() {
            write!(f, "({})", items.join(" "))?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct StatusReport {
    pub kind: TaskKind,
    pub stats: TaskStats,
    pub errors: HashMap<TaskId, Error>,
}

impl StatusReport {
    pub fn new(kind: TaskKind) -> Self {
        Self {
            kind,
            stats: TaskStats::default(),
            errors: HashMap::default(),
        }
    }
}

#[derive(Debug)]
pub struct TaskManager<T: Task> {
    result_store: TaskResults<T>,
    stats: TaskStats,
    errors: HashMap<TaskId, Error>,
}

impl<T: Debug + Task + 'static> Default for TaskManager<T> {
    fn default() -> Self {
        Self {
            result_store: TaskResults::<T>::new(),
            stats: TaskStats::default(),
            errors: HashMap::new(),
        }
    }
}

impl<T: Debug + Task + 'static> TaskManager<T> {
    fn name() -> &'static str {
        let name = std::any::type_name::<T>();
        let last_lt = name.rfind('<').unwrap_or(name.len());
        let index = name.rfind(':').map(|i| i + 1).unwrap_or(1);
        &name[index..last_lt]
    }

    pub fn new() -> Self {
        Self::default()
    }

    pub fn start(
        this: &Arc<Mutex<Self>>,
        mut task_receiver: TaskReceiverFor<T>,
        results_sender: ResultSenderFor<T>,
    ) {
        let this = this.clone();
        spawn(async move {
            let (tx, mut rx) = mpsc::unbounded_channel();
            loop {
                tokio::select! {
                    Some(task) = task_receiver.recv() => {
                        let mut this = this.lock().expect("");
                        trace!("New task: {task:?}");
                        this.handle_new_task(task, &tx, results_sender.clone());
                    }
                    Some((task, update)) = rx.recv() => {
                        let mut this = this.lock().expect("");
                        trace!("New update: {update:?}");
                        this.handle_update(task, update, results_sender.clone());
                    }
                    else => break,
                }
            }
        });
    }

    pub fn handle_update(
        &mut self,
        task: T,
        update: Update<T::Output, Error>,
        results_sender: ResultSenderFor<T>,
    ) {
        trace!(
            "{} received update from task: {task:#?} {update:#?}",
            Self::name()
        );
        let task_id = task.get_hash();
        let mut current_results = self
            .result_store
            .entry(task_id)
            .or_insert_with(TaskStatus::new);
        let mut is_complete = false;
        let mut error = None;
        let results_so_far = &mut current_results.results;
        match update {
            Update::NextResult(res) => {
                self.stats.total_num_results += 1;
                results_sender
                    .send(res.clone())
                    .expect("Should be able to send results");
                if T::RESULT_IS_CACHABLE {
                    results_so_far.push(res);
                }
            }
            Update::FinalResult(res) => {
                self.stats.total_num_results += 1;
                self.stats.num_succeeded += 1;
                is_complete = true;
                results_sender
                    .send(res.clone())
                    .expect("Should be able to send results");
                if T::RESULT_IS_CACHABLE {
                    results_so_far.push(res);
                }
            }
            Update::Complete => {
                self.stats.num_succeeded += 1;
                is_complete = true;
            }
            Update::Failed(err) => {
                self.errors.insert(task_id, err.clone());
                self.stats.num_failed += 1;
                is_complete = true; // For completeness...?
                error = Some(err);
            }
        };
        current_results.state = match (error, is_complete) {
            (Some(err), _is_complete) => TaskState::Failure(err),
            (None, /*is_complete*/ true) => TaskState::Complete,
            (None, /*is_complete*/ false) => TaskState::Partial,
        };
    }

    pub fn handle_new_task(
        &mut self,
        task: T,
        result_or_error_sender: &mpsc::UnboundedSender<(T, Update<T::Output, Error>)>,
        results_sender: ResultSenderFor<T>,
    ) {
        // Get a new job from 'upstream'.
        self.stats.num_requests += 1;
        let status = self
            .result_store
            .entry(task.get_hash())
            .or_insert_with(TaskStatus::new);
        if task.invalidate() {
            *status = TaskStatus::new(); // Forget the previous value!
        }
        // TODO(caching): Find the running one and listen for it's results.
        //if status.state == TaskState::Running {
        //self.stats.num_already_running += 1;
        //return; // Done: Already running.
        //}
        match (&status.state, T::RESULT_IS_CACHABLE) {
            // TODO(caching): Consider that partial results 'should' still be safe to re-use and could pre-start later work.
            /* TaskState::Partial | */
            (TaskState::Complete, true) => {
                self.stats.num_cached += 1;
                trace!("{} cached task: {task:#?}", Self::name());
                for result in status.results.iter().cloned() {
                    results_sender
                        .send(result)
                        .expect("Should be able to send results");
                }
                return; // Done: Finished.
            }
            (_, false) => {
                trace!("{} un-cacheable (will re-run): {task:#?}", Self::name())
            }
            // Continue on and re-launch the job, duplicated work should not propagate if completed.
            _ => trace!("{} task with status {status:#?}: {task:#?}", Self::name()),
        }
        // Launch the job!!!
        status.state = TaskState::Running;
        let result_or_error_sender = result_or_error_sender.clone();
        spawn(async move {
            // Tasks will report that they are running. Do not report them here.
            task.perform(result_or_error_sender).await;
        });
    }

    pub async fn report_stats(
        this: Arc<Mutex<Self>>,
        mut stats_requester: broadcast::Receiver<()>,
        stats_sender: mpsc::UnboundedSender<StatusReport>,
    ) {
        trace!("{} starting report_stats", Self::name());
        // trace!("{}: Waiting for tasks...", Self::name());
        while stats_requester.recv().await.is_ok() {
            let this = this.lock().expect("");
            if stats_sender
                .send(StatusReport {
                    kind: <T as Task>::TASK_KIND,
                    stats: this.stats,
                    errors: this.errors.clone(),
                })
                .is_err()
            {
                debug!("Stats receiver closed");
                break;
            }
        }
        let this = this.lock().expect("");
        stats_sender
            .send(StatusReport {
                kind: <T as Task>::TASK_KIND,
                stats: this.stats,
                errors: this.errors.clone(),
            })
            .unwrap_or_else(|err| {
                debug!("Could not report final task manager stats: {err}");
            });
        trace!("{} no more tasks or results. Terminating...", Self::name());
    }
}
