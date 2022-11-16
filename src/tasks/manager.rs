use crate::error::Error;

use log::{debug, trace};
use std::collections::HashMap;
use std::fmt::Debug;
use tokio::sync::broadcast;
use tokio::sync::mpsc;

use super::status::*;
use super::task_trait::*;
use super::TaskKind;

// TODO: Add timing information, etc.
// TODO: Support re-running multiple times for stability testing.
// TODO: Store the Tasks and their statuses in a contiguous vec.
// TODO: Still use hashing to look up tasks and their IDs.
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
        write!(f, "{num_succeeded}/{num_real}")?;
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
}

impl StatusReport {
    pub fn new(kind: TaskKind) -> Self {
        Self {
            kind,
            stats: TaskStats::default(),
        }
    }
}

#[derive(Debug, Default)]
pub struct ManagerConfig {
    pub disable_caching: bool,
    // TODO: Probably should be able to enable or disable stat & timing collection.
}

#[derive(Debug)]
pub struct TaskManager<T: Task> {
    task_receiver: TaskReceiverFor<T>,
    result_sender: TaskSenderFor<T>,
    stats_sender: mpsc::UnboundedSender<StatusReport>,
    stats_requester: broadcast::Receiver<()>,
    result_store: TaskResults<T>,
    stats: TaskStats,
    config: ManagerConfig,
}

impl<T: Debug + Task + 'static> TaskManager<T> {
    fn name() -> &'static str {
        let name = std::any::type_name::<T>();
        let last_lt = name.rfind('<').unwrap_or(name.len());
        let index = name.rfind(':').map(|i| i + 1).unwrap_or(1);
        &name[index..last_lt]
    }

    pub fn new(
        task_receiver: TaskReceiverFor<T>,
        result_sender: TaskSenderFor<T>,
        stats_sender: mpsc::UnboundedSender<StatusReport>,
        stats_requester: broadcast::Receiver<()>,
        config: ManagerConfig,
    ) -> Self {
        Self {
            task_receiver,
            result_sender,
            stats_sender,
            stats_requester,
            result_store: TaskResults::<T>::new(),
            stats: TaskStats::default(),
            config,
        }
    }

    pub async fn handle_update(&mut self, task: T, update: Update<T::Output, Error>) {
        trace!(
            "{} received update from task: {task:#?} {update:#?}",
            Self::name()
        );
        let caching_enabled = !self.config.disable_caching;
        let mut current_results = self
            .result_store
            .entry(task.get_hash())
            .or_insert_with(TaskStatus::new);
        let mut is_complete = false;
        let mut error = None;
        let results_so_far = &mut current_results.results;
        match update {
            Update::NextResult(res) => {
                self.stats.total_num_results += 1;
                self.result_sender
                    .send(res.clone())
                    .expect("Should be able to send results");
                if caching_enabled {
                    results_so_far.push(res);
                }
            }
            Update::FinalResult(res) => {
                self.stats.total_num_results += 1;
                self.stats.num_succeeded += 1;
                is_complete = true;
                self.result_sender
                    .send(res.clone())
                    .expect("Should be able to send results");
                if caching_enabled {
                    results_so_far.push(res);
                }
            }
            Update::Complete => {
                self.stats.num_succeeded += 1;
                is_complete = true;
            }
            Update::Failed(err) => {
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

    pub async fn handle_new_task(
        &mut self,
        result_or_error_sender: &mpsc::UnboundedSender<(T, Update<T::Output, Error>)>,
        task: T,
    ) {
        // Get a new job from 'upstream'.
        self.stats.num_requests += 1;
        let status = self
            .result_store
            .entry(task.get_hash())
            .or_insert_with(TaskStatus::new);
        if status.state != TaskState::New {
            self.stats.num_already_running += 1;
            return; // Done: Already running.
        }
        status.state = TaskState::Running;
        match (&status.state, T::RESULT_IS_CACHABLE) {
            // TODO: Consider that partial results 'should' still be safe to re-use and could pre-start later work.
            /* TaskState::Partial | */
            (TaskState::Complete, true) => {
                self.stats.num_cached += 1;
                trace!("{} cached task: {task:#?}", Self::name());
                for result in status.results.iter().cloned() {
                    self.result_sender
                        .send(result)
                        .expect("Should be able to send results");
                }
                return; // Done: Finished.
            }
            (TaskState::Complete, false) => {
                trace!("{} un-cacheable (will re-run): {task:#?}", Self::name())
            }
            // Continue on and re-launch the job, duplicated work should not propagate if completed.
            _ => trace!("{} task with status {status:#?}: {task:#?}", Self::name()),
        }
        // Launch the job!!!
        let result_or_error_sender = result_or_error_sender.clone();
        tokio::spawn(async move {
            // Tasks will report that they are running. Do not report them here.
            task.perform(result_or_error_sender).await;
        });
    }

    pub async fn run_loop(&mut self) {
        trace!("{} starting run_loop", Self::name());
        let (result_or_error_sender, mut result_or_error_receiver) =
            mpsc::unbounded_channel::<(T, Update<T::Output, Error>)>();

        loop {
            trace!("{}: Waiting for tasks...", Self::name());
            tokio::select! {
                Some((task, update)) = result_or_error_receiver.recv() => {
                    self.handle_update(task, update).await;
                },
                Some(task) = self.task_receiver.recv() => {
                    self.handle_new_task(&result_or_error_sender, task).await;
                },
                Ok(()) = self.stats_requester.recv() => {
                    let _ = self.stats_sender.send(StatusReport {
                        kind: <T as Task>::TASK_KIND,
                        stats: self.stats,
                    }); // TODO: !?
                }
                else => break,
            }
        }
        self.stats_sender
            .send(StatusReport {
                kind: <T as Task>::TASK_KIND,
                stats: self.stats,
            })
            .unwrap_or_else(|err| {
                debug!("Could not report final task manager stats: {}", err);
            });
        trace!("{} no more tasks or results. Terminating...", Self::name());
    }
}
