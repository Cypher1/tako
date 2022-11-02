use super::job::{FinishType, Job, JobId, JobState};

#[derive(Debug)]
pub struct JobStore<JobType> {
    ready: Vec<JobId<JobType>>,
    all_jobs: Vec<Job<JobType>>,
    terminating: bool,
}

impl<JobType> Default for JobStore<JobType> {
    fn default() -> Self {
        Self {
            ready: Vec::new(),
            all_jobs: Vec::new(),
            terminating: false,
        }
    }
}

impl<JobType> JobStore<JobType> {
    pub fn num_ready(&self) -> usize {
        self.ready.len()
    }

    pub fn num_finished(&self) -> usize {
        let mut num = 0;
        for job in self.all_jobs.iter() {
            if let JobState::Finished(_) = job.state {
                num += 1;
            }
        }
        num
    }

    pub fn num(&self) -> usize {
        self.all_jobs.len()
    }

    pub fn num_running(&self) -> usize {
        let mut num = 0;
        for job in self.all_jobs.iter() {
            if let JobState::Running = job.state {
                num += 1;
            }
        }
        num
    }

    pub fn num_waiting(&self) -> usize {
        let mut num = 0;
        for job in self.all_jobs.iter() {
            if let JobState::Waiting = job.state {
                num += 1;
            }
        }
        num
    }

    pub fn wind_down(&mut self) {
        self.terminating = true;
    }

    pub fn get_job(&mut self) -> Option<(JobId<JobType>, &Job<JobType>)> {
        if self.terminating {
            return None;
        }
        let mut best = None;
        let ready = &mut self.ready;
        // TODO: consider a sorted datastructure.
        for (index, job_id) in ready.iter().enumerate() {
            let job = job_id.get(&self.all_jobs);
            let count = job.dependents.len();
            if best.map(|(max, _index)| max < count).unwrap_or(true) {
                best = Some((count, index));
            }
        }
        let mut job_id = ready.pop()?; // pop swap to remove in O(1).
        let index = if let Some((_count, index)) = best {
            index
        } else {
            return None;
        };
        if index < ready.len() {
            std::mem::swap(&mut job_id, &mut ready[index]);
        }
        let job = job_id.get_mut(&mut self.all_jobs);
        job.state = JobState::Running;
        Some((job_id, job)) // Should not be mutable
    }

    pub fn add_job(&mut self, job: Job<JobType>) -> JobId<JobType> {
        let id = JobId::new(
            &mut self.all_jobs,
            job
        );
        for dep in &job.dependencies {
            dep.get_mut(&mut self.all_jobs).dependents.push(id);
        }
        self.try_make_ready(id);
        id
    }

    pub fn restart(&mut self, job_id: JobId<JobType>)
    where
        JobType: std::fmt::Debug,
    {
        let job = job_id.get_mut(&mut self.all_jobs);
        if job.state == JobState::Running {
            eprintln!(
                "Job {job_id:?} {:?} restarted while still running, may clobber",
                &job
            );
        }
        job.state = JobState::Waiting;
        self.try_make_ready(job_id);
    }

    fn try_make_ready(&mut self, job_id: JobId<JobType>) {
        let job = job_id.get(&self.all_jobs);
        if job.state != JobState::Waiting {
            return; // Already running or finished, wait to retry.
        }
        for dep in &job.dependencies {
            let state = &dep.get(&self.all_jobs).state;
            if let JobState::Finished(_) = state {
                continue;
            }
            return; // Not ready, leave as is.
        }
        let job = job_id.get_mut(&mut self.all_jobs);
        job.state = JobState::Ready;
        self.ready.push(job_id);
    }

    pub fn finish_job(&mut self, job_id: JobId<JobType>, result: FinishType) {
        {
            let job = job_id.get_mut(&mut self.all_jobs);
            job.state = JobState::Finished(result);
        }
        let deps = job_id.get(&self.all_jobs).dependents.clone();
        for dep_id in deps {
            self.try_make_ready(dep_id);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    type JobType = &'static str;

    #[test]
    fn jobs_doesnt_invent_jobs_from_nowhere() {
        let mut jobs: JobStore<JobType> = JobStore::default();
        assert!(jobs.get_job().is_none());
    }

    #[test]
    fn job_counts_start_at_zero() {
        let jobs: JobStore<JobType> = JobStore::default();
        assert_eq!(jobs.num_finished(), 0);
        assert_eq!(jobs.num_waiting(), 0);
        assert_eq!(jobs.num(), 0);
        assert_eq!(jobs.num_ready(), 0);
        assert_eq!(jobs.num_running(), 0);
    }

    #[test]
    fn job_counts_update_with_new_jobs() {
        let mut jobs = JobStore::default();
        jobs.add_job(Job::new((), vec![]));
        assert_eq!(jobs.num_finished(), 0);
        assert_eq!(jobs.num_waiting(), 0);
        assert_eq!(jobs.num(), 1);
        assert_eq!(jobs.num_ready(), 1);
        assert_eq!(jobs.num_running(), 0);
    }

    #[test]
    fn jobs_maintain_their_job_type() {
        let mut jobs = JobStore::default();
        let job1 = jobs.add_job(Job::new("job1", vec![]));
        let job2 = jobs.add_job(Job::new("job2", vec![job1]));
        let todo = {
            let todo = jobs.get_job();
            assert!(todo.is_some());
            let todo = todo.unwrap();
            assert_eq!(todo.0, job1);
            assert_eq!(todo.1.ty, "job1");
            todo.0
        };
        jobs.finish_job(todo, FinishType::Success);
        dbg!(&jobs);
        let todo = jobs.get_job();
        assert!(todo.is_some());
        let todo = todo.unwrap();
        assert_eq!(todo.0, job2);
        assert_eq!(todo.1.ty, "job2");
        assert!(jobs.get_job().is_none());
    }

    #[test]
    fn jobs_track_their_dependents() {
        let mut jobs = JobStore::default();
        let job1 = jobs.add_job(Job::new("job1", vec![]));
        let job2 = jobs.add_job(Job::new("job2", vec![job1]));
        let todo = jobs.get_job();
        assert!(todo.is_some());
        let todo = todo.unwrap();
        assert_eq!(todo.0, job1);
        assert_eq!(todo.1.dependents, vec![job2]);
    }

    #[test]
    fn num_jobs_counts_are_consistent() {
        let mut jobs = JobStore::default();
        let job1 = jobs.add_job(Job::new("job1", vec![]));
        jobs.add_job(Job::new("job2", vec![job1]));
        assert_eq!(jobs.num_finished(), 0);
        assert_eq!(jobs.num_waiting(), 1);
        assert_eq!(jobs.num(), 2);
        assert_eq!(jobs.num_ready(), 1);
        assert_eq!(jobs.num_running(), 0);
        let todo = {
            let todo = jobs.get_job();
            assert!(todo.is_some());
            let todo = todo.unwrap();
            assert_eq!(todo.0, job1);
            assert_eq!(todo.1.ty, "job1");
            todo.0
        };
        assert_eq!(jobs.num_finished(), 0);
        assert_eq!(jobs.num_waiting(), 1);
        assert_eq!(jobs.num(), 2);
        assert_eq!(jobs.num_ready(), 0);
        assert_eq!(jobs.num_running(), 1);

        jobs.finish_job(todo, FinishType::Success);
        assert_eq!(jobs.num_finished(), 1);
        assert_eq!(jobs.num_waiting(), 0);
        assert_eq!(jobs.num(), 2);
        assert_eq!(jobs.num_ready(), 1);
        assert_eq!(jobs.num_running(), 0);
    }

    #[test]
    fn wind_down_stops_handing_out_new_jobs() {
        let mut jobs = JobStore::default();
        jobs.add_job(Job::new("job1", vec![]));
        jobs.add_job(Job::new("job2", vec![]));
        jobs.wind_down();
        assert!(jobs.get_job().is_none());
    }
}
