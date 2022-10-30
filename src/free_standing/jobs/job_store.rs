use super::job::{FinishType, Job, JobState, JobId};

pub struct JobStore<JobType> {
    ready: Vec<JobId<JobType>>,
    jobs: Vec<Job<JobType>>,
    terminating: bool,
}

impl<JobType: std::fmt::Debug> JobStore<JobType> {
    pub fn num_ready(&self) -> usize {
        self.ready.len()
    }

    pub fn num_finished(&self) -> usize {
        let mut num = 0;
        for job in self.jobs.iter() {
            if let JobState::Finished(_) = job.state {
                num += 1;
            }
        }
        num
    }

    pub fn num(&self) -> usize {
        self.jobs.len()
    }

    pub fn num_waiting(&self) -> usize {
        self.num() - self.num_ready() - self.num_finished()
    }

    pub fn wind_down(&mut self) {
        self.terminating = true;
    }

    pub fn get(&mut self) -> Option<(JobId<JobType>, &Job<JobType>)> {
        if self.terminating {
            return None;
        }
        let mut best = None;
        let ready = &mut self.ready;
        // TODO: consider a sorted datastructure.
        let mut index = 0;
        for job_id in ready.iter() {
            let job = job_id.get(&self.jobs);
            let count = job.dependents.len();
            if best.map(|(max, _index)|max < count).unwrap_or(true) {
                best = Some((count, index));
            }
            index += 1;
        }
        let mut job_id = if let Some(job_id) = ready.pop() { // pop swap to remove in O(1).
            job_id
        } else {
            return None;
        };
        let index = if let Some((_count, index)) = best {
            index
        } else {
            return None;
        };
        if index < ready.len() {
            std::mem::swap(&mut job_id, &mut ready[index]);
        }
        let job = job_id.get_mut(&mut self.jobs);
        job.state = JobState::Running;
        Some((job_id, job)) // Should not be mutable
    }

    pub fn add_job(&mut self, job: Job<JobType>) -> JobId<JobType> {
        use std::convert::TryInto;
        let id = JobId::new(self.jobs.len().try_into().unwrap_or_else(|e|panic!("Too many job ids: {}", e)));
        for dep in &job.dependencies {
            dep.get_mut(&mut self.jobs).dependents.push(id);
        }
        self.jobs.push(job);
        self.try_make_ready(id);
        id
    }

    pub fn restart(&mut self, job_id: JobId<JobType>) {
        let job = job_id.get_mut(&mut self.jobs);
        if job.state == JobState::Running {
            eprintln!("Job {:?} {:?} restarted while still running, may clobber", job_id, &job);
        }
        job.state = JobState::Waiting;
        self.try_make_ready(job_id);
    }

    fn try_make_ready(&mut self, job_id: JobId<JobType>) {
        let job = job_id.get(&self.jobs);
        if job.state != JobState::Waiting {
            return; // Already running or finished, wait to retry.
        }
        for dep in &job.dependencies {
            if let JobState::Finished(_) = dep.get(&self.jobs).state {
                continue;
            }
            return; // Not ready, leave as is.
        }
        let job = job_id.get_mut(&mut self.jobs);
        job.state = JobState::Ready;
        self.ready.push(job_id);
    }

    pub fn finish_job(&mut self, job_id: JobId<JobType>, result: FinishType) {
        let deps = job_id.get(&self.jobs).dependents.clone();
        for dep_id in deps {
            self.try_make_ready(dep_id);
        }
        let job = job_id.get_mut(&mut self.jobs);
        job.state = JobState::Finished(result);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    type JobType = &'static str;

    #[test]
    fn jobs_doesnt_invent_jobs_from_nowhere() {
        let mut jobs = JobStore::default();
        assert!(jobs.get().is_none());
    }

    #[test]
    fn job_counts_start_at_zero() {
        let mut jobs: JobStore<JobType> = JobStore::default();
        assert_eq!(jobs.num_finished(), 0);
        assert_eq!(jobs.num_waiting(), 0);
        assert_eq!(jobs.num(), 0);
        assert_eq!(jobs.num_ready(), 0);
        assert_eq!(jobs.num_running(), 0);
    }

    #[test]
    fn job_counts_update_with_new_jobs() {
        let mut jobs = JobStore::default();
        let job1 = Job::new((), vec![]);
        let job1 = jobs.add_job(job1);
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
        let todo = jobs.get();
        assert!(todo.is_some());
        let todo = todo.unwrap();
        assert_eq!(todo.0, job1);
        assert_eq!(todo.1.ty, "job1");
        jobs.finish_job(todo.unwrap(), FinishType::Success);
        let todo = jobs.get();
        assert!(todo.is_some());
        let todo = todo.unwrap();
        assert_eq!(todo.0, job2);
        assert_eq!(todo.1.ty, "job2");
        assert!(jobs.get().is_none());
    }

    #[test]
    fn jobs_track_their_dependents() {
        let mut jobs = JobStore::default();
        let job1 = jobs.add_job(Job::new("job1", vec![]));
        let job2 = jobs.add_job(Job::new("job2", vec![job1]));
        let todo = jobs.get();
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

        let todo = jobs.get();
        assert!(todo.is_some());
        let todo = todo.unwrap();
        assert_eq!(todo.0, job1);
        assert_eq!(todo.1.ty, "job1");
        assert_eq!(jobs.num_finished(), 0);
        assert_eq!(jobs.num_waiting(), 1);
        assert_eq!(jobs.num(), 2);
        assert_eq!(jobs.num_ready(), 0);
        assert_eq!(jobs.num_running(), 1);

        jobs.finish_job(todo.unwrap(), FinishType::Success);
        assert_eq!(jobs.num_finished(), 1);
        assert_eq!(jobs.num_waiting(), 0);
        assert_eq!(jobs.num(), 2);
        assert_eq!(jobs.num_ready(), 0);
        assert_eq!(jobs.num_running(), 0);
    }

    #[test]
    fn wind_down_stops_handing_out_new_jobs() {
        let mut jobs = JobStore::default();
        jobs.add_job(Job::new("job1", vec![]));
        jobs.add_job(Job::new("job2", vec![]));
        jobs.wind_down();
        assert!(jobs.get().is_none());
    }
}
