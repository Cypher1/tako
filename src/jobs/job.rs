use crate::typed_index::TypedIndex;

#[derive(Debug, Clone, Copy)]
pub enum FinishType {
    Success, // Include a handle to the result?
    Cancelled,
    Failed,
}
use FinishType::*;

#[derive(Debug, Clone, Copy)]
pub enum JobState {
    Waiting,
    Running, // TODO: Include a handle to the process?
    Finished(FinishType),
}
use JobState::*;

#[derive(Debug)]
pub struct Job<JobType> {
    ty: JobType,
    state: JobState,
    dependents: Vec<JobId>,
    dependencies: Vec<JobId>,
}
pub type JobId = TypedIndex<Job>;

impl<JobType> Job<JobType> {
    pub fn new(ty: JobType, dependencies: Vec<JobId>) -> Self {
        Self {
            ty,
            state: JobState::Waiting,
            dependents: Vec::new(),
            dependencies,
        }
    }

    pub fn add_dependent(&mut self, id: JobId) {
        self.dependents.push(id);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn todo_job_test() {
        todo!()
    }
}