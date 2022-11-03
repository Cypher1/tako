use crate::free_standing::typed_index::TypedIndex;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum FinishType {
    Success, // Include a handle to the result?
    Cancelled,
    Failed,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum JobState {
    Waiting,
    Ready,
    Running, // TODO: Include a handle to the process?
    Finished(FinishType),
}

#[derive(Debug)]
pub struct Job<JobType> {
    pub kind: JobType,
    pub state: JobState,
    pub dependents: Vec<JobId<JobType>>,
    pub dependencies: Vec<JobId<JobType>>,
}
pub type JobId<T> = TypedIndex<Job<T>>;

impl<JobType> Job<JobType> {
    pub fn new(kind: JobType, dependencies: Vec<JobId<JobType>>) -> Self {
        Self {
            kind,
            state: JobState::Waiting,
            dependents: Vec::new(),
            dependencies,
        }
    }

    pub fn add_dependent(&mut self, id: JobId<JobType>) {
        self.dependents.push(id);
    }
}
