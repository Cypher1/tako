#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TaskState<E: std::error::Error> {
    /// Place holder to record that a job is already running and shouldn't need to be run again
    /// unless invalidated.
    New,
    Invalidated,
    Running,
    Partial,  // Include a handle to the result?
    Complete, // Include a handle to the result?
    Failure(E),
    // TODO(feature): Invalidated, // Has previous run correctly, but the previous result is (somehow) 'known' to be stale.
    // TODO(feature): Cancelled,  // Include why it was cancelled?
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TaskStatus<T, E: std::error::Error> {
    pub index: Option<usize>,
    pub state: TaskState<E>,
    pub results: Vec<T>,
}

impl<T, E: std::error::Error> Default for TaskStatus<T, E> {
    fn default() -> Self {
        Self {
            index: None,
            state: TaskState::New,
            results: Vec::new(),
        }
    }
}

impl<T, E: std::error::Error> TaskStatus<T, E> {
    #[must_use] pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Update<O: Send, E: std::error::Error + Send> {
    NextResult(O),
    FinalResult(O),
    Complete,
    Failed(E),
}
