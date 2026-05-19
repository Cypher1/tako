use qbice::{Executor, TrackedEngine};

use super::*;

#[derive(Clone, Copy, Debug)]
struct LoadExecutor;

impl<C: qbice::Config> Executor<Load, C> for LoadExecutor {
    async fn execute(&self, query: &Load, engine: &TrackedEngine<C>) -> Result<String, TError> {
        // TODO(correctness): Look at  notify::recommended_watcher
        // See `tako/src/main.rs` `WatchFileTask`
        match &query.file {
            FileRef::File(path) => {
                Ok(std::fs::read_to_string(&path)?)
            }
            FileRef::InMemory(_path, contents) => Ok(contents.clone()),
            FileRef::Dependency { name, version, internal_path } => {
                // Download the dependency if it's not already down
                // Load the file from there.
                todo!("{name}@{version} / {internal_path:?}");
            }
        }
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::ExternalInput
    }
}
