use async_trait::async_trait;
use std::hash::Hasher;
use tokio::sync::mpsc;

use super::status::*;
use super::TaskKind;
use crate::error::{Error, TError};

type TaskHash = u64;
type TaskId = TaskHash;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TaskMeta {
    // Stores track meta data, like a cached task hash.
    kind: TaskKind, // Where to look for the task
    task_hash: TaskHash,   // the cached hash value
}

pub type TaskReceiverFor<T> = mpsc::UnboundedReceiver<T>;
pub type TaskSenderFor<T> = mpsc::UnboundedSender<<T as Task>::Output>;
pub type UpdateSender<T, O> = mpsc::UnboundedSender<(TaskId, T, Update<O, Error>)>;

#[async_trait]
pub trait Task: std::fmt::Debug + Clone + std::hash::Hash + Eq + Sized + Send {
    // TODO: Separate the code that performs the task
    // from the part that generates new tasks.
    // TODO: Only perform 'new' tasks.

    type Output: std::fmt::Debug + Clone + Send;
    const TASK_KIND: TaskKind;
    const RESULT_IS_CACHABLE: bool = true;

    fn cache_hash(&self, _hash: TaskHash) {
        // default to dropping it... but try not to?
        // Hashing will visit all owned memory, so
        // for small tasks rehashing may be better than not...
    }
    fn cached_hash(&self) -> Option<TaskHash> {
        None
    }

    fn create_meta(&self) -> TaskMeta {
        let task_hash = if let Some(hash) = self.cached_hash() {
            hash
        } else {
            let mut hasher = fxhash::FxHasher::default();
            self.hash(&mut hasher);
            let task_hash = hasher.finish();
            self.cache_hash(task_hash);
            task_hash
        };
        TaskMeta {
            kind: Self::TASK_KIND,
            task_hash,
        }
    }

    fn has_file_path(&self) -> Option<&str> {
        None
    }
    fn has_source(&self) -> Option<&str> {
        None
    }
    fn has_module(&self) -> Option<&()> {
        None
    }
    // TODO: More...

    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>);

    fn decorate_error<E: Into<TError>>(&self, error: E) -> Error {
        Error::new(error.into(), self.has_file_path(), self.has_source(), seld.has_module())
    }
}
