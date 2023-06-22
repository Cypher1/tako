use async_trait::async_trait;
use std::hash::Hasher;
use std::path::PathBuf;
use tokio::sync::mpsc;

use super::status::Update;
use super::TaskKind;
use crate::error::{Error, TError};

type TaskHash = u64;
pub type TaskId = TaskHash;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TaskMeta {
    // Stores track meta data, like a cached task hash.
    kind: TaskKind,      // Where to look for the task
    task_hash: TaskHash, // the cached hash value
}

pub type TaskReceiverFor<T> = mpsc::UnboundedReceiver<T>;
pub type TaskSenderFor<T> = mpsc::UnboundedSender<T>;
pub type ResultReceiverFor<T> = mpsc::UnboundedReceiver<<T as Task>::Output>;
pub type ResultSenderFor<T> = mpsc::UnboundedSender<<T as Task>::Output>;
pub type UpdateSenderFor<T> = mpsc::UnboundedSender<(T, Update<<T as Task>::Output, Error>)>;

#[async_trait]
pub trait Task: std::fmt::Debug + Clone + std::hash::Hash + Eq + Sized + Send {
    // TODO(clarity): Separate the code that performs the task
    // from the part that generates new tasks.

    type Output: std::fmt::Debug + Clone + Send;
    const TASK_KIND: TaskKind;
    const RESULT_IS_CACHABLE: bool = true; // i.e. should not be re-run if cached.

    fn cached_hash(&self) -> Option<TaskHash> {
        None
    }
    fn cache_hash(&self, _hash: TaskHash) {
        // default to dropping it... but try not to?
        // Hashing will visit all owned memory, so
        // for small tasks rehashing may be better than not...
    }

    fn compute_hash(&self) -> TaskHash {
        let mut hasher = fxhash::FxHasher::default();
        self.hash(&mut hasher);
        let task_hash = hasher.finish();
        self.cache_hash(task_hash);
        task_hash
    }

    fn get_hash(&self) -> TaskHash {
        if let Some(hash) = self.cached_hash() {
            hash
        } else {
            self.compute_hash()
        }
    }

    fn create_meta(&self) -> TaskMeta {
        TaskMeta {
            kind: Self::TASK_KIND,
            task_hash: self.get_hash(),
        }
    }

    fn has_file_path(&self) -> Option<&PathBuf> {
        None
    }
    fn has_source(&self) -> Option<&str> {
        None
    }
    fn has_module(&self) -> Option<&()> {
        None
    }
    // TODO(debugging): More...

    fn invalidate(&self) -> bool {
        false
    }

    async fn perform(self, result_sender: UpdateSenderFor<Self>);

    fn decorate_error<E: Into<TError>>(&self, error: E) -> Error {
        Error::new(
            error.into(),
            self.has_file_path(),
            self.has_source(),
            self.has_module(),
        )
    }
}
