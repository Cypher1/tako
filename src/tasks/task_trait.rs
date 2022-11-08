use async_trait::async_trait;
use std::hash::Hasher;
use tokio::sync::mpsc;

use super::status::*;
use super::TaskKind;
use crate::error::{Error, TError};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TaskMeta {
    // Stores track meta data, like a cached task hash.
    kind: TaskKind, // Where to look for the task
    task_id: u64,   // the cached hash value
}

pub type TaskReceiverFor<T> = mpsc::UnboundedReceiver<T>;
pub type TaskSenderFor<T> = mpsc::UnboundedSender<<T as Task>::Output>;
pub type UpdateSender<T, O> = mpsc::UnboundedSender<(T, Update<O, Error>)>;

#[async_trait]
pub trait Task: std::fmt::Debug + Clone + std::hash::Hash + Eq + Sized + Send {
    // TODO: Separate the code that performs the task
    // from the part that generates new tasks.
    // TODO: Only perform 'new' tasks.

    type Output: std::fmt::Debug + Clone + Send;
    const TASK_KIND: TaskKind;
    const RESULT_IS_CACHABLE: bool = true;

    fn create_meta(&self) -> TaskMeta {
        let mut hasher = fxhash::FxHasher::default();
        self.hash(&mut hasher);
        let task_id = hasher.finish();
        TaskMeta {
            kind: Self::TASK_KIND,
            task_id,
        }
    }
    fn has_file_path(&self) -> Option<&str> {
        None
    }
    // fn has_module(&self) -> Option<&Module> {
    //  None
    //}
    // fn has_tokens(&self) -> Option<&Vec<Token>> {
    //  None
    //}
    // fn has_ast(&self) -> Option<&Ast> {
    //  None
    //}
    // TODO: More...

    async fn perform(self, result_sender: UpdateSender<Self, Self::Output>);

    fn decorate_error<E: Into<TError>>(&self, error: E) -> Error {
        Error::new(error.into(), self.has_file_path(), None, None)
    }
}
