use crate::cli_options::Options;
use crate::compiler_context::Compiler;
use crate::error::Error;
use crate::primitives::Prim;
use crate::tasks::{RequestTask, StatusReport, TaskKind, TaskStats};
use async_trait::async_trait;
use futures::{future::FutureExt, StreamExt};
use log::{debug, trace};
use std::collections::{BTreeSet, HashMap};
use std::path::PathBuf;
use tokio::{
    self,
    sync::mpsc,
};

#[derive(Debug)]
pub struct Client {
    pub manager_status: HashMap<TaskKind, TaskStats>,
    pub compiler: Compiler,
    pub history: Vec<String>, // TODO: Mark Input v output.
    pub errors_for_file: HashMap<Option<PathBuf>, BTreeSet<Error>>,
    pub options: Options,
    pub result_receiver: mpsc::UnboundedReceiver<Prim>,
    pub result_sender: mpsc::UnboundedSender<Prim>,
}

impl Client {
    fn new(compiler: Compiler, options: Options) -> Self {
        let (result_sender, result_receiver) = mpsc::unbounded_channel();
        Self {
            manager_status: HashMap::default(),
            history: Vec::default(),
            errors_for_file: HashMap::default(),
            compiler,
            options,
            result_receiver,
            result_sender,
        }
    }
}
