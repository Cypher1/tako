use async_trait::async_trait;
use log::trace;
use warp::Filter;

use super::UserInterface;
use crate::ast::Ast;
use crate::tasks::{RequestTask, StatusReport};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use tokio::time;
use tokio::{
    self,
    sync::{broadcast, mpsc},
};

const TICK: Duration = Duration::from_millis(100);

#[derive(Debug, Default)]
pub struct Http {}

#[allow(unused)]
#[tokio::main]
async fn main() {
    let index = warp::path::end().map(|| "Hello everyone!".to_string());

    // GET /hello/warp => 200 OK with body "Hello, warp!"
    let hello = warp::path!("hello" / String).map(|name| format!("Hello, {}!", name));

    warp::serve(index.or(hello))
        .run(([127, 0, 0, 1], 3030))
        .await;
}

#[async_trait]
impl UserInterface for Http {
    async fn launch(
        mut task_manager_status_receiver: mpsc::UnboundedReceiver<StatusReport>,
        // User control of the compiler
        _request_sender: Option<mpsc::UnboundedSender<RequestTask>>,
        _response_getter: Option<mpsc::UnboundedReceiver<Ast>>,
        stats_requester: Arc<Mutex<broadcast::Sender<()>>>,
    ) -> std::io::Result<()> {
        let _start_time = Instant::now();
        let _http = Self::default();
        let mut stats_ticker = time::interval(TICK);

        loop {
            tokio::select! {
                Some(StatusReport { kind, stats }) = task_manager_status_receiver.recv() => {
                    trace!("TaskManager stats: {kind:?} => {stats}");
                    // http.manager_status.insert(kind, stats);
                },
                _ = stats_ticker.tick() => {
                    let stats_requester = stats_requester.lock().expect("stats requester lock");
                    stats_requester.send(()).expect("TODO");
                }
                else => break,
            }
            // http.render()?;
        }
        Ok(())
    }
}
