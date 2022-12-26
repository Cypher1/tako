use super::UserInterface;
use crate::cli_options::Options;

use crate::primitives::Prim;
use crate::tasks::{RequestTask, StatusReport};
use async_trait::async_trait;
use log::trace;
use std::time::{Duration, Instant};
use tokio::time;
use tokio::{
    self,
    sync::{broadcast, mpsc},
};
use warp::Filter;

const TICK: Duration = Duration::from_millis(100);

#[derive(Debug, Default)]
pub struct Http {}

async fn run_server() {
    let index = warp::path::end().map(|| "Hello everyone!".to_string());

    // GET /hello/warp => 200 OK with body "Hello, warp!"
    let hello = warp::path!("hello" / String).map(|name| format!("Hello, {name}!"));

    warp::serve(index.or(hello))
        .run(([127, 0, 0, 1], 3030))
        .await;
}

#[async_trait]
impl UserInterface for Http {
    async fn launch(
        mut task_manager_status_receiver: broadcast::Receiver<StatusReport>,
        // User control of the compiler
        _request_sender: mpsc::UnboundedSender<(RequestTask, mpsc::UnboundedSender<Prim>)>,
        stats_requester: broadcast::Sender<()>,
        _options: Options,
    ) -> std::io::Result<()> {
        let _start_time = Instant::now();
        let _http = Self::default();
        let mut stats_ticker = time::interval(TICK);

        tokio::spawn(async move {
            run_server().await;
        });

        loop {
            tokio::select! {
                Ok(StatusReport { kind, stats, errors }) = task_manager_status_receiver.recv() => {
                    trace!("TaskManager stats: {kind:?} => {stats}\nerrors: {errors:#?}");
                    // http.manager_status.insert(kind, stats);
                },
                _ = stats_ticker.tick() => {
                    stats_requester.send(()).expect("TODO");
                }
                else => break,
            }
            // http.render()?;
        }
        Ok(())
    }
}
