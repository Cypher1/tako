use super::UserInterface;
use crate::cli_options::Options;
use crate::compiler_context::Compiler;
use crate::tasks::StatusReport;
use async_trait::async_trait;
use log::trace;
use std::time::{Duration, Instant};
use tokio::sync::broadcast;
use tokio::time;
use warp::Filter;

const TICK: Duration = Duration::from_millis(100);

#[derive(Debug)]
pub struct Http {
    task_manager_status_receiver: broadcast::Receiver<StatusReport>,
    stats_requester: broadcast::Sender<()>,
    _options: Options,
}

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
    async fn launch(compiler: &Compiler, _options: Options) -> std::io::Result<Self> {
        let task_manager_status_receiver = compiler.status_sender.subscribe();
        let stats_requester = compiler.stats_requester.clone();
        Ok(Self {
            task_manager_status_receiver,
            stats_requester,
            _options,
        })
    }

    async fn run_loop(mut self) -> std::io::Result<()> {
        let _start_time = Instant::now();
        let mut stats_ticker = time::interval(TICK);

        tokio::spawn(async move {
            run_server().await;
        });

        loop {
            tokio::select! {
                Ok(StatusReport { kind, stats, errors }) = self.task_manager_status_receiver.recv() => {
                    trace!("TaskManager stats: {kind:?} => {stats}\nerrors: {errors:#?}");
                    // http.manager_status.insert(kind, stats);
                },
                _ = stats_ticker.tick() => {
                    self.stats_requester.send(()).expect("TODO");
                }
                else => break,
            }
            // http.render()?;
        }
        Ok(())
    }
}
