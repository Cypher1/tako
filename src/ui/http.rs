use super::{UserInterface, Client};
use crate::cli_options::Options;
use crate::compiler_context::Compiler;
use crate::tasks::StatusReport;
use async_trait::async_trait;
use log::{debug, trace};
use std::time::{Duration, Instant};
use tokio::sync::{broadcast, mpsc};
use tokio::time;
use warp::Filter;

const TICK: Duration = Duration::from_millis(100);

#[derive(Debug)]
pub struct Http {
    clients: Vec<Client>,
    task_manager_status_receiver: broadcast::Receiver<StatusReport>,
    stats_requester: broadcast::Sender<()>,
    _options: Options,
}

#[derive(Debug)]
enum CompilerRequest {
    GetVersion,
}

async fn run_server(tx: mpsc::UnboundedSender<CompilerRequest>) {
    let index = warp::path::end().map(|| "Hello everyone! (try /hello/name)".to_string());

    // GET /hello/warp => 200 OK with body "Hello, warp!"
    let hello = warp::path!("hello" / String).map(|name| format!("Hello, {name}!"));
    let version = warp::path!("version").map(move || {
        tx.send(CompilerRequest::GetVersion).expect("Sending...");
        use crate::cli_options::{TITLE, VERSION};
        format!("{TITLE}{VERSION}")
    });

    warp::serve(index.or(hello).or(version))
        .run(([127, 0, 0, 1], 3030))
        .await;
}

#[async_trait]
impl UserInterface for Http {
    async fn launch(compiler: &Compiler, _options: Options) -> std::io::Result<Self> {
        let task_manager_status_receiver = compiler.status_sender.subscribe();
        let stats_requester = compiler.stats_requester.clone();
        Ok(Self {
            clients: Vec::new(),
            task_manager_status_receiver,
            stats_requester,
            _options,
        })
    }

    async fn run_loop(mut self) -> std::io::Result<()> {
        let _start_time = Instant::now();
        let mut stats_ticker = time::interval(TICK);

        let (tx, mut rx) = mpsc::unbounded_channel();

        tokio::spawn(async move {
            run_server(tx).await;
        });

        loop {
            tokio::select! {
                Ok(StatusReport { kind, stats, errors }) = self.task_manager_status_receiver.recv() => {
                    trace!("TaskManager stats: {kind:?} => {stats}\nerrors: {errors:#?}");
                    // http.manager_status.insert(kind, stats);
                },
                Some(req) = rx.recv() => {
                    debug!("Http request to compiler: {:?}", &req);
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
