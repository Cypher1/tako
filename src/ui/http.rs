use super::{Client, UserInterface};
use crate::cli_options::Options;
use crate::cli_options::{TITLE, VERSION};
use crate::compiler_context::Compiler;
use crate::primitives::Prim;
use crate::tasks::{RequestTask, StatusReport};
use async_trait::async_trait;
use log::{debug, trace};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::time::{Duration, Instant};
use tokio::sync::{broadcast, mpsc};
use tokio::time;
use warp::Filter;

const TICK: Duration = Duration::from_millis(100);

#[derive(Debug)]
pub struct Http {
    clients: HashMap<u64, Client>,
    task_manager_status_receiver: broadcast::Receiver<StatusReport>,
    stats_requester: broadcast::Sender<()>,
    _options: Options,
}

#[derive(Debug)]
enum CompilerRequest {
    RequestTask(RequestTask, u64, mpsc::UnboundedSender<Prim>),
}

async fn run_server(request_sender: mpsc::UnboundedSender<CompilerRequest>) {
    let index = warp::path::end().map(|| "Hello everyone! (try /request/name)".to_string());
    let version = warp::path!("version").map(|| format!("{TITLE}{VERSION}"));

    // GET /request/warp => 200 OK with body "Hello, warp!"
    let request = warp::path!("request" / String).then(move |client_id: String| {
        let request_sender = request_sender.clone();
        async move {
            let (tx, mut rx) = mpsc::unbounded_channel();
            request_sender
                .send(CompilerRequest::RequestTask(
                    RequestTask::EvalLine("1+2".to_string()),
                    to_client_id(&client_id),
                    tx,
                ))
                .expect("TODO");
            let result = rx.recv().await;
            format!("Hello, {client_id}!\n{result:?}")
        }
    });

    warp::serve(index.or(request).or(version))
        .run(([127, 0, 0, 1], 3030))
        .await;
}

fn to_client_id(t: &str) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

#[async_trait]
impl UserInterface for Http {
    async fn launch(compiler: &Compiler, _options: Options) -> std::io::Result<Self> {
        let task_manager_status_receiver = compiler.status_sender.subscribe();
        let stats_requester = compiler.stats_requester.clone();
        Ok(Self {
            clients: HashMap::new(),
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
                    match req {
                        CompilerRequest::RequestTask(request, client_id, tx) => {
                            debug!("{:?}: {:?}", client_id, request);
                            debug!("{:?}", self.clients.entry(client_id));
                            tx.send(Prim::I32(1)).expect("TODO");
                        }
                    }
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
