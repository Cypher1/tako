use crate::cli_options::{Options, TITLE, VERSION};
use async_trait::async_trait;
use log::debug;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::time::{Duration, Instant};
use takolib::primitives::Prim;
use takolib::tasks::RequestTask;
use takolib::ui::OptionsTrait;
use takolib::ui::{Client, UserInterface};
use tokio::sync::{mpsc, oneshot};
use tokio::time;
use warp::hyper::Response;
use warp::Filter;

const TICK: Duration = Duration::from_millis(100);

#[derive(Debug)]
pub struct Http {
    client_launch_request_sender:
        mpsc::UnboundedSender<(oneshot::Sender<Client>, Box<dyn OptionsTrait>)>,
    clients: HashMap<u64, Client>,
    // task_manager_status_receiver: broadcast::Receiver<StatusReport>,
    // stats_requester: broadcast::Sender<()>,
    options: Options,
}

#[derive(Debug)]
enum CompilerRequest {
    RequestTask(RequestTask, u64, mpsc::UnboundedSender<Prim>),
}

async fn run_server(request_sender: mpsc::UnboundedSender<CompilerRequest>) {
    let version = warp::path!("version").map(|| format!("{TITLE}{VERSION}"));

    // GET / => 200 OK with body "Hello, warp!"
    let request = warp::path::end()
        .and(warp::query::<HashMap<String, String>>())
        .then(move |map: HashMap<String, String>| {
            eprintln!("request: {map:?}");
            let line = map
                .get("line")
                .cloned()
                .unwrap_or_else(|| "\"Try ./?line=\"1+2\"\"".to_string());
            let client_id = to_client_id("TODO");
            let request_sender = request_sender.clone();
            async move {
                let (tx, mut rx) = mpsc::unbounded_channel();
                request_sender
                    .send(CompilerRequest::RequestTask(
                        RequestTask::EvalLine(line.to_string()),
                        client_id,
                        tx,
                    ))
                    .expect("Error...");
                let result = rx.recv().await;

                Response::builder().body(format!(
                    "<!DOCTYPE html>
                    <html>
                        <head>
                            <title>Tako Web</title>
                        </head>
                        <body>
                            <h1>Tako Web</h1>
                            <p>{line}</p>
                            <p>{result:?}</p>
                            <form action=\"/\">
                                <input type=\"text\" name=\"line\" value=\"{line}\">
                                <input type=\"submit\" value=\"Submit\">
                            </form>
                        </body>
                    </html>",
                    line = html_escape::encode_text(&line)
                ))
            }
        });

    let port = 3030;
    let ip = [127, 0, 0, 1];

    eprintln!(
        "Launched at http://{ip}:{port}",
        ip = ip.map(|n| n.to_string()).join(".")
    );
    warp::serve(request.or(version)).run((ip, port)).await;
}

fn to_client_id(t: &str) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

#[async_trait]
impl UserInterface<Options> for Http {
    async fn launch(
        client_launch_request_sender: mpsc::UnboundedSender<(
            oneshot::Sender<Client>,
            Box<dyn OptionsTrait>,
        )>,
        options: Options,
    ) -> std::io::Result<Self> {
        // let task_manager_status_receiver = compiler.status_sender.subscribe();
        // let stats_requester = compiler.stats_requester.clone();
        Ok(Self {
            client_launch_request_sender,
            clients: HashMap::new(),
            options,
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
                // Ok(StatusReport { kind, stats, errors }) = self.task_manager_status_receiver.recv() => {
                    // trace!("TaskManager stats: {kind:?} => {stats}\nerrors: {errors:#?}");
                    // http.manager_status.insert(kind, stats);
                // },
                Some(req) = rx.recv() => {
                    debug!("Http request to compiler: {:?}", &req);
                    match req {
                        CompilerRequest::RequestTask(request, client_id, tx) => {
                            debug!("{:?}: {:?}", client_id, request);
                            if let std::collections::hash_map::Entry::Vacant(e) = self.clients.entry(client_id) {
                                let client = Self::get_client(&mut self.client_launch_request_sender, Box::new(self.options.clone())).await;
                                e.insert(client);
                            }
                            let client = self.clients.get_mut(&client_id).expect("Just created this client");
                            debug!("{:?}", client);
                            client.send_command(request);
                            if let Err(e) = tx.send(
                                client.result_receiver.recv().await.expect("Should get a result")
                            ) {
                                debug!("Client connection terminated: {e:?}");
                            }
                            // TODO(correctness): Send back a page with a websocket it in that
                            // receives updates.
                        }
                    }
                },
                _ = stats_ticker.tick() => {
                    // TODO: self.stats_requester.send(()).expect("Server task closed");
                }
                else => break,
            }
            // http.render()?;
        }
        Ok(())
    }
}
