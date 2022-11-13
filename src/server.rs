use warp::Filter;

#[tokio::main]
async fn main() {
    let index = warp::path::end()
        .map(|| format!("Hello everyone!"));

    // GET /hello/warp => 200 OK with body "Hello, warp!"
    let hello = warp::path!("hello" / String)
        .map(|name| format!("Hello, {}!", name));

    warp::serve(
        index
        .or(hello)
    )
        .run(([127, 0, 0, 1], 3030))
        .await;
}
