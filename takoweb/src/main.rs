mod app;
mod client;

use app::App;

fn main() {
    yew::Renderer::<App>::new().render();
}
