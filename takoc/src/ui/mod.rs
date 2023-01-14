mod http;
mod tui;

pub use http::Http;
pub use tui::Tui;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UiMode {
    Tui,
    Http,
}
