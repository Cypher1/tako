use crate::compiler_context::Compiler;
use crate::cli_options::Options;
use async_trait::async_trait;

mod client;
mod http;
mod tui;

pub use client::Client;
pub use http::Http;
pub use tui::Tui;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UserAction {
    Something,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UiMode {
    Tui,
    Http,
}

#[async_trait]
pub trait UserInterface {
    async fn launch(
        compiler: &Compiler,
        options: Options,
    ) -> std::io::Result<Self>
    where
        Self: Sized;

    async fn run_loop(self) -> std::io::Result<()>;
}
