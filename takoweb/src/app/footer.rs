use takolib::VERSION;
use yew::{function_component, html, Html};

#[function_component(Footer)]
pub fn footer() -> Html {
    html! {
        <footer class="footer">
        <div class="content has-text-centered">
        <p>{"Made with love by "}<a target="_blank" rel="noopener noreferrer" href="https://github.com/Cypher1">{"Cypher1"}</a>{"."}</p>
        <p>{"Version "}<span>{VERSION}</span></p>
        </div>
        </footer>
    }
}
