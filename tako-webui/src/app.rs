use yew::prelude::*;
use yew::{function_component, html, Properties, Callback, Html};

#[derive(PartialEq, Properties, Clone, Eq)]
pub struct ModuleProps {
    pub path: String,
    pub language: String,
    pub source: String,
}

#[function_component(App)]
pub fn app() -> Html {
    html! {
        <>
            <head>
                <link rel="stylesheet" href="./static/prism.css" />
            </head>
            <main>
                <Navbar></Navbar>
                <Interface></Interface>
            </main>
            <Footer></Footer>
            <script src="./static/prism.js"></script>
        </>
    }
}

#[function_component(Footer)]
pub fn footer() -> Html {
    html! {
        <footer class="footer">
        <div class="content has-text-centered">
            {"Made with love by "}<a target="_blank" rel="noopener noreferrer" href="https://github.com/Cypher1">{"Cypher1"}</a>{"."}
        </div>
        </footer>
    }
}

#[function_component(RepoStatus)]
pub fn repo_status() -> Html {
    html! {
        <>
          <a class="navbar-item" href="https://github.com/Cypher1/tako/actions"><img src="https://github.com/Cypher1/tako/workflows/Rust/badge.svg" alt="Build Status"/></a>
          <a class="navbar-item" href="https://github.com/Cypher1/tako/issues"><img src="https://img.shields.io/github/issues/Cypher1/tako.svg" alt="GitHub issues"/></a>
        </>
    }
}

#[function_component(Navbar)]
pub fn navbar() -> Html {
    let is_active = use_state(|| false);
    let onclick = {
        let is_active = is_active.clone();
        Callback::from(move |_| is_active.set(!*is_active))
    };

    html! {
        <nav class="navbar" role="navigation" aria-label="main navigation">
        <div class="navbar-brand">
          <a class="navbar-item" href="/">
            <img class="logo" src="./static/tako.png" alt="Tako octopus logo" />
            {"Tako"}
          </a>
          <RepoStatus></RepoStatus>
          <button {onclick} class={format!("navbar-burger {}", if *is_active { "is-active" } else { "" })} aria-label="menu" aria-expanded="false" data-target="navbarMenu">
            <span aria-hidden="true"></span>
            <span aria-hidden="true"></span>
            <span aria-hidden="true"></span>
        </button>
        </div>
        <div id="navbarMenu" class={format!("navbar-menu {}", if *is_active { "is-active" } else { "" })}>
          <div class="navbar-start">
          </div>
          <div class="navbar-end">
            <div class="navbar-item">
              <a class="navbar-item" href="https://github.com/Cypher1/tako">
              {"View the project on "}<strong>{"Github!"}</strong>
              </a>
            </div>
          </div>
        </div>
      </nav>
    }
}

#[function_component(Interface)]
pub fn interface() -> Html {
let python_code: &str = "
print(\"hi\")
for x in range(1, 100):
    print(x)
";

let clike_code: &str = "
int foo() {
    int i = 3;
    return i;
}
";
let rust_code: &str = "
impl Foo {
  fn foo(mut self) -> Self {
    let i: u32 = 3;
    todo!()
  }
}
";

    html! {
        <>
            <span class="subtitle">{ "INTERFACE GOES HERE" }<i class="heart" /></span>
            <Module path="test.py" language="python" source={python_code}></Module>
            <Module path="example.tk" language="clike" source={clike_code}></Module>
            <Module path="example.rs" language="rust" source={rust_code}></Module>
        </>
    }
}

#[function_component(Module)]
pub fn module(props: &ModuleProps) -> Html {
    html! {
        <>
            <div class="card">
                <div class="card-title">{&props.path}{" ("}{&props.language}{")"}</div>
                <pre data-line="2,4-5"><code class="line-numbers language-${this.language}">
                {&props.source}
                </code></pre>
            </div>
        </>
    }
}


