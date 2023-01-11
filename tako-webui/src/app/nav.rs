use yew::prelude::*;
use yew::{function_component, html, Callback, Html};

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
            <strong>{"Tako"}</strong>
          </a>
          <a class="navbar-item" href="https://github.com/Cypher1/tako/"> {"Getting started"} </a>
          <a class="navbar-item" href="https://github.com/Cypher1/tako/"> {"Docs"} </a>
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
              {"View the project on\u{00a0}"}<strong>{"Github!"}</strong>
              </a>
            </div>
          </div>
        </div>
      </nav>
    }
}
