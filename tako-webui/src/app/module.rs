use crate::client::interpret;
use yew::prelude::*;
use yew::{function_component, html, Html, Properties};

#[derive(PartialEq, Properties, Clone, Eq)]
pub struct ModuleProps {
    pub path: String,
    pub language: String,
    pub source: String,
}

#[function_component(Module)]
pub fn module(props: &ModuleProps) -> Html {
    let last_result = use_state(|| "...".to_string());
    let source = use_state(|| props.source.clone());
    let onclick = {
        let last_result = last_result.clone();
        let source = source.clone();
        Callback::from(move |_| {
            let last_result = last_result.clone();
            let source = source.clone();
            wasm_bindgen_futures::spawn_local(async move {
                let result = interpret(&*source).await;
                last_result.set(result)
            });
        })
    };

    html! {
        <>
            <div class="card">
                <div class="card-header">
                    <div class="card-header-title">{&props.path}{" ("}{&props.language}{")"}</div>
                    <button class="card-header-icon" aria-label="more options">
                        <span class="icon">
                            <i class="fas fa-angle-down" aria-hidden="true"></i>
                        </span>
                    </button>
                </div>
                <div class="card-content">
                    <div class="content">
                        <button {onclick} aria-label="run" aria-expanded="false">{&*last_result}</button>
                        <pre><code class={format!("line-numbers language-{}", props.language)}>
                        {&*source}
                        </code></pre>
                    </div>
                </div>
            </div>
        </>
    }
}
