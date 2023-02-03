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
    let source = use_state(|| props.source.clone());
    let eval_result = use_state(|| "...".to_string());
    let change_on_click = {
        let source = source.clone();
        Callback::from(move |_| {
            source.set(format!("{}*{}", *source, source.len()));
        })
    };

    let run_on_click = {
        let source = source.clone();
        let eval_result = eval_result.clone();
        Callback::from(move |_| {
            let result = tokio::runtime::Builder::new_current_thread()
                .build()
                .unwrap()
                .block_on(
                    interpret(&source)
                );
            eval_result.set(result);
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
                        <button onclick={change_on_click} aria-label="change" aria-expanded="false">{ "+1" }</button>
                        <button onclick={run_on_click} aria-label="run" aria-expanded="false">{ &*eval_result }</button>
                        //<pre><code class={format!("line-numbers language-{}", props.language)}>
                        {&*source}
                        //</code></pre>
                    </div>
                </div>
            </div>
        </>
    }
}
