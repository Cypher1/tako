use crate::client::interpret;
use yew::prelude::*;
use yew::{function_component, html, Html, Properties};
use web_sys::HtmlTextAreaElement;

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

    let on_change = {
        let source = source.clone();
        Callback::from(move |e: Event| {
            let input_el = e.target_dyn_into::<HtmlTextAreaElement>();
            if let Some(input_el) = input_el {
                let val = input_el.value();
                source.set(val);
            } else {
                // TODO: ERR
                source.set("TODO: ERR".to_string());
            }
        })
    };

    let run_on_click = {
        let source = source.clone();
        let eval_result = eval_result.clone();
        Callback::from(move |_| {
            let result = tokio::runtime::Builder::new_current_thread()
                .build()
                .unwrap()
                .block_on(interpret(&source));
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
                        <h3>
                            <button onclick={&run_on_click} aria-label="run" aria-expanded="false">{"Run"}</button>
                            <span onclick={&run_on_click} aria-label="run" aria-expanded="false">{ &*eval_result }</span>
                        </h3>
                    </div>
                    <div class="content">
                        <textarea onchange={on_change} value={(*source).clone()}/>
                        //<pre><code class={format!("line-numbers language-{}", props.language)}>
                        // {&*source}
                        //</code></pre>
                    </div>
                </div>
            </div>
        </>
    }
}
