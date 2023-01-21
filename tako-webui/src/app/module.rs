use yew::{function_component, html, Html, Properties};

#[derive(PartialEq, Properties, Clone, Eq)]
pub struct ModuleProps {
    pub path: String,
    pub language: String,
    pub source: String,
}

#[function_component(Module)]
pub fn module(props: &ModuleProps) -> Html {
    let last_result = use_state(|| false);
    let onclick = {
        let last_result = last_result.clone();
        Callback::from(move |_| last_result.set(!*last_result))
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
                        <button {onclick} aria-label="run" aria-expanded="false">{*last_result}</button>
                        <pre><code class={format!("line-numbers language-{}", props.language)}>
                        {&props.source}
                        </code></pre>
                    </div>
                </div>
            </div>
        </>
    }
}
