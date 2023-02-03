mod footer;
mod module;
mod nav;

use footer::Footer;
use module::Module;
use nav::Navbar;
use yew::{function_component, html, Html};

#[function_component(App)]
pub fn app() -> Html {
    html! {
        <>
            <head>
                <script src="https://kit.fontawesome.com/c85306ca98.js" crossorigin="anonymous"></script>
                <link rel="shortcut icon" type="image/png" href="./static/favicon.ico"/>
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

    let tako_code: &str = "12 + 3";

    html! {
        <div class="container">
            <div class="columns is-multiline is-centered">
                <div class="column">
                    <Module path="test.py" language="python" source={python_code}></Module>
                </div>
                <div class="column">
                    <Module path="example.tk" language="clike" source={clike_code}></Module>
                </div>
                <div class="column">
                    <Module path="example.rs" language="rust" source={rust_code}></Module>
                </div>
                <div class="column">
                    <Module path="example.tk" language="clike" source={tako_code}></Module>
                </div>
            </div>
        </div>
    }
}
