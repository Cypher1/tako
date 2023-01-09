import { LitElement, html, css } from 'lit';
import { property, customElement } from 'lit/decorators.js';

@customElement('tako-module')
export class TakoModule extends LitElement {
  static styles = css``;

  @property({ type: String }) path = [];

  render() {
    return html`
      <link rel="stylesheet" href="prism.css">

<pre><code class="line-numbers language-python">
print("hi")
for x in range(1, 100):
    print(x)
</code></pre>
<pre><code class="line-numbers language-clike">
int foo() {
    int i = 3;
    return i;
}
</code></pre>
<pre data-line="2,4-5"><code class="line-numbers language-rust">
impl Foo {
  fn foo(mut self) -> Self {
    let i: u32 = 3;
    todo!()
  }
}
</code></pre>
<script src="prism.js"></script>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'tako-module': TakoModule;
  }
}
