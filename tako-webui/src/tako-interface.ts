import { LitElement, html, css } from 'lit';
import { property, customElement } from 'lit/decorators.js';
import { TakoModule } from './tako-module';
import './tako-module';

const pythonCode = `
print("hi")
for x in range(1, 100):
    print(x)
`;

const clikeCode = `
int foo() {
    int i = 3;
    return i;
}
`;
const rustCode = `
impl Foo {
  fn foo(mut self) -> Self {
    let i: u32 = 3;
    todo!()
  }
}
`;

@customElement('tako-interface')
export class TakoInterface extends LitElement {
  static styles = css`
    .banner {
      max-width: calc(10vw - 20px);
      align-items: center;
      text-align: center;
    }
  `;

  @property({ type: Array<TakoModule> }) modules = [
    html`<tako-module path="test.py" language="python" source="${pythonCode}"></tako-module>`,
    html`<tako-module path="example.tk" language="clike" source="${clikeCode}"></tako-module>`,
    html`<tako-module path="example.rs" language="rust" source="${rustCode}"></tako-module>`,
  ];

  render() {
    return html`
      <section class="banner">
        <h1 id="coming-soon">Coming soon…</h1>
      </section>
      <section class="container">
        <div>${this.modules}</div>
      </section>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'tako-interface': TakoInterface;
  }
}
