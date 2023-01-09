import { LitElement, html, css } from 'lit';
import { property, customElement } from 'lit/decorators.js';
import { TakoModule } from './tako-module.js';

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
    html`<tako-module path="test.tk"></tako-module>`,
    html`<tako-module path="example.tk"></tako-module>`,
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
