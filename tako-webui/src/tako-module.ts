import { LitElement, html, css } from 'lit';
import { property, customElement } from 'lit/decorators.js';

@customElement('tako-module')
export class TakoModule extends LitElement {
  static styles = css``;

  @property({ type: String }) path = [];

  render() {
    return html` <div>${this.path}</div> `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'tako-module': TakoModule;
  }
}
