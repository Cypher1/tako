import { LitElement, html, css } from 'lit';
import { property, customElement } from 'lit/decorators.js';

@customElement('tako-module')
export class TakoModule extends LitElement {
  static styles = css``;

  @property({ type: String }) path = [];

  render() {
    return html`
    <head>
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css">
    <div class="card">
      <header class="card-header">
        <p class="card-header-title">
          Component
        </p>
        <button class="card-header-icon" aria-label="more options">
          <span class="icon">
            <i class="fas fa-angle-down" aria-hidden="true"></i>
          </span>
        </button>
      </header>
      <div class="card-content">
        <div class="content">
          <syntax-highlight language="javascript">
            for (const item of items) {
              item.x += item.y;
            }
          </syntax-highlight>
        </div>
      </div>
    </div>
`;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'tako-module': TakoModule;
  }
}
