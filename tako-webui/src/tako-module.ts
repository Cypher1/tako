import { LitElement, html, css } from 'lit';
import { property, customElement } from 'lit/decorators.js';

@customElement('tako-module')
export class TakoModule extends LitElement {
  static styles = css``;

  @property() path: string[] = [];
  @property() source: string = '';
  @property() language: string = 'tako';

  render() {
    return html`
    <link rel="stylesheet" href="prism.css">
    <div class="card">
    <div class="card-title">
    ${this.path} (${this.language})
    </div>
<pre data-line="2,4-5"><code class="line-numbers language-${this.language}">
${this.source}
</code></pre>
<script src="prism.js"></script>
</div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'tako-module': TakoModule;
  }
}
