import { LitElement, html, css } from 'lit';
import { property, customElement } from 'lit/decorators.js';
import './tako-interface';

const takoLogo = new URL('./tako.png', import.meta.url).href;

@customElement("app-root")
export class TakoWebui extends LitElement {
  @property({ type: String }) header: string = 'Tako';

  @property({ type: String }) tagline: string =
    'An experiment in software verification';

  @property({ type: URL }) logo: string = takoLogo; // TODO(correctness): should be URL...

  @property({ type: Boolean }) navbarActive: boolean = false;

  static styles = css`
    :host {
      min-height: 100vh;
      display: flex;
      flex-direction: column;
      align-items: left;
      justify-content: flex-start;
      font-size: calc(10px + 1vmin);
      font-family: sans-serif;
      color: #1a2b42;
      margin: 0 auto;
      text-align: left;
      background-color: #ededed; /*var(--tako-webui-background-color);*/
    }

    main {
      flex-grow: 1;
    }
    .logo {
      animation: app-logo-spin infinite 20s linear;
      text-align: center;
    }

    @keyframes app-logo-spin {
      from {
        transform: rotate(0deg);
      }
      to {
        transform: rotate(360deg);
      }
    }
  `;

  render() {
    return html`
      <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css">
      <nav class="navbar" role="navigation" aria-label="main navigation">
        <div class="navbar-brand">
          <a class="navbar-item" href="/">
          <img src="${this.logo}" alt="Tako octopus logo">
            ${this.header}
          </a>
          <a class="navbar-item" href="https://github.com/Cypher1/tako/actions"><img src="https://github.com/Cypher1/tako/workflows/Rust/badge.svg" alt="Build Status"></a>
          <a class="navbar-item" href="https://github.com/Cypher1/tako/issues"><img src="https://img.shields.io/github/issues/Cypher1/tako.svg" alt="GitHub issues"></a>

        <button class="navbar-burger ${
          this.navbarActive ? 'is-active' : ''
        }" aria-label="menu" aria-expanded="false" data-target="navbarMenu" @click="${() => {
      this.navbarActive = !this.navbarActive;
    }}">
          <span aria-hidden="true"></span>
          <span aria-hidden="true"></span>
          <span aria-hidden="true"></span>
        </button>
        </div>
        <div id="navbarMenu" class="navbar-menu ${
          this.navbarActive ? 'is-active' : ''
        }">
          <div class="navbar-start">
          </div>
          <div class="navbar-end">
            <div class="navbar-item">
              <a class="navbar-item" href="https://github.com/Cypher1/tako">
                View the project on&nbsp;<strong> Github!</strong>
              </a>
            </div>
          </div>
        </div>
      </div>
      </nav>
      </h2>
      <main>
        <tako-interface>
        </tako-interface>
      </main>

      <p class="app-footer">
      Made with love by
      <a
      target="_blank"
      rel="noopener noreferrer"
      href="https://github.com/Cypher1"
        >Cypher1</a
      >.
        </p>
      `;
  }
}
