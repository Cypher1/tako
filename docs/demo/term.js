import { Terminal } from 'xterm';
import { WebLinksAddon } from 'xterm-addon-web-links';

const terminal = new Terminal();
const fitAddon = new FitAddon();
terminal.loadAddon(new WebLinksAddon());
terminal.open(containerElement);
