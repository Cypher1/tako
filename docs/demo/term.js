import { Terminal } from 'xterm';
import { WebLinksAddon } from 'xterm-addon-web-links';

const terminal = new Terminal();
const fitAddon = new FitAddon();
terminal.loadAddon(new WebLinksAddon());
terminal.open(document.getElementById('terminal'));
terminal.write('Hello from \x1B[1;3;31mxterm.js\x1B[0m $ ')
