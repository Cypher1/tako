#[derive(Debug)]
pub struct Options {
    files: Vec<String>,
    interactive: bool,
    wasm: bool,
    show_ast: bool,
    show_full_ast: bool,
    debug: i32,
}

impl Default for Options {
    fn default() -> Options {
        Options {
            files: vec![],
            interactive: false,
            wasm: false,
            show_ast: false,
            show_full_ast: false,
            debug: 0,
        }
    }
}

pub fn parseArgs(args: Vec<String>) -> Options::Err {
    let mut opts = Options::default();
    for f in args {
        if !f.starts_with('-') {
            opts.files.push(f);
        } else {
            match f.as_str() {
                "-i" | "--interactive" => {
                    opts.interactive = true;
                    opts.files.push("/dev/stdin".to_string());
                }
                "-r" | "--run" => opts.interactive = true,
                "-d" => opts.debug += 1,
                "--ast" => opts.show_ast = true,
                "--wasm" => opts.wasm = true,
                "--full-ast" => opts.show_full_ast = true,
                "--version" => {
                    println!("{}{}", TITLE, VERSION);
                    return Ok(opts);
                }
                arg => {
                    if arg != "-h" && arg != "--help" {
                        eprintln!("unexpected flag '{}'", f);
                    }
                    eprintln!("{}{}\n{}", TITLE, VERSION, USAGE);
                    return Ok(opts);
                }
            }
        }
    }
    Ok(opts)
}

pub const TITLE: &str = "tako v";

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub const USAGE: &str = "An experimental programming language for ergonomic software verification.

Usage:
  tako [-i|-r] [-d <level>] [--ast] [--full-ast] <files>...
  tako (-h | --help)
  tako --version

Options:
  -i --interactive    Run as a repl (interactive mode).
  -r --run            Run files in interpreter.
  -d --debug=<level>  Level of debug logging to use [default: 0].
  --wasm              Compile to wasm [default: false].
  --ast               Pretty print an abstract syntax tree of the code.
  --full_ast          Debug print an abstract syntax tree of the code.
  -h --help           Show this screen.
  --version           Show compiler version.
";

