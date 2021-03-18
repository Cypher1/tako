#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Command {
    Build,
    Interpret,
    Repl,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Options {
    pub files: Vec<String>,
    pub cmd: Command,
    pub show_ast: bool,
    pub show_table: bool,
    pub show_full_ast: bool,
    pub debug_level: i32,
    pub interpreter_args: Vec<String>,
}

impl Default for Options {
    fn default() -> Options {
        Options {
            files: vec![],
            cmd: Command::Build,
            show_ast: false,
            show_table: false,
            show_full_ast: false,
            debug_level: 0,
            interpreter_args: vec![],
        }
    }
}

impl Options {
    pub fn with_debug(self: Options, debug_level: i32) -> Options {
        Options{debug_level, ..self}
    }

    pub fn new<I, T>(args: I) -> Options
    where
        I: IntoIterator<Item = T>,
        T: Into<String>,
    {
        let mut opts = Options::default();
        let mut got_dashdash = false;
        for f in args.into_iter().map(Into::into) {
            if got_dashdash {
                opts.interpreter_args.push(f.to_owned());
                continue;
            }
            if f.is_empty() {
            } else if f.starts_with('-') {
                match f.as_str() {
                    "-i" | "--interactive" => opts.cmd = Command::Repl,
                    "-r" | "--run" => opts.cmd = Command::Interpret,
                    "-d" => opts.debug_level += 1,
                    "--ast" => opts.show_ast = true,
                    "--table" => opts.show_table = true,
                    "--full-ast" => opts.show_full_ast = true,
                    "--version" => {
                        println!("{}{}", TITLE, VERSION);
                        return opts;
                    }
                    "--" => got_dashdash = true,
                    arg => {
                        if arg != "-h" && arg != "--help" {
                            eprintln!("unexpected flag '{}'", f);
                        }
                        print_cli_help();
                        return opts;
                    }
                }
            } else {
                if opts.files.is_empty() {
                    // This is the first argument, so it should be the 'main'.
                    opts.interpreter_args.push(f.to_owned());
                }
                opts.files.push(f.to_string());
            }
        }
        opts
    }
}

pub fn print_cli_info() {
    eprintln!("{}{}", TITLE, VERSION);
}

pub fn print_cli_help() {
    print_cli_info();
    eprintln!("{}", USAGE);
}

pub const TITLE: &str = "tako v";

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub const USAGE: &str = "An experimental programming language for ergonomic software verification.

Usage:
  tako [-i|-r] [-d <level>] [--ast] [--full-ast] [--table] <files>...
  tako (-h | --help)
  tako --version

Options:
  -i --interactive    Run as a repl (interactive mode).
  -r --run            Run files in interpreter.
  -d --debug=<level>  Level of debug logging to use [default: 0].
  --ast               Pretty print an abstract syntax tree of the code.
  --full-ast          Debug print an abstract syntax tree of the code.
  --table             Pretty print the symbol table of the code.
  -h --help           Show this screen.
  --version           Show compiler version.
";
