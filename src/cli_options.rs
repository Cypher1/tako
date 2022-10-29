use log::warn;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Command {
    Build,
    Interpret,
    Repl,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Options {
    pub files: Vec<String>,
    pub cmd: Command,
    pub interpreter_args: Vec<String>,
}

impl Default for Options {
    fn default() -> Options {
        Options {
            files: vec![],
            cmd: Command::Build,
            interpreter_args: vec![],
        }
    }
}

impl Options {
    #[must_use]
    pub fn with_file(self: Options, filename: &str) -> Options {
        let mut files = self.files;
        files.push(filename.to_owned());
        Options { files, ..self }
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
                opts.interpreter_args.push(f.clone());
                continue;
            }
            if f.is_empty() {
            } else if f.starts_with('-') {
                match f.as_str() {
                    "-i" | "--interactive" => opts.cmd = Command::Repl,
                    "-r" | "--run" => opts.cmd = Command::Interpret,
                    "--version" => {
                        println!("{}{}", TITLE, VERSION);
                        return opts;
                    }
                    "--" => got_dashdash = true,
                    arg => {
                        if arg != "-h" && arg != "--help" {
                            warn!("unexpected flag '{}'", f);
                        }
                        print_cli_help();
                        return opts;
                    }
                }
            } else {
                if opts.files.is_empty() {
                    // This is the first argument, so it should be the 'main'.
                    opts.interpreter_args.push(f.clone());
                }
                opts.files.push(f.to_string());
            }
        }
        opts
    }
}

pub fn print_cli_info() {
    println!("{}{}", TITLE, VERSION);
}

pub fn print_cli_help() {
    print_cli_info();
    println!("{}", USAGE);
}

pub const TITLE: &str = "tako v";

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub const USAGE: &str = "An experimental programming language for ergonomic software verification.

Usage:
  tako [-i|-r|-si|-sr] <files>...
  tako (-h | --help)
  tako --version

Options:
  -i --interactive    Run as a repl (interactive mode).
  -r --run            Run files in interpreter.
  -si --stack_interactive    Run as a repl (interactive mode) using the experimental stack based interpter.
  -sr --stack_run            Run files in the experimental stack based interpter.
  -h --help           Show this screen.
  --version           Show compiler version.
";
