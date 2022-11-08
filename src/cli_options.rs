use crate::ui::UiMode;
use log::warn;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Command {
    Build,
    Interpret,
    Repl,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CodeGenOptions {
    // TODO: Add options
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Options {
    pub executable_location: Option<String>,
    pub files: Vec<String>,
    pub cmd: Command,
    pub interpreter_args: Vec<String>,
    pub ui_mode: UiMode,
    pub early_exit: bool,
    /// `optimization_level` should be proportional to maximum time spent on optimisation.
    pub optimization_level: u32,
    pub code_gen: Option<CodeGenOptions>,
}

impl Default for Options {
    fn default() -> Options {
        Options {
            executable_location: None,
            files: vec![],
            cmd: Command::Build,
            interpreter_args: vec![],
            ui_mode: UiMode::Cli,
            early_exit: false,
            optimization_level: 3,
            code_gen: Some(CodeGenOptions {}),
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
            if opts.executable_location.is_none() {
                opts.executable_location = Some(f.clone());
                continue;
            }
            if got_dashdash {
                opts.interpreter_args.push(f.clone());
                continue;
            }
            if f.is_empty() {
            } else if f.starts_with('-') {
                match f.as_str() {
                    "-i" | "--interactive" => opts.cmd = Command::Repl,
                    "-r" | "--run" => opts.cmd = Command::Interpret,
                    "-e" | "--early-exit" => opts.early_exit = true,
                    "--cli" => opts.ui_mode = UiMode::Cli,
                    "--tui" => opts.ui_mode = UiMode::Tui,
                    "-O0" => opts.optimization_level = 0,
                    "-O1" => opts.optimization_level = 1,
                    "-O2" => opts.optimization_level = 2,
                    "-O3" => opts.optimization_level = 3,
                    "--version" => {
                        print_cli_info();
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
    println!("{TITLE}{VERSION}");
}

pub fn print_cli_help() {
    print_cli_info();
    println!("{USAGE}");
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
  -h --help           Show this screen.
  --version           Show compiler version.

Configuration:
  -e --early-exit     Quit after the first error.
  -O<n>               Optimisation level: 0|1|2|3.
  --cli               Use a simpler command line interface.
  --tui               Use an interactive terminal user interface.
";
