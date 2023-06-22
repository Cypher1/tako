use crate::ui::Mode as UiMode;
use log::warn;
use std::path::PathBuf;
use tako::ui::OptionsTrait;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Command {
    Build,
    Interpret,
    Repl,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Options {
    pub executable_location: String,
    pub files: Vec<PathBuf>,
    pub cmd: Command,
    pub interpreter_args: Vec<String>,
    pub ui_mode: UiMode,
    /// `optimization_level` should be proportional to maximum time spent on optimisation.
    pub optimization_level: u32,
}

impl OptionsTrait for Options {
    fn interactive(&self) -> bool {
        self.cmd == Command::Repl
    }

    fn oneshot(&self) -> bool {
        self.cmd == Command::Build || self.cmd == Command::Interpret
    }

    fn files(&self) -> &Vec<PathBuf> {
        &self.files
    }

    fn interpreter(&self) -> bool {
        self.cmd != Command::Build
    }
}

impl Default for Options {
    fn default() -> Self {
        Self {
            executable_location: String::new(),
            files: vec![],
            cmd: Command::Build,
            interpreter_args: vec![],
            ui_mode: UiMode::Tui,
            optimization_level: 3,
        }
    }
}

impl Options {
    #[must_use]
    pub fn with_file(self, filename: &str) -> Self {
        let mut files = self.files;
        files.push(filename.into());
        Self { files, ..self }
    }

    pub fn new<I, T>(args: I) -> Self
    where
        I: IntoIterator<Item = T>,
        T: Into<String>,
    {
        let mut opts = Self::default();
        let mut got_dashdash = false;
        for f in args.into_iter().map(Into::into) {
            if opts.executable_location.is_empty() {
                opts.executable_location = f.clone();
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
                    "--tui" => opts.ui_mode = UiMode::Tui,
                    "--http" => opts.ui_mode = UiMode::Http,
                    "-O0" => opts.optimization_level = 0,
                    "-O1" => opts.optimization_level = 1,
                    "-O2" => opts.optimization_level = 2,
                    "-O3" => opts.optimization_level = 3,
                    "--version" => {
                        print_cli_info();
                        std::process::exit(1);
                    }
                    "--" => got_dashdash = true,
                    arg => {
                        if arg != "-h" && arg != "--help" {
                            warn!("unexpected flag '{}'", f);
                        }
                        print_cli_help();
                        std::process::exit(1);
                    }
                }
            } else {
                if opts.files.is_empty() {
                    // This is the first argument, so it should be the 'main'.
                    opts.interpreter_args.push(f.clone());
                }
                opts.files.push(f.into());
            }
        }
        if opts.files.is_empty() && opts.cmd != Command::Repl {
            print_cli_help();
            std::process::exit(1);
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

pub const VERSION: &str = tako::VERSION;

pub const USAGE: &str = "An experimental programming language for ergonomic software verification.

Usage:
  tako [-i|-r] <files>...
  tako (-h | --help)
  tako --version

Options:
  -i --interactive    Run as a repl (interactive mode).
  -r --run            Run files in interpreter.
  -h --help           Show this screen.
  --version           Show compiler version.

Configuration:
  -O<n>               Optimisation level: 0|1|2|3.
  --tui               Use a terminal based interface.
  --http              Use a web based interface.
";
