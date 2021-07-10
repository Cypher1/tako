#![deny(clippy::all)]

#[macro_use]
pub mod data_structures;

#[cfg(test)]
#[macro_use]
mod pretty_assertions {
    // Wrapper around string slice that makes debug output `{:?}` to print string same way as `{}`.
    // Used in different `assert*!` macros in combination with `pretty_assertions` crate to make
    // test failures to show nice diffs.
    #[derive(PartialEq, Eq)]
    pub struct MultiPretty<T>(pub T);

    /// Make diff to display string as multi-line string
    impl<'a> std::fmt::Debug for MultiPretty<&'a str> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str(self.0)
        }
    }

    impl<'a> std::fmt::Debug for MultiPretty<Vec<&'a str>> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            for s in self.0.iter() {
                f.write_str(s)?;
                f.write_str("\n")?;
            }
            Ok(())
        }
    }

    impl<'a> std::fmt::Debug for MultiPretty<Vec<String>> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            for s in self.0.iter() {
                f.write_str(&s)?;
                f.write_str("\n")?;
            }
            Ok(())
        }
    }

    macro_rules! assert_str_eq {
        ($left:expr, $right:expr) => {
            pretty_assertions::assert_eq!(
                crate::pretty_assertions::MultiPretty($left),
                crate::pretty_assertions::MultiPretty($right)
            );
        };
    }
}

use std::fs::File;
use std::io::prelude::*;

pub mod ast;
pub mod cli_options;
pub mod database;
pub mod errors;
pub mod externs;
pub mod primitives;

mod location;
mod symbol_table;
mod tokens;

// This is where the fun, but currently unused stuff goes
#[allow(dead_code)]
mod experimental;

// This is where all the compiler passes (rather than shared infrastructure) gors.
pub mod passes;

mod components;
use ast::Visitor;
use passes::interpreter::Interpreter;
use passes::pretty_print::PrettyPrint;

use database::DBStorage;
use errors::TError;
use passes::interpreter::ImplFn;

pub fn work<'a>(
    storage: &mut DBStorage,
    filename: &str,
    print_impl: Option<ImplFn<'a>>,
) -> Result<String, TError> {
    let mut contents = String::new();
    let mut file = File::open(filename.to_owned())?;
    file.read_to_string(&mut contents)?;

    work_on_string(storage, contents, filename, print_impl)
}

pub fn work_on_string<'a>(
    storage: &mut DBStorage,
    contents: String,
    filename: &str,
    print_impl: Option<ImplFn<'a>>,
) -> Result<String, TError> {
    let module_name = storage.module_name(filename.to_owned());
    storage.set_file(filename, contents);

    use cli_options::Command;
    if storage.options.cmd == Command::Build {
        storage.build_with_gpp(module_name)
    } else {
        let root = storage.look_up_definitions(module_name)?;
        let mut interp = Interpreter::default();
        if let Some(print_impl) = print_impl {
            interp.impls.insert("print".to_string(), print_impl);
        }
        let res = interp.visit_root(storage, &root)?;
        use ast::ToNode;
        PrettyPrint::process(&res.into_node(), storage).or_else(|_| panic!("Pretty print failed"))
    }
}
