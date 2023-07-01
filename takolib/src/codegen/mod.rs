pub mod backend;
pub mod cpp_ast;

use crate::ast::{Ast, NodeId};
use crate::error::TError;
use crate::primitives::Prim;
use backend::{backend, create_context, Backend, BackendConfig, BackendStateTrait};
use std::path::Path;

pub fn codegen(path: &Path, _ast: &Ast, _root: Option<NodeId>) -> Result<Prim, TError> {
    let config = BackendConfig {};
    let context = create_context();
    {
        let mut bend = backend(config, &context);
        {
            let mut cg = bend.add_module("main").expect("Codegen error"); // TODO: Convert

            // for each function, code gen it
            // bend.add_function();
            let (_main, argc, argv) = cg.add_main();

            let char_star_type = cg.string_type();
            //let i32_type = cg.i32_type();
            // let zero = cg.const_int(i32_type, 0);
            let argv_0 = cg.access_into_array(char_star_type.into(), argv.into_pointer_value());
            cg.printf("ARGC: %d, ARGV: %s\n", &[argc, argv_0]);
            let argc = argc.into_int_value();
            let argc = std::ptr::addr_of!(argc);
            unsafe {
                cg.build_return(Some(&*argc));
            }
            cg.create_binary(path)?;
            Ok(Prim::Str(path.display().to_string()))
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::parser::{parse, tokens::lex};

    use super::*;
    use std::path::PathBuf;

    fn test_build_output_dir() -> PathBuf {
        Path::new("/tmp/tako_tests/llvm_backend").to_path_buf()
    }
    fn test_file1() -> PathBuf {
        test_build_output_dir().join("test.tk")
    }

    fn setup(s: &str) -> Result<(PathBuf, Ast, NodeId), TError> {
        crate::ensure_initialized();
        std::fs::create_dir_all(test_build_output_dir()).expect("Make test output dir");

        let path = test_file1();
        let tokens = lex(s)?;
        let ast = parse(&path, s, &tokens)?;
        assert!(!ast.roots.is_empty());
        let root = ast.roots[0];
        Ok((path, ast, root))
    }

    #[test]
    fn can_print_hello_world_using_codegen() -> Result<(), TError> {
        let (path, ast, root) = setup("x=1")?;

        codegen(&path, &ast, root)?;

        // TODO: Run and check hello world program's output.
        Ok(())
    }
}
