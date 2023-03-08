use crate::error::TError;
use std::path::Path;
use crate::ast::{Ast, NodeId};
use crate::backend::{Backend, BackendStateTrait, create_context, backend, BackendConfig};

pub fn codegen(_path: &Path, _ast: &Ast, _root: NodeId) -> Result<(), TError> {
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
            cg.printf("ARGC: %d, ARGV: %s\n", &[argc.into(), argv_0.into()]);
            let argc = argc.into_int_value();
            let argc = std::ptr::addr_of!(argc);
            unsafe {
                cg.build_return(Some(&*argc));
            }
            Ok(())
        }
    }
}
