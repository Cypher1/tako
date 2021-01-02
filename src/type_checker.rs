use crate::ast::{Node, Node::*};
use crate::database::Compiler;
use crate::errors::TError;

use crate::primitives::{bit_type, i32_type, record, string_type, unit_type, void_type, Prim::*, Type};

pub fn infer(db: &dyn Compiler, expr: &Node, env: &Type) -> Result<Type, TError> {
    // Infer that expression t has type A, t => A
    // See https://ncatlab.org/nlab/show/bidirectional+typechecking
    use crate::ast::*;
    match expr {
        PrimNode(prim, _) => match prim {
            Void() => Ok(void_type()),
            Unit() => Ok(unit_type()),
            I32(_) => Ok(i32_type()),
            Bool(_) => Ok(bit_type()),
            Str(_) => Ok(string_type()),
            Lambda(node) => infer(db, node.as_ref(), env), // TODO: abstraction
            Struct(vals) => {
                let mut tys: Vec<Type> = vec![];
                for val in vals.iter() {
                    tys.push(infer(db, &val.1.clone().to_node(), env)?);
                }
                Ok(record(tys)?)
            }
            TypeValue(_ty) => Ok(Type::Variable("Type".to_string())),
        },
        UnOpNode(UnOp {
            name,
            inner: _,
            info: _,
        }) => {
            if let Some(ext) = db.get_extern(name.to_string())? {
                // TODO intros
                return Ok(ext.ty);
            }
            panic!("TODO Impl type checking for user defined UnOp")
        },
        BinOpNode(BinOp {
            name,
            left: _,
            right: _,
            info: _,
        }) => {
            if let Some(ext) = db.get_extern(name.to_string())? {
                // TODO intros
                return Ok(ext.ty);
            }
            panic!("TODO Impl type checking for user defined BinOp")
        },
        SymNode(Sym { name, info: _ }) => {
            if let Some(ext) = db.get_extern(name.to_string())? {
                // TODO intros
                return Ok(ext.ty);
            }
            panic!("TODO Impl type checking for user defined Sym")
        },
        ApplyNode(Apply {
            inner,
            args,
            info: _,
        }) => {
            let inner_ty = infer(db, inner, env)?;
            let mut arg_tys = vec![];
            for arg in args.iter() {
                let ty = infer(db, &arg.clone().to_node(), env)?;
                arg_tys.push(ty);
            }
            panic!("TODO Type checking apply: {:?} with {:?}", inner_ty, arg_tys)
        },
        LetNode(Let {
            name: _,
            value: _,
            args: _,
            info: _,
        }) => panic!("TODO Impl type checking for Let"),
        Error(err) => panic!("TODO Impl type checking for Let {}", err),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ToNode;
    use crate::database::DB;

    fn assert_type(prog: &'static str, ty: &'static str) {
        use crate::ast::Visitor;
        use crate::cli_options::Options;
        use crate::interpreter::Interpreter;
        use std::collections::HashMap;
        let mut db = DB::default();
        db.set_options(Options::default());
        let module = vec![];
        let prog_str = db.parse_str(module.clone(), prog).unwrap();
        dbg!(&prog_str);

        let env = Type::Record(set![]); // TODO: Track the type env
        let prog = TypeValue(infer(&db, &prog_str, &env).unwrap());
        let ty = db.parse_str(module, ty).unwrap();
        dbg!(&ty);
        let mut state = vec![HashMap::new()];
        let result_type = Interpreter::default().visit(&db, &mut state, &ty);
        if let Err(err) = &result_type {
            dbg!(format!("{}", &err));
        }
        assert_eq!(format!("{}", &prog), format!("{}", result_type.clone().unwrap()));
        assert_eq!(prog, result_type.unwrap());
    }

    #[test]
    fn infer_type_of_i32() {
        let db = DB::default();
        let num = I32(23).to_node();
        let env = Type::Record(set![]); // TODO: Track the type env
        assert_eq!(infer(&db, &num, &env), Ok(i32_type()));
        assert_type("23", "I32");
    }

    #[test]
    fn infer_type_of_str() {
        assert_type("\"23\"", "String");
    }

    // #[test]
    fn infer_type_of_sym_i32() {
        assert_type("x=12;x", "I32");
    }

    // #[test]
    fn infer_type_of_sym_str() {
        assert_type("x=\"12\";x", "String");
    }

    // #[test]
    fn infer_type_of_pair_str_i32() {
        assert_type("(\"12\",23)", "(String, I32)");
    }

    // #[test]
    fn infer_type_of_sym_with_extra_lets_i32() {
        assert_type("x=12;y=4;x", "I32");
    }

    // #[test]
    fn infer_type_of_id() {
        assert_type("{x}", "(x=X) -> X");
    }

    // #[test]
    fn infer_type_of_id_apply() {
        assert_type("{x}(x=12)", "I32");
    }

    // #[test]
    fn infer_type_of_id_apply_it_arg() {
        assert_type("{it}(12)", "I32");
    }

    // #[test]
    fn infer_type_of_id_apply_explicit_it_arg() {
        assert_type("{it}(it=12)", "I32");
    }

    // #[test]
    fn infer_type_of_plus_expr() {
        assert_type("12+32", "I32");
    }

    #[test]
    fn infer_type_of_argc() {
        assert_type("argc", "I32");
    }

    // #[test]
    fn infer_type_of_argv() {
        assert_type("argv", "(it=I32) -> String");
    }
}
