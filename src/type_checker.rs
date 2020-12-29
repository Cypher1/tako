use crate::ast::{Node, Node::*, Prim::*};
use crate::database::Compiler;
use crate::errors::TError;

use crate::types::{bit_type, i32_type, record, string_type, unit_type, void_type, Type};

pub fn infer(db: &dyn Compiler, expr: &Node) -> Result<Type, TError> {
    // Infer that expression t has type A, t => A
    // See https://ncatlab.org/nlab/show/bidirectional+typechecking
    use crate::ast::*;
    match expr {
        PrimNode(prim) => match prim {
            Void(_) => Ok(void_type()),
            Unit(_) => Ok(unit_type()),
            I32(_, _) => Ok(i32_type()),
            Bool(_, _) => Ok(bit_type()),
            Str(_, _) => Ok(string_type()),
            Lambda(node) => infer(db, node.as_ref()), // TODO: abstraction
            Struct(vals, _) => {
                let mut tys: Vec<Type> = vec![];
                for val in vals.iter() {
                    tys.push(infer(db, &val.1.clone().to_node())?);
                }
                Ok(record(tys)?)
            }
            TypeValue(_ty, _) => Ok(Type::Variable("Type".to_string())),
        },
        UnOpNode(UnOp {
            name: _,
            inner: _,
            info: _,
        }) => panic!("TODO Impl type checking for UnOp"),
        BinOpNode(BinOp {
            name: _,
            left: _,
            right: _,
            info: _,
        }) => panic!("TODO Impl type checking for BinOp"),
        SymNode(Sym { name: _, info: _ }) => panic!("TODO Impl type checking for Sym"),
        ApplyNode(Apply {
            inner: _,
            args: _,
            info: _,
        }) => panic!("TODO Impl type checking for Apply"),
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
    use crate::ast::{Info, ToNode};
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
        let prog = TypeValue(infer(&db, &prog_str).unwrap(), Info::default());
        let ty = db.parse_str(module, ty).unwrap();
        dbg!(&ty);
        let mut state = vec![HashMap::new()];
        let result_type = Interpreter::default().visit(&db, &mut state, &ty);
        assert_eq!(Ok(prog), result_type);
    }

    #[test]
    fn infer_type_of_i32() {
        let db = DB::default();
        let num = I32(23, Info::default()).to_node();
        assert_eq!(infer(&db, &num), Ok(i32_type()));
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
        assert_type("{x}", ",(x: X): X");
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
}
