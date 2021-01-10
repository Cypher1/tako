use crate::ast::{Node, Node::*};
use crate::database::Compiler;
use crate::errors::TError;
use crate::interpreter::Interpreter;
use std::collections::HashMap;

use crate::primitives::{
    bit_type, i32_type, record, string_type, unit_type, void_type, Prim, Prim::*,
};

pub fn infer(db: &dyn Compiler, expr: &Node, env: &Prim) -> Result<Prim, TError> {
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
                let mut tys: Vec<Prim> = vec![];
                for val in vals.iter() {
                    tys.push(infer(db, &val.1.clone().to_node(), env)?);
                }
                Ok(record(tys)?)
            }
            _ty => Ok(Prim::Variable("Type".to_string())),
        },
        UnOpNode(UnOp { name, inner, info }) => {
            let it_ty = infer(db, inner, env)?;
            let ty = infer(db, &Sym {name: name.to_string(), info: info.clone()}.to_node(), env)?;
            eprintln!("({})(it = {})", &ty, &it_ty);
            let app = Apply {
                inner: Box::new(ty.to_node()),
                args: vec![Let {
                    name: "it".to_string(),
                    args: None,
                    value: Box::new(it_ty.to_node()),
                    info: info.clone(),
                }],
                info: info.clone(),
            };
            let mut state = vec![HashMap::new()];
            Interpreter::default().visit_apply(db, &mut state, &app)
        }
        BinOpNode(BinOp {
            name,
            left,
            right,
            info,
        }) => {
            let left_ty = infer(db, left, env)?;
            let env = if name == ";" {
                env.clone().merge(left_ty.clone())
            } else {
                env.clone()
            };
            let right_ty = infer(db, right, &env)?;
            let ty = infer(db, &Sym {name: name.to_string(), info: info.clone()}.to_node(), &env)?;
            eprintln!("({})(left = {}, right = {})", &ty, &left_ty, &right_ty);
            let app = Apply {
                inner: Box::new(ty.to_node()),
                args: vec![
                    Let {
                        name: "left".to_string(),
                        args: None,
                        value: Box::new(left_ty.to_node()),
                        info: info.clone(),
                    },
                    Let {
                        name: "right".to_string(),
                        args: None,
                        value: Box::new(right_ty.to_node()),
                        info: info.clone(),
                    },
                ],
                info: info.clone(),
            };
            let mut state = vec![HashMap::new()];
            Interpreter::default().visit_apply(db, &mut state, &app)
        }
        SymNode(Sym { name, info: _ }) => {
            if let Some(ext) = db.get_extern(name.to_string())? {
                // TODO intros
                return Ok(ext.ty);
            }
            Ok(env.access(name))
        }
        ApplyNode(Apply { inner, args, info }) => {
            let mut arg_tys = vec![];
            let mut let_tys = vec![];
            for arg in args.iter() {
                let ty = infer(db, &arg.value.clone().to_node(), env)?;
                let_tys.push((arg.name.clone(), ty.clone()));
                let ty_let = Let {
                    name: arg.name.clone(),
                    args: None,
                    value: Box::new(ty.to_node()),
                    info: info.clone(),
                };
                arg_tys.push(ty_let);
            }
            let inner_ty = infer(db, inner, &env.clone().merge(Struct(let_tys)))?;
            let app = Apply {
                inner: Box::new(inner_ty.to_node()),
                args: arg_tys,
                info: info.clone(),
            };
            let mut state = vec![HashMap::new()];
            Interpreter::default().visit_apply(db, &mut state, &app)
        }
        LetNode(Let {
            name,
            value,
            args,
            info,
        }) => {
            let ty = if let Some(ty) = info.ty.clone() {
                let mut state = vec![HashMap::new()];
                Interpreter::default().visit(db, &mut state, &ty)?
            } else {
                infer(db, &value.clone().to_node(), env)?
            };
            if let Some(args) = args {
                let mut arg_tys = rec![];
                for arg in args.iter() {
                    let ty = infer(db, &arg.clone().to_node(), env)?;
                    arg_tys = arg_tys.merge(ty);
                }
                let ty = Function {
                    intros: set![],
                    results: Box::new(ty),
                    arguments: Box::new(arg_tys),
                };
                Ok(Prim::Struct(vec![(name.clone(), ty)]))
            } else {
                let ty = Prim::Struct(vec![(name.clone(), ty)]);
                Ok(ty)
            }
        }
        Error(err) => Err(err.clone())
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
        let mut db = DB::default();
        db.set_options(Options::new(&["-d".to_string(), "-d".to_string(), "-d".to_string()]));
        let module = vec![];
        let prog_str = db.parse_str(module.clone(), prog).unwrap();
        dbg!(&prog_str);

        let env = rec![]; // TODO: Track the type env
        let prog_ast = infer(&db, &prog_str, &env).unwrap();

        let ty_ast = db.parse_str(module, ty).unwrap();
        let mut state = vec![HashMap::new()];
        let result_type = Interpreter::default().visit(&db, &mut state, &ty_ast);
        dbg!(&ty_ast, format!("{}", &result_type.clone().unwrap()));
        if let Err(err) = &result_type {
            dbg!(format!("{}", &err));
        }
        assert_eq!(
            format!("{}", &prog_ast),
            format!("{}", result_type.clone().unwrap())
        );
        assert_eq!(prog_ast, result_type.unwrap());
    }

    #[test]
    fn infer_type_of_i32() {
        let db = DB::default();
        let num = I32(23).to_node();
        let env = rec![]; // TODO: Track the type env
        assert_eq!(infer(&db, &num, &env), Ok(i32_type()));
        assert_type("23", "I32");
    }

    #[test]
    fn infer_type_of_str() {
        assert_type("\"23\"", "String");
    }

    #[test]
    fn infer_type_of_let_i32() {
        assert_type("x=12", "(x=I32)");
    }

    #[test]
    fn infer_type_of_let_string_to_i32() {
        assert_type("x(s: String)=12", "(x=(s=String)->I32)");
    }

    #[test]
    fn infer_type_of_sym_i32() {
        assert_type("x=12;x", "I32");
    }

    #[test]
    fn infer_type_of_sym_str() {
        assert_type("x=\"12\";x", "String");
    }

    #[test]
    fn infer_type_of_pair_str_i32() {
        assert_type("(\"12\",23)", "(String, I32)");
    }

    #[test]
    fn infer_type_of_sym_with_extra_lets_i32() {
        assert_type("x=12;y=4;x", "I32");
    }

    #[test]
    fn infer_type_of_sym_with_struct_lets_i32() {
        assert_type("x=12,y=4,x", "(x=I32,y=I32,it=I32)");
    }

    #[test]
    fn infer_type_of_sym_without_let() {
        assert_type("x", "a. (x=a) -> a");
    }

    #[test]
    fn infer_type_of_id() {
        assert_type("{x}", "a. (x=a) -> a");
    }

    #[test]
    fn infer_type_of_id_apply() {
        assert_type("{x}(x=12)", "I32");
    }

    #[test]
    fn infer_type_of_id_apply_it_arg() {
        assert_type("{it}(12)", "I32");
    }

    #[test]
    fn infer_type_of_id_apply_explicit_it_arg() {
        assert_type("{it}(it=12)", "I32");
    }

    #[test]
    fn infer_type_of_plus_expr() {
        assert_type("12+32", "I32");
    }

    #[test]
    fn infer_type_of_argc() {
        assert_type("argc", "I32");
    }

    #[test]
    fn infer_type_of_argv() {
        assert_type("argv", "(it=I32) -> String");
    }
}
