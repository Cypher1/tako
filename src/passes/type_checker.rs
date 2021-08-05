use crate::ast::{Node, Node::*};
use crate::database::DBStorage;
use crate::errors::TError;
use crate::passes::interpreter::Interpreter;
use log::*;
use std::collections::BTreeSet;
use std::collections::HashMap;

use crate::primitives::{bit_type, i32_type, record, string_type, Prim::*, Val, Val::*};

pub fn infer(storage: &mut DBStorage, expr: &Node, env: &Val) -> Result<Val, TError> {
    // Infer that expression t has type A, t => A
    // See https://ncatlab.org/nlab/show/bidirectional+typechecking
    use crate::ast::*;
    match expr {
        ValNode(prim, _) => match prim {
            Product(vals) => {
                let mut tys: BTreeSet<Val> = set![];
                for val in vals.iter() {
                    tys.insert(infer(storage, &val.clone().into_node(), env)?);
                }
                Ok(Product(tys))
            }
            Union(vals) => {
                let mut tys: BTreeSet<Val> = set![];
                for val in vals.iter() {
                    tys.insert(infer(storage, &val.clone().into_node(), env)?);
                }
                Ok(Union(tys))
            }
            PrimVal(I32(_)) => Ok(i32_type()),
            PrimVal(Bool(_)) => Ok(bit_type()),
            PrimVal(Str(_)) => Ok(string_type()),
            Lambda(node) => infer(storage, node.as_ref(), env), // TODO: abstraction
            Struct(vals) => {
                let mut tys: Vec<Val> = vec![];
                for val in vals.iter() {
                    tys.push(infer(storage, &val.1.clone().into_node(), env)?);
                }
                Ok(record(tys)?)
            }
            _ty => Ok(Val::Variable("Type".to_string())),
        },
        UnOpNode(UnOp { name, inner, info }) => {
            let it_ty = infer(storage, inner, env)?;
            let new_env = env.clone().merge(rec! {"it" => it_ty.clone()});
            let inner_ty = infer(
                storage,
                &Sym {
                    name: name.to_string(),
                    info: info.clone(),
                }
                .into_node(),
                &new_env,
            )?;
            let app = Apply {
                inner: Box::new(inner_ty.into_node()),
                args: vec![Let {
                    name: "it".to_string(),
                    args: None,
                    value: Box::new(it_ty.into_node()),
                    info: info.clone(),
                }],
                info: info.clone(),
            };
            let mut state = vec![];
            Interpreter::default().visit_apply(storage, &mut state, &app)
        }
        BinOpNode(BinOp {
            name,
            left,
            right,
            info,
        }) => {
            let left_ty = infer(storage, left, env)?;
            let tmp_env = env.clone().merge(left_ty.clone());
            let right_ty = infer(storage, right, &tmp_env)?;
            let new_env = env
                .clone()
                .merge(rec! {"left" => left_ty.clone(), "right" => right_ty.clone()});
            let inner_ty = infer(
                storage,
                &Sym {
                    name: name.to_string(),
                    info: info.clone(),
                }
                .into_node(),
                &new_env,
            )?;
            let app = Apply {
                inner: Box::new(inner_ty.into_node()),
                args: vec![
                    Let {
                        name: "left".to_string(),
                        args: None,
                        value: Box::new(left_ty.into_node()),
                        info: info.clone(),
                    },
                    Let {
                        name: "right".to_string(),
                        args: None,
                        value: Box::new(right_ty.into_node()),
                        info: info.clone(),
                    },
                ],
                info: info.clone(),
            };
            let mut state = vec![];
            Interpreter::default().visit_apply(storage, &mut state, &app)
        }
        SymNode(Sym { name, info: _ }) => {
            if let Some(ext) = storage.get_extern(name.to_string())? {
                // TODO intros
                let mut frame = HashMap::new();
                for (name, ty) in env.clone().into_struct().iter() {
                    frame.insert(name.clone(), ty.clone());
                }
                let mut state = vec![frame];
                return Interpreter::default().visit(storage, &mut state, &ext.ty);
            }
            debug!("access sym: {}", name);
            debug!("env sym: {}", env);
            Ok(env.access(name))
        }
        ApplyNode(Apply { inner, args, info }) => {
            let mut arg_tys = vec![];
            let mut let_tys = vec![];
            for arg in args.iter() {
                let ty = infer(storage, &arg.value.clone().into_node(), env)?;
                let_tys.push((arg.name.clone(), ty.clone()));
                let ty_let = Let {
                    name: arg.name.clone(),
                    args: None,
                    value: Box::new(ty.into_node()),
                    info: info.clone(),
                };
                arg_tys.push(ty_let);
            }
            let new_env = env.clone().merge(Struct(let_tys));
            debug!("new_env: {}", &new_env);
            let inner_ty = infer(storage, inner, &new_env)?;
            let app = Apply {
                inner: Box::new(inner_ty.into_node()),
                args: arg_tys,
                info: info.clone(),
            };
            let mut state = vec![HashMap::new()];
            Interpreter::default().visit_apply(storage, &mut state, &app)
        }
        AbsNode(Abs {
            name,
            value,
            info: _,
        }) => {
            let ty = infer(storage, &value.clone().into_node(), env)?;
            match ty {
                Function {
                    mut intros,
                    arguments,
                    results,
                } => {
                    intros.insert((name.to_string(), Variable("typename".to_string())));
                    Ok(Function {
                        intros,
                        arguments,
                        results,
                    })
                }
                ty => panic!("abstraction over {} for {}", &name, &ty),
            }
        }
        LetNode(Let {
            name,
            value,
            args,
            info,
        }) => {
            let ty = if let Some(ty) = info.ty.clone() {
                let mut state = vec![HashMap::new()];
                Interpreter::default().visit(storage, &mut state, &ty)?
            } else {
                infer(storage, &value.clone().into_node(), env)?
            };
            let ty = if let Some(args) = args {
                let mut arg_tys = rec![];
                for arg in args.iter() {
                    let ty = infer(storage, &arg.clone().into_node(), env)?;
                    arg_tys = arg_tys.merge(ty);
                }
                Function {
                    intros: set![],
                    results: Box::new(ty),
                    arguments: Box::new(arg_tys),
                }
            } else {
                ty
            };
            Ok(Val::Struct(vec![(name.clone(), ty)]))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ToNode;
    use crate::database::DBStorage;

    type Test = Result<(), TError>;

    fn assert_type(prog_str: &'static str, type_str: &'static str) -> Test {
        dbg!(&prog_str);
        dbg!(&type_str);
        use crate::ast::Visitor;
        let mut storage = DBStorage::default();

        let type_filename = "test/type.tk";
        storage.set_file(type_filename, type_str.to_owned());
        let type_module = storage.module_name(type_filename.to_owned());

        let ty = storage.look_up_definitions(type_module)?;
        let result_type = Interpreter::default().visit_root(&mut storage, &ty);
        if let Err(err) = &result_type {
            dbg!(format!("{}", &err));
        }
        let result_type = result_type?;

        let prog_filename = "test/prog.tk";
        let prog_module = storage.module_name(prog_filename.to_owned());

        let prog = storage.parse_str(prog_module, prog_str)?;

        let env = Variable("test_program".to_string()); // TODO: Track the type env
        let prog_ty = infer(&mut storage, &prog, &env)?;

        info!("got: {}", &prog_ty);
        info!("expected: {}", &result_type);
        assert_eq!(format!("{}", &prog_ty), format!("{}", &result_type));
        assert_eq!(prog_ty, result_type);
        Ok(())
    }

    #[test]
    fn infer_type_of_i32() -> Test {
        use crate::primitives::int32;
        let mut storage = DBStorage::default();
        let num = int32(23).into_node();
        let env = rec![]; // TODO: Track the type env
        assert_eq!(infer(&mut storage, &num, &env), Ok(i32_type()));
        assert_type("23", "I32")
    }

    #[test]
    fn infer_type_of_str() -> Test {
        assert_type("\"23\"", "String")
    }

    #[test]
    fn infer_type_of_let_i32() -> Test {
        assert_type("x=12", "(x=I32)")
    }

    // #[test]
    fn infer_type_of_let_string_to_i32() -> Test {
        assert_type("x(s: String)=12", "(x=(s=String)->I32)")
    }

    #[test]
    fn infer_type_of_sym_i32() -> Test {
        assert_type("x=12;x", "I32")
    }

    #[test]
    fn infer_type_of_sym_str() -> Test {
        assert_type("x=\"12\";x", "String")
    }

    //#[test]
    fn infer_type_of_pair_str_i32() -> Test {
        assert_type("(\"12\",23)", "(String, I32)")
    }

    #[test]
    fn infer_type_of_sym_with_extra_lets_i32() -> Test {
        assert_type("x=12,y=4;x", "I32")
    }

    #[test]
    fn infer_type_of_sym_with_struct_lets_i32() -> Test {
        assert_type("x=12,y=4,x", "(x=I32,y=I32,it=I32)")
    }

    // #[test]
    fn infer_type_of_sym_without_let() -> Test {
        assert_type("x", "test_program |- x |- test_program.x")
    }

    // #[test]
    fn infer_type_of_id() -> Test {
        assert_type("{x}", "a|-(x=a) -> a")
    }

    #[test]
    fn infer_type_of_id_apply() -> Test {
        assert_type("{x}(x=12)", "I32")
    }

    #[test]
    fn infer_type_of_id_apply_it_arg() -> Test {
        assert_type("{it}(12)", "I32")
    }

    #[test]
    fn infer_type_of_id_apply_explicit_it_arg() -> Test {
        assert_type("{it}(it=12)", "I32")
    }

    #[test]
    fn infer_type_of_plus_expr() -> Test {
        assert_type("12+32", "I32")
    }

    #[test]
    fn infer_type_of_argc() -> Test {
        assert_type("argc", "I32")
    }

    #[test]
    fn infer_type_of_argv() -> Test {
        assert_type("argv", "(it=I32) -> String")
    }
}
