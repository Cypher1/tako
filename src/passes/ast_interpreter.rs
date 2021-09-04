use crate::ast::{Abs, Apply, BinOp, HasInfo, Info, Let, Root, Sym, ToNode, UnOp, Visitor};
use crate::database::DBStorage;
use crate::errors::TError;
use crate::externs::*;
use crate::primitives::{
    boolean, int32, merge_vals, never_type, Frame,
    Prim::{Bool, I32},
    Val,
    Val::{Function, Lambda, PrimVal, Product, Struct, Variable},
};
use log::{debug, info};
use std::collections::HashMap;

pub type ImplFn<'a> =
    &'a mut dyn FnMut(&mut DBStorage, HashMap<String, Box<dyn Fn() -> Res>>, Info) -> Res;
pub type PureImplFn<'a> =
    &'a dyn Fn(&DBStorage, HashMap<String, Box<dyn Fn() -> Res>>, Info) -> Res;

// Walks the AST interpreting it.
pub struct Interpreter<'a> {
    pub impls: HashMap<String, ImplFn<'a>>,
}

impl<'a> Default for Interpreter<'a> {
    fn default() -> Interpreter<'a> {
        Interpreter {
            impls: HashMap::new(),
        }
    }
}

fn find_symbol<'a>(state: &'a [Frame], name: &str) -> Option<&'a Val> {
    for frame in state.iter().rev() {
        if let Some(val) = frame.get(name) {
            return Some(val); // This is the variable
        }
        // Not in this frame, go back up.
    }
    None
}

type State = Vec<Frame>;
impl<'a> Visitor<State, Val, Val> for Interpreter<'a> {
    fn visit_root(&mut self, storage: &mut DBStorage, root: &Root) -> Res {
        let mut base_frame = map! {};
        for (name, ext) in storage.get_externs().iter() {
            base_frame.insert(name.clone(), ext.value.clone());
        }
        let mut state = vec![base_frame];
        self.visit(storage, &mut state, &root.ast)
    }

    fn visit_sym(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Sym) -> Res {
        let name = &expr.name;
        debug!("evaluating sym '{}'", name);
        let value = find_symbol(state, name);
        if let Some(prim) = value {
            debug!("{} = (from stack) {}", name, prim.clone().into_node());
            return Ok(prim.clone());
        }
        if let Some(ext) = crate::externs::get_implementation(name) {
            debug!("{} = (from externs)", name);
            let frame = || {
                let mut frame_vals: HashMap<String, Box<dyn Fn() -> Res>> = map!();
                for (name, val) in state.last().expect("Stack frame missing") {
                    let val = val.clone();
                    frame_vals.insert(name.to_string(), Box::new(move || Ok(val.clone())));
                }
                frame_vals
            };
            return ext(storage, frame(), expr.get_info());
        }
        Err(TError::UnknownSymbol(
            name.to_string(),
            expr.info.clone(),
            "interpreter::?".to_string(),
        ))
    }

    fn visit_val(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Val) -> Res {
        debug!("visiting prim {}", &expr);
        match expr {
            Variable(name) => {
                let frame = state.last().cloned().unwrap_or_default();
                let mut kvs = vec![];
                for (k, v) in &frame {
                    kvs.push(format!("{} = {}", k, v));
                }
                debug!("variable {}, state: {}", &name, &kvs.join(","));
                return state
                    .last()
                    .expect("Stack frame missing")
                    .get(name)
                    .cloned()
                    .ok_or_else(|| {
                        TError::OutOfScopeTypeVariable(name.to_string(), Info::default())
                    }); // TODO: Get some info?
            }
            Product(tys) => {
                let mut new_tys = set![];
                for ty in tys.iter() {
                    let new_ty = self.visit_val(storage, state, ty)?; // Evaluate the type
                    if new_tys.len() == 1 {
                        let ty = new_tys
                            .iter()
                            .cloned()
                            .next()
                            .expect("This should never fail (1 sized set shouldn't be empty)");
                        match (&ty, &new_ty) {
                            (Product(ty), Product(new_ty)) => {
                                new_tys = ty.union(new_ty).cloned().collect();
                                continue;
                            }
                            (Struct(ty), Struct(new_ty)) => {
                                new_tys = set![Struct(merge_vals(ty.clone(), new_ty.clone()))];
                                continue;
                            }
                            (Struct(ty), new_ty) => {
                                new_tys = set![Struct(merge_vals(
                                    ty.clone(),
                                    vec![("it".to_string(), new_ty.clone())]
                                ))];
                                continue;
                            }
                            _ => {}
                        }
                    }
                    new_tys.insert(new_ty);
                }
                if new_tys.len() == 1 {
                    let ty = new_tys
                        .iter()
                        .cloned()
                        .next()
                        .expect("This should never fail (1 sized set shouldn't be empty)");
                    return Ok(ty);
                }
                return Ok(Product(new_tys));
            }
            Struct(tys) => {
                let mut new_tys = vec![];
                for (name, ty) in tys.iter() {
                    let new_ty = self.visit_val(storage, state, ty)?; // Evaluate the type
                    new_tys.push((name.clone(), new_ty));
                }
                return Ok(Struct(new_tys));
            }
            Function {
                arguments,
                results,
                intros: _,
            } => {
                if let Some(frame) = state.clone().last() {
                    let mut new_args = vec![];
                    for (arg, ty) in &arguments.clone().into_struct() {
                        let never = never_type();
                        let arg_ty = &frame.get(arg).unwrap_or(&never);
                        debug!(">> {}: {} unified with {}", &arg, &ty, &arg_ty);
                        let unified = ty.unify(arg_ty, state)?;
                        debug!(">>>> {}", &unified);
                        new_args.push((arg.clone(), unified));
                    }
                    let results = self.visit_val(storage, state, results)?;
                    return Ok(Function {
                        intros: dict!(),
                        arguments: Box::new(Struct(new_args)),
                        results: Box::new(results),
                    });
                }
            }
            _ => {}
        }
        Ok(expr.clone())
    }

    fn visit_apply(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Apply) -> Res {
        debug!("evaluating apply {}", expr.clone().into_node());
        state.push(Frame::new());
        expr.args
            .iter()
            .map(|arg| self.visit_let(storage, state, arg))
            .collect::<Result<Vec<Val>, TError>>()?;
        // Retrive the inner
        let inner = self.visit(storage, state, &*expr.inner)?;
        // Run the inner
        debug!(
            "apply args {:?} to inner {}",
            state.last(),
            inner.clone().into_node()
        );
        let res = match inner {
            Val::Lambda(func) => self.visit(storage, state, &*func)?,
            Val::PrimVal(prim) => {
                use crate::primitives::Prim;
                match prim {
                    Prim::BuiltIn(name) => {
                        debug!("looking up interpreter impl {}", name);
                        let frame = || {
                            let mut frame_vals: HashMap<String, Box<dyn Fn() -> Res>> = map!();
                            for (name, val) in state.last().expect("Stack frame missing") {
                                let val = val.clone();
                                frame_vals
                                    .insert(name.to_string(), Box::new(move || Ok(val.clone())));
                            }
                            frame_vals
                        };
                        if let Some(extern_impl) = &mut self.impls.get_mut(&name) {
                            return extern_impl(storage, frame(), expr.get_info());
                        }
                        debug!("looking up default impl {}", &name);
                        if let Some(default_impl) = crate::externs::get_implementation(&name) {
                            return default_impl(storage, frame(), expr.get_info());
                        }
                        panic!("Built a 'Built in' with unknown built in named {}", name);
                    }
                    prim => Val::PrimVal(prim),
                }
            }
            Val::Function {
                intros: _, // TODO
                arguments,
                results,
            } => {
                let frame = state.last_mut().expect("Stack frame missing");
                if let Struct(vals) = *arguments {
                    for (name, val) in vals {
                        frame.insert(name, val);
                    }
                } else {
                    frame.insert("it".to_string(), *arguments);
                }
                self.visit_val(storage, state, &*results)?
            }
            val => val,
        };
        state.pop();
        match res {
            Function { results, .. } => Ok(*results),
            res => {
                debug!("unexpected, apply on a {}", res);
                Ok(res)
            }
        }
    }

    fn visit_abs(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Abs) -> Res {
        debug!("introducing abstraction {}", expr.clone().into_node());
        // Add a new scope
        let mut frame = Frame::new();
        frame.insert(expr.name.clone(), Variable(expr.name.clone()));
        state.push(frame);
        let result = self.visit(storage, state, &expr.value)?;
        // Drop the finished scope
        state.pop();
        // TODO: Rewrap in the abstraction
        Ok(result)
    }

    fn visit_let(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Let) -> Res {
        debug!("evaluating let {}", expr.clone().into_node());
        if expr.args.is_some() {
            let val = Val::Lambda(expr.value.clone());
            state
                .last_mut()
                .expect("Stack frame missing")
                .insert(expr.name.clone(), val.clone());
            return Ok(val);
        }
        // Add a new scope
        state.push(Frame::new());
        let result = self.visit(storage, state, &expr.value)?;
        // Drop the finished scope
        state.pop();
        let frame = state.last_mut().expect("Stack frame missing");
        frame.insert(expr.name.clone(), result.clone());
        Ok(Val::Struct(vec![(expr.name.clone(), result)]))
    }

    fn visit_un_op(&mut self, storage: &mut DBStorage, state: &mut State, expr: &UnOp) -> Res {
        debug!("evaluating unop {}", expr.clone().into_node());
        let i = self.visit(storage, state, &expr.inner)?;
        let info = expr.clone().get_info();
        match expr.name.as_str() {
            "!" => match i {
                PrimVal(Bool(n)) => Ok(boolean(!n)),
                Lambda(_) => Ok(Lambda(Box::new(expr.clone().into_node()))),
                _ => Err(TError::TypeMismatch("!".to_string(), Box::new(i), info)),
            },
            "+" => match i {
                PrimVal(I32(n)) => Ok(int32(n)),
                Lambda(_) => Ok(Lambda(Box::new(expr.clone().into_node()))),
                _ => Err(TError::TypeMismatch("+".to_string(), Box::new(i), info)),
            },
            "-" => match i {
                PrimVal(I32(n)) => Ok(int32(-n)),
                Lambda(_) => Ok(Lambda(Box::new(expr.clone().into_node()))),
                _ => Err(TError::TypeMismatch("-".to_string(), Box::new(i), info)),
            },
            op => Err(TError::UnknownPrefixOperator(op.to_string(), info)),
        }
    }

    fn visit_bin_op(&mut self, storage: &mut DBStorage, state: &mut State, expr: &BinOp) -> Res {
        debug!("evaluating binop {}", expr.clone().into_node());
        let info = expr.clone().get_info();
        let l = self.visit(storage, state, &expr.left);
        let mut r = || self.visit(storage, state, &expr.right);
        match expr.name.as_str() {
            "+" => prim_add(&l?, &r()?, info),
            "++" => prim_add_strs(&l?, &r()?, info),
            "==" => prim_eq(&l?, &r()?, info),
            "!=" => prim_neq(&l?, &r()?, info),
            ">" => prim_gt(&l?, &r()?, info),
            "<" => prim_gt(&r()?, &l?, info),
            ">=" => prim_gte(&l?, &r()?, info),
            "<=" => prim_gte(&r()?, &l?, info),
            "-" => prim_sub(&l?, &r()?, info),
            "*" => prim_mul(&l?, &r()?, info),
            "/" => prim_div(&l?, &r()?, info),
            "%" => prim_mod(&l?, &r()?, info),
            "^" => prim_pow(&l?, &r()?, info),
            "&&" => prim_and(&l?, &r()?, info),
            "||" => prim_or(&l?, &r()?, info),
            "->" => prim_type_arrow(l?, r()?, info),
            "&" => prim_type_and(l?, r()?),
            "|" => prim_type_or(l?, r()?, info),
            "," => {
                let left = l?;
                let right = r()?;
                Ok(left.merge(right))
            }
            "." => {
                let l = l?;
                let r = r()?;
                self.visit_apply(
                    storage,
                    state,
                    &Apply {
                        inner: Box::new(r.into_node()),
                        args: vec![Let {
                            name: "it".to_string(),
                            value: Box::new(l.into_node()),
                            args: None,
                            info: info.clone(),
                        }],
                        info,
                    },
                )
            }
            ";" => {
                l?;
                let right = r()?;
                Ok(right)
            }
            "?" => match l {
                Err(_) => r(),
                l => l,
            },
            "-|" => match l {
                //TODO: Add pattern matching.
                Ok(PrimVal(Bool(false))) => Err(TError::RequirementFailure(info)),
                Ok(_) => r(),
                l => l,
            },
            op => Err(TError::UnknownInfixOperator(op.to_string(), info)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Node::ValNode;
    use crate::primitives::{
        boolean, i32_type, int32, number_type, record, string, string_type, sum, Val::Union,
    };

    fn get_db() -> DBStorage {
        DBStorage::default()
    }

    #[test]
    fn eval_num() {
        let mut storage = get_db();
        let tree = ValNode(int32(12), Info::default());
        assert_eq!(
            Interpreter::default().visit(&mut storage, &mut vec![], &tree),
            Ok(int32(12))
        );
    }

    fn eval_str(storage: &mut DBStorage, s: &str) -> Res {
        let filename = "test/file.tk";
        let module_name = storage.module_name(filename.to_owned());
        storage.set_file(filename, s.to_string());
        let root = storage.look_up_definitions(module_name)?;
        Interpreter::default().visit_root(storage, &root)
    }

    #[allow(dead_code)]
    fn trace<T: std::fmt::Display, E>(t: Result<T, E>) -> Result<T, E> {
        match &t {
            Ok(t) => debug!(">> {}", &t),
            Err(_) => debug!(">> #error"),
        }
        t
    }

    #[test]
    fn parse_and_eval_bool() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "true"), Ok(boolean(true)));
    }

    #[test]
    fn parse_and_eval_bool_and() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "true&&true"), Ok(boolean(true)));
        assert_eq!(eval_str(db, "false&&true"), Ok(boolean(false)));
        assert_eq!(eval_str(db, "true&&false"), Ok(boolean(false)));
        assert_eq!(eval_str(db, "false&&false"), Ok(boolean(false)));
    }

    #[test]
    fn parse_and_eval_bool_or() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "true||true"), Ok(boolean(true)));
        assert_eq!(eval_str(db, "false||true"), Ok(boolean(true)));
        assert_eq!(eval_str(db, "true||false"), Ok(boolean(true)));
        assert_eq!(eval_str(db, "false||false"), Ok(boolean(false)));
    }

    #[test]
    fn parse_and_eval_bool_eq() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "true==true"), Ok(boolean(true)));
        assert_eq!(eval_str(db, "false==true"), Ok(boolean(false)));
        assert_eq!(eval_str(db, "true==false"), Ok(boolean(false)));
        assert_eq!(eval_str(db, "false==false"), Ok(boolean(true)));
    }

    #[test]
    fn parse_and_eval_i32() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "32"), Ok(int32(32)));
    }

    #[test]
    fn parse_and_eval_i32_eq() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "0==0"), Ok(boolean(true)));
        assert_eq!(eval_str(db, "-1==1"), Ok(boolean(false)));
        assert_eq!(eval_str(db, "1==123"), Ok(boolean(false)));
        assert_eq!(eval_str(db, "1302==1302"), Ok(boolean(true)));
    }

    #[test]
    fn parse_and_eval_i32_pow() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "2^3"), Ok(int32(8)));
        assert_eq!(eval_str(db, "3^2"), Ok(int32(9)));
        assert_eq!(eval_str(db, "-4^2"), Ok(int32(-16)));
        assert_eq!(eval_str(db, "(-4)^2"), Ok(int32(16)));
        assert_eq!(eval_str(db, "2^3^2"), Ok(int32(512)));
    }

    #[test]
    fn parse_and_eval_str() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "\"32\""), Ok(string("32")));
    }

    #[test]
    fn parse_and_eval_let() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "x=3;x"), Ok(int32(3)));
    }

    #[test]
    fn parse_and_eval_let_with_args() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "x(it)=it*2;x(3)"), Ok(int32(6)));
    }

    #[test]
    fn parse_and_eval_i32_type() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "I32"), Ok(crate::primitives::i32_type()));
    }

    #[test]
    fn parse_and_eval_number_type() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "Number"), Ok(crate::primitives::number_type()));
    }

    #[test]
    fn parse_and_eval_string_type() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "String"), Ok(crate::primitives::string_type()));
    }

    #[test]
    fn parse_and_eval_string_or_number_type() {
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "String | Number"),
            Ok(Union(set![number_type(), string_type()]))
        );
    }

    #[test]
    fn parse_and_eval_string_and_number_type() {
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "String & Number"),
            Ok(Product(set![number_type(), string_type()]))
        );
    }

    #[test]
    fn parse_and_eval_tagged_string_or_number_type() {
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "String + I32"),
            sum(vec![string_type(), i32_type()])
        );
    }

    #[test]
    fn parse_and_eval_string_times_number_type() {
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "String * I32"),
            record(vec![string_type(), i32_type()])
        );
    }

    #[test]
    fn parse_and_eval_struct_x4_y5_access_x() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "struct(x=4, y=5)(\"x\")"), Ok(int32(4)));
    }

    #[test]
    fn parse_and_eval_struct_x4_y5_access_y() {
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "struct(x=4, y=\"Hi\")(\"y\")"),
            Ok(string("Hi"))
        );
    }

    #[test]
    fn parse_and_eval_struct_x4_y5() {
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "\"\"++struct(x=4, y=\"Hi\")"),
            Ok(string("(((it==\'x\')-|4)?((it==\'y\')-|\'Hi\'))")) // Ok(Str("struct(x=4, y=\"Hi\")".to_string()))
        );
    }

    #[test]
    fn parse_and_eval_struct_empty() {
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "struct()"),
            Ok(Lambda(Box::new(Product(set![]).into_node())))
        );
    }

    #[test]
    fn parse_and_eval_print() {
        use crate::primitives::Prim::BuiltIn;
        use crate::primitives::Val::PrimVal;
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "print"),
            Ok(PrimVal(BuiltIn("print".to_string())))
        );
    }

    #[test]
    fn tako_add_eq_rust_eq() {
        use rand::Rng;
        let db = &mut get_db();
        let mut rng = rand::thread_rng();
        for _ in 0..100 {
            let num1: i32 = rng.gen();
            let num2: i32 = rng.gen();
            let res = num1.wrapping_add(num2);
            info!("mul {:?} + {:?} => {:?}", num1, num2, res);
            assert_eq!(
                eval_str(db, &format!("mul(x, y)=x+y;mul(x= {}, y= {})", num1, num2)),
                Ok(int32(res))
            );
        }
    }

    #[test]
    fn tako_mul_eq_rust_eq() {
        use rand::Rng;
        let db = &mut get_db();
        let mut rng = rand::thread_rng();
        for _ in 0..100 {
            let num1: i32 = rng.gen();
            let num2: i32 = rng.gen();
            let res = num1.wrapping_mul(num2);
            info!("mul {:?} * {:?} => {:?}", num1, num2, res);
            assert_eq!(
                eval_str(db, &format!("mul(x, y)=x*y;mul(x= {}, y= {})", num1, num2)),
                Ok(int32(res))
            );
        }
    }
}
