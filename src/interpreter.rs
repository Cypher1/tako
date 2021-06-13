use super::ast::*;
use super::database::Compiler;
use super::errors::TError;
use super::primitives::{
    boolean, int32, merge_vals, string, void_type, Frame, Prim::*, Val, Val::*,
};
use std::collections::HashMap;

pub type ImplFn<'a> =
    &'a mut dyn FnMut(&dyn Compiler, HashMap<String, Box<dyn Fn() -> Res>>, Info) -> Res;
pub type PureImplFn<'a> =
    &'a dyn Fn(&dyn Compiler, HashMap<String, Box<dyn Fn() -> Res>>, Info) -> Res;

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

fn prim_add(l: &Val, r: &Val, _info: Info) -> Res {
    use super::primitives::sum;
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(Bool(r))) => {
            Ok(int32(if *l { 1 } else { 0 } + if *r { 1 } else { 0 }))
        }
        (PrimVal(Bool(l)), PrimVal(I32(r))) => Ok(int32(r.wrapping_add(if *l { 1 } else { 0 }))),
        (PrimVal(Bool(l)), PrimVal(Str(r))) => Ok(PrimVal(Str(l.to_string() + &r.to_string()))),
        (PrimVal(I32(l)), PrimVal(Bool(r))) => Ok(int32(l.wrapping_add(if *r { 1 } else { 0 }))),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(int32(l.wrapping_add(*r))),
        (PrimVal(I32(l)), PrimVal(Str(r))) => Ok(PrimVal(Str(l.to_string() + &r.to_string()))),
        (PrimVal(Str(l)), PrimVal(Bool(r))) => Ok(PrimVal(Str(l.to_string() + &r.to_string()))),
        (PrimVal(Str(l)), PrimVal(I32(r))) => Ok(PrimVal(Str(l.to_string() + &r.to_string()))),
        (PrimVal(Str(l)), PrimVal(Str(r))) => Ok(PrimVal(Str(l.to_string() + &r.to_string()))),
        (l, r) => Ok(sum(vec![l.clone(), r.clone()])?),
        //(l, r) => Err(TError::TypeMismatch2(
        //"+".to_string(),
        //Box::new((*l).clone()),
        //Box::new((*r).clone()),
        //info,
        //)),
    }
}

pub fn prim_add_strs(l: &Val, r: &Val, _info: Info) -> Res {
    let to_str = |v: &Val| {
        if let PrimVal(Str(s)) = v {
            s.to_string()
        } else {
            format!("{}", v)
        }
    };
    Ok(PrimVal(Str(format!("{}{}", to_str(l), to_str(r)))))
}

fn prim_eq(l: &Val, r: &Val, info: Info) -> Res {
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(Bool(r))) => Ok(boolean(*l == *r)),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(boolean(l == r)),
        (PrimVal(Str(l)), PrimVal(Str(r))) => Ok(boolean(l == r)),
        (l, r) => Err(TError::TypeMismatch2(
            "==".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_neq(l: &Val, r: &Val, info: Info) -> Res {
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(Bool(r))) => Ok(boolean(*l != *r)),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(boolean(l != r)),
        (PrimVal(Str(l)), PrimVal(Str(r))) => Ok(boolean(l != r)),
        (l, r) => Err(TError::TypeMismatch2(
            "!=".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_gt(l: &Val, r: &Val, info: Info) -> Res {
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(Bool(r))) => Ok(boolean(*l & !(*r))),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(boolean(l > r)),
        (PrimVal(Str(l)), PrimVal(Str(r))) => Ok(boolean(l > r)),
        (l, r) => Err(TError::TypeMismatch2(
            ">".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_gte(l: &Val, r: &Val, info: Info) -> Res {
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(Bool(r))) => Ok(boolean(*l >= *r)),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(boolean(l >= r)),
        (PrimVal(Str(l)), PrimVal(Str(r))) => Ok(boolean(l >= r)),
        (l, r) => Err(TError::TypeMismatch2(
            ">=".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_sub(l: &Val, r: &Val, info: Info) -> Res {
    match (l, r) {
        (PrimVal(I32(l)), PrimVal(Bool(r))) => Ok(int32(l - if *r { 1 } else { 0 })),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(int32(l - r)),
        (l, r) => Err(TError::TypeMismatch2(
            "-".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_mul(l: &Val, r: &Val, info: Info) -> Res {
    use super::primitives::record;
    let fail = || {
        Err(TError::TypeMismatch2(
            "*".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        ))
    };
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(I32(r))) => Ok(int32(if *l { *r } else { 0 })),
        (PrimVal(Bool(l)), PrimVal(Str(r))) => Ok(string(if *l { r } else { "" })),
        (PrimVal(I32(l)), PrimVal(Bool(r))) => Ok(int32(if *r { *l } else { 0 })),
        (PrimVal(Str(l)), PrimVal(Bool(r))) => Ok(string(if *r { l } else { "" })),
        (PrimVal(Bool(_)), PrimVal(_)) => fail(),
        (PrimVal(_), PrimVal(Bool(_))) => fail(),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(int32(l.wrapping_mul(*r))),
        (l, r) => Ok(record(vec![l.clone(), r.clone()])?),
    }
}

fn prim_div(l: &Val, r: &Val, info: Info) -> Res {
    match (l, r) {
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(int32(l / r)),
        (l, r) => Err(TError::TypeMismatch2(
            "/".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_mod(l: &Val, r: &Val, info: Info) -> Res {
    match (l, r) {
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(int32(l % r)),
        (l, r) => Err(TError::TypeMismatch2(
            "%".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_and(l: &Val, r: &Val, info: Info) -> Res {
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(Bool(r))) => Ok(boolean(*l && *r)),
        (l, r) => Err(TError::TypeMismatch2(
            "&&".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_or(l: &Val, r: &Val, info: Info) -> Res {
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(Bool(r))) => Ok(boolean(*l || *r)),
        (l, r) => Err(TError::TypeMismatch2(
            "||".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_type_arrow(l: Val, r: Val, _info: Info) -> Res {
    Ok(Val::Function {
        intros: dict!(),
        results: Box::new(r),
        arguments: Box::new(l),
    })
}

pub fn prim_type_and(l: Val, r: Val) -> Res {
    Ok(Val::Product(set!(l, r)))
}

fn prim_type_or(l: Val, r: Val, _info: Info) -> Res {
    Ok(Val::Union(set!(l, r)))
}

pub fn prim_pow(l: &Val, r: &Val, info: Info) -> Res {
    match (l, r) {
        (PrimVal(I32(l)), PrimVal(Bool(r))) => Ok(int32(if *r { *l } else { 1 })),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(int32(i32::pow(*l, *r as u32))), // TODO: require pos pow
        (l, r) => Err(TError::TypeMismatch2(
            "^".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

// TODO: Return nodes.
pub type Res = Result<Val, TError>;
type State = Vec<Frame>;
impl<'a> Visitor<State, Val, Val> for Interpreter<'a> {
    fn visit_root(&mut self, db: &dyn Compiler, root: &Root) -> Res {
        let mut base_frame = map! {};
        for (name, ext) in db.get_externs()?.iter() {
            base_frame.insert(name.to_owned(), ext.value.clone());
        }
        let mut state = vec![base_frame];
        self.visit(db, &mut state, &root.ast)
    }

    fn visit_sym(&mut self, db: &dyn Compiler, state: &mut State, expr: &Sym) -> Res {
        if db.debug_level() > 1 {
            eprintln!("evaluating sym {}", expr.clone().into_node());
        }
        let name = &expr.name;
        let value = find_symbol(&state, name);
        if let Some(prim) = value {
            if db.debug_level() > 0 {
                eprintln!("{} = (from stack) {}", name, prim.clone().into_node());
            }
            return Ok(prim.clone());
        }
        Err(TError::UnknownSymbol(
            name.to_string(),
            expr.info.clone(),
            "interpreter::?".to_string(),
        ))
    }

    fn visit_val(&mut self, db: &dyn Compiler, state: &mut State, expr: &Val) -> Res {
        if db.debug_level() > 2 {
            eprintln!("visiting prim {}", &expr);
        }
        match expr {
            Variable(name) => {
                let frame = state.last().cloned().unwrap_or_default();
                let mut kvs = vec![];
                for (k, v) in frame.iter() {
                    kvs.push(format!("{} = {}", k, v))
                }
                eprintln!("variable {}, state: {}", &name, &kvs.join(","));
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
                    let new_ty = self.visit_val(db, state, &ty)?; // Evaluate the type
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
                    let new_ty = self.visit_val(db, state, &ty)?; // Evaluate the type
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
                    for (arg, ty) in arguments.clone().into_struct().iter() {
                        let void = void_type();
                        let arg_ty = &frame.get(arg).unwrap_or(&void);
                        eprintln!(">> {}: {} unified with {}", &arg, &ty, &arg_ty);
                        let unified = ty.unify(arg_ty, state)?;
                        eprintln!(">>>> {}", &unified);
                        new_args.push((arg.clone(), unified));
                    }
                    let results = self.visit_val(db, state, results)?;
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

    fn visit_apply(&mut self, db: &dyn Compiler, state: &mut State, expr: &Apply) -> Res {
        if db.debug_level() > 1 {
            eprintln!("evaluating apply {}", expr.clone().into_node());
        }
        state.push(Frame::new());
        expr.args
            .iter()
            .map(|arg| self.visit_let(db, state, arg))
            .collect::<Result<Vec<Val>, TError>>()?;
        // Retrive the inner
        let inner = self.visit(db, state, &*expr.inner)?;
        // Run the inner
        if db.debug_level() > 2 {
            eprintln!(
                "apply args {:?} to inner {}",
                state.last(),
                inner.clone().into_node()
            );
        }
        let res = match inner {
            Val::Lambda(func) => self.visit(db, state, &*func)?,
            Val::PrimVal(prim) => {
                use crate::primitives::Prim;
                match prim {
                    Prim::BuiltIn(name) => {
                        if db.debug_level() > 2 {
                            eprintln!("looking up interpreter impl {}", name);
                        }
                        let frame = || {
                            let mut frame_vals: HashMap<String, Box<dyn Fn() -> Res>> = map!();
                            for (name, val) in
                                state.last().expect("Stack frame missing").clone().iter()
                            {
                                let val = val.clone();
                                frame_vals
                                    .insert(name.to_string(), Box::new(move || Ok(val.clone())));
                            }
                            frame_vals
                        };
                        if let Some(extern_impl) = &mut self.impls.get_mut(&name) {
                            return extern_impl(db, frame(), expr.get_info());
                        }
                        if db.debug_level() > 2 {
                            eprintln!("looking up default impl {}", &name);
                        }
                        if let Some(default_impl) =
                            crate::externs::get_implementation(name.to_owned())
                        {
                            return default_impl(db, frame(), expr.get_info());
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
                self.visit_val(db, state, &*results)?
            }
            val => val,
        };
        state.pop();
        match res {
            Function { results, .. } => Ok(*results),
            res => {
                if db.debug_level() > 1 {
                    eprintln!("unexpected, apply on a {}", res);
                }
                Ok(res)
            }
        }
    }

    fn visit_abs(&mut self, db: &dyn Compiler, state: &mut State, expr: &Abs) -> Res {
        if db.debug_level() > 1 {
            eprintln!("introducing abstraction {}", expr.clone().into_node());
        }

        // Add a new scope
        let mut frame = Frame::new();
        frame.insert(expr.name.clone(), Variable(expr.name.clone()));
        state.push(frame);
        let result = self.visit(db, state, &expr.value)?;
        // Drop the finished scope
        state.pop();
        // TODO: Rewrap in the abstraction
        Ok(result)
    }

    fn visit_let(&mut self, db: &dyn Compiler, state: &mut State, expr: &Let) -> Res {
        if db.debug_level() > 1 {
            eprintln!("evaluating let {}", expr.clone().into_node());
        }

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
        let result = self.visit(db, state, &expr.value)?;
        // Drop the finished scope
        state.pop();
        let frame = state.last_mut().expect("Stack frame missing");
        frame.insert(expr.name.clone(), result.clone());
        Ok(Val::Struct(vec![(expr.name.clone(), result)]))
    }

    fn visit_un_op(&mut self, db: &dyn Compiler, state: &mut State, expr: &UnOp) -> Res {
        if db.debug_level() > 1 {
            eprintln!("evaluating unop {}", expr.clone().into_node());
        }
        let i = self.visit(db, state, &expr.inner)?;
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

    fn visit_bin_op(&mut self, db: &dyn Compiler, state: &mut State, expr: &BinOp) -> Res {
        if db.debug_level() > 1 {
            eprintln!("evaluating binop {}", expr.clone().into_node());
        }
        let info = expr.clone().get_info();
        let l = self.visit(db, state, &expr.left);
        let mut r = || self.visit(db, state, &expr.right);
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
                    db,
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

    fn handle_error(&mut self, _db: &dyn Compiler, _state: &mut State, expr: &TError) -> Res {
        Err(expr.clone())
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::super::ast::*;
    use super::super::cli_options::Options;
    use super::super::database::{Compiler, DB};
    use super::super::primitives::{boolean, int32, number_type, string, string_type, Val::*};
    use super::{Interpreter, Res};
    use Node::*;

    #[test]
    fn eval_num() {
        let mut db = DB::default();
        db.set_options(Options::default());
        let tree = ValNode(int32(12), Info::default());
        assert_eq!(
            Interpreter::default().visit(&db, &mut vec![], &tree),
            Ok(int32(12))
        );
    }

    fn eval_str(s: &str) -> Res {
        use std::sync::Arc;
        let mut db = DB::default();
        let filename = "test/file.tk";
        let module_name = db.module_name(filename.to_owned());
        db.set_file(filename.to_owned(), Ok(Arc::new(s.to_string())));
        db.set_options(Options::default().with_debug(3));
        let root = db.look_up_definitions(module_name)?;
        Interpreter::default().visit_root(&db, &root)
    }

    #[allow(dead_code)]
    fn trace<T: std::fmt::Display, E>(t: Result<T, E>) -> Result<T, E> {
        match &t {
            Ok(t) => eprintln!(">> {}", &t),
            Err(_) => eprintln!(">> #error"),
        }
        t
    }

    #[test]
    fn parse_and_eval_bool() {
        assert_eq!(eval_str("true"), Ok(boolean(true)));
    }

    #[test]
    fn parse_and_eval_bool_and() {
        assert_eq!(eval_str("true&&true"), Ok(boolean(true)));
        assert_eq!(eval_str("false&&true"), Ok(boolean(false)));
        assert_eq!(eval_str("true&&false"), Ok(boolean(false)));
        assert_eq!(eval_str("false&&false"), Ok(boolean(false)));
    }

    #[test]
    fn parse_and_eval_bool_or() {
        assert_eq!(eval_str("true||true"), Ok(boolean(true)));
        assert_eq!(eval_str("false||true"), Ok(boolean(true)));
        assert_eq!(eval_str("true||false"), Ok(boolean(true)));
        assert_eq!(eval_str("false||false"), Ok(boolean(false)));
    }

    #[test]
    fn parse_and_eval_bool_eq() {
        assert_eq!(eval_str("true==true"), Ok(boolean(true)));
        assert_eq!(eval_str("false==true"), Ok(boolean(false)));
        assert_eq!(eval_str("true==false"), Ok(boolean(false)));
        assert_eq!(eval_str("false==false"), Ok(boolean(true)));
    }

    #[test]
    fn parse_and_eval_i32() {
        assert_eq!(eval_str("32"), Ok(int32(32)));
    }

    #[test]
    fn parse_and_eval_i32_eq() {
        assert_eq!(eval_str("0==0"), Ok(boolean(true)));
        assert_eq!(eval_str("-1==1"), Ok(boolean(false)));
        assert_eq!(eval_str("1==123"), Ok(boolean(false)));
        assert_eq!(eval_str("1302==1302"), Ok(boolean(true)));
    }

    #[test]
    fn parse_and_eval_i32_pow() {
        assert_eq!(eval_str("2^3"), Ok(int32(8)));
        assert_eq!(eval_str("3^2"), Ok(int32(9)));
        assert_eq!(eval_str("-4^2"), Ok(int32(-16)));
        assert_eq!(eval_str("(-4)^2"), Ok(int32(16)));
        assert_eq!(eval_str("2^3^2"), Ok(int32(512)));
    }

    #[test]
    fn parse_and_eval_str() {
        assert_eq!(eval_str("\"32\""), Ok(string("32")));
    }

    #[test]
    fn parse_and_eval_let() {
        assert_eq!(eval_str("x=3;x"), Ok(int32(3)));
    }

    #[test]
    fn parse_and_eval_let_with_args() {
        assert_eq!(eval_str("x(it)=it*2;x(3)"), Ok(int32(6)));
    }

    #[test]
    fn parse_and_eval_i32_type() {
        assert_eq!(eval_str("I32"), Ok(crate::primitives::i32_type()));
    }

    #[test]
    fn parse_and_eval_number_type() {
        assert_eq!(eval_str("Number"), Ok(crate::primitives::number_type()));
    }

    #[test]
    fn parse_and_eval_string_type() {
        assert_eq!(eval_str("String"), Ok(crate::primitives::string_type()));
    }

    #[test]
    fn parse_and_eval_string_or_number_type() {
        assert_eq!(
            eval_str("String | Number"),
            Ok(Union(set![number_type(), string_type()]))
        );
    }

    #[test]
    fn parse_and_eval_string_and_number_type() {
        assert_eq!(
            eval_str("String & Number"),
            Ok(Product(set![number_type(), string_type()]))
        );
    }

    #[test]
    fn parse_and_eval_tagged_string_or_number_type() {
        use crate::primitives::*;
        assert_eq!(
            eval_str("String + I32"),
            sum(vec![string_type(), i32_type()])
        );
    }

    #[test]
    fn parse_and_eval_string_times_number_type() {
        use crate::primitives::*;
        assert_eq!(
            eval_str("String * I32"),
            record(vec![string_type(), i32_type()])
        );
    }

    #[test]
    fn parse_and_eval_struct_x4_y5_access_x() {
        assert_eq!(eval_str("struct(x=4, y=5)(\"x\")"), Ok(int32(4)));
    }

    #[test]
    fn parse_and_eval_struct_x4_y5_access_y() {
        assert_eq!(eval_str("struct(x=4, y=\"Hi\")(\"y\")"), Ok(string("Hi")));
    }

    #[test]
    fn parse_and_eval_struct_x4_y5() {
        assert_eq!(
            eval_str("\"\"++struct(x=4, y=\"Hi\")"),
            Ok(string("(((it==\'x\')-|4)?((it==\'y\')-|\'Hi\'))")) // Ok(Str("struct(x=4, y=\"Hi\")".to_string()))
        );
    }

    #[test]
    fn parse_and_eval_struct_empty() {
        assert_eq!(
            eval_str("struct()"),
            Ok(Lambda(Box::new(Product(set![]).into_node())))
        );
    }

    #[test]
    fn parse_and_eval_print() {
        use crate::primitives::Prim::BuiltIn;
        use crate::primitives::Val::PrimVal;
        assert_eq!(eval_str("print"), Ok(PrimVal(BuiltIn("print".to_string()))));
    }

    #[test]
    fn tako_add_eq_rust_eq() {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        for _ in 0..100 {
            let num1: i32 = rng.gen();
            let num2: i32 = rng.gen();
            let res = num1.wrapping_add(num2);
            eprintln!("mul {:?} + {:?} = {:?}", num1, num2, res);
            assert_eq!(
                eval_str(&format!("mul(x, y)=x+y;mul(x= {}, y= {})", num1, num2)),
                Ok(int32(res))
            );
        }
    }

    #[test]
    fn tako_mul_eq_rust_eq() {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        for _ in 0..100 {
            let num1: i32 = rng.gen();
            let num2: i32 = rng.gen();
            let res = num1.wrapping_mul(num2);
            eprintln!("mul {:?} * {:?} = {:?}", num1, num2, res);
            assert_eq!(
                eval_str(&format!("mul(x, y)=x*y;mul(x= {}, y= {})", num1, num2)),
                Ok(int32(res))
            );
        }
    }
}
