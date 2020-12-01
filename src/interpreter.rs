use std::collections::HashMap;

use super::ast::*;
use super::database::Compiler;
use super::errors::TError;
use super::type_checker::infer;

type Frame = HashMap<String, Prim>;

pub type ImplFn<'a> = &'a mut dyn FnMut(&dyn Compiler, HashMap<String, Box<dyn Fn() -> Res>>, Info) -> Res;
pub type PureImplFn<'a> = &'a dyn Fn(&dyn Compiler, HashMap<String, Box<dyn Fn() -> Res>>, Info) -> Res;

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

fn find_symbol<'a>(state: &'a [Frame], name: &str) -> Option<&'a Prim> {
    for frame in state.iter().rev() {
        if let Some(val) = frame.get(name) {
            return Some(val); // This is the variable
        }
        // Not in this frame, go back up.
    }
    None
}

fn prim_add(l: &Prim, r: &Prim, info: Info) -> Res {
    use crate::types::sum;
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(I32(if *l { 1 } else { 0 } + if *r { 1 } else { 0 }, info)),
        (Bool(l, _), I32(r, _)) => Ok(I32(r.wrapping_add(if *l { 1 } else { 0 }), info)),
        (Bool(l, _), Str(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (I32(l, _), Bool(r, _)) => Ok(I32(l.wrapping_add(if *r { 1 } else { 0 }), info)),
        (I32(l, _), I32(r, _)) => Ok(I32(l.wrapping_add(*r), info)),
        (I32(l, _), Str(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (Str(l, _), Bool(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (Str(l, _), I32(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (Str(l, _), Str(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (TypeValue(l, _), TypeValue(r, _)) => Ok(TypeValue(sum(vec![l.clone(), r.clone()])?, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "+".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

pub fn prim_add_strs(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    let to_str = |val: &Prim| match val {
        Void(_) => "Void".to_string(),
        Unit(_) => "()".to_string(),
        Bool(v, _) => format!("{}", v),
        I32(v, _) => format!("{}", v),
        Str(v, _) => v.clone(),
        Struct(v, info) => format!("{:?}", Struct(v.to_vec(), info.clone())),
        Lambda(v) => format!("{}", v),
        TypeValue(v, _) => format!("{}", v),
    };
    Ok(Str(to_str(l) + &to_str(r), info))
}

fn prim_eq(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(Bool(*l == *r, info)),
        (I32(l, _), I32(r, _)) => Ok(Bool(l == r, info)),
        (Str(l, _), Str(r, _)) => Ok(Bool(l == r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "==".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_neq(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(Bool(*l != *r, info)),
        (I32(l, _), I32(r, _)) => Ok(Bool(l != r, info)),
        (Str(l, _), Str(r, _)) => Ok(Bool(l != r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "!=".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_gt(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(Bool(*l & !(*r), info)),
        (I32(l, _), I32(r, _)) => Ok(Bool(l > r, info)),
        (Str(l, _), Str(r, _)) => Ok(Bool(l > r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            ">".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_gte(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(Bool(*l >= *r, info)),
        (I32(l, _), I32(r, _)) => Ok(Bool(l >= r, info)),
        (Str(l, _), Str(r, _)) => Ok(Bool(l >= r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            ">=".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_sub(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (I32(l, _), Bool(r, _)) => Ok(I32(l - if *r { 1 } else { 0 }, info)),
        (I32(l, _), I32(r, _)) => Ok(I32(l - r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "-".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_mul(l: &Prim, r: &Prim, info: Info) -> Res {
    use crate::types::record;
    use Prim::*;
    match (l, r) {
        (Bool(l, _), I32(r, _)) => Ok(I32(if *l { *r } else { 0 }, info)),
        (Bool(l, _), Str(r, _)) => Ok(Str(if *l { r.to_string() } else { "".to_string() }, info)),
        (I32(l, _), Bool(r, _)) => Ok(I32(if *r { *l } else { 0 }, info)),
        (I32(l, _), I32(r, _)) => Ok(I32(l.wrapping_mul(*r), info)),
        (Str(l, _), Bool(r, _)) => Ok(Str(if *r { l.to_string() } else { "".to_string() }, info)),
        (TypeValue(l, _), TypeValue(r, _)) => {
            Ok(TypeValue(record(vec![l.clone(), r.clone()])?, info))
        }
        (l, r) => Err(TError::TypeMismatch2(
            "*".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_div(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (I32(l, _), I32(r, _)) => Ok(I32(l / r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "/".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_mod(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (I32(l, _), I32(r, _)) => Ok(I32(l % r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "%".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_and(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(Bool(*l && *r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "&&".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_or(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(Bool(*l || *r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "||".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

fn prim_type_and(l: Prim, r: Prim, info: Info) -> Res {
    use crate::types::Type;
    use Prim::*;
    match (l, r) {
        (TypeValue(l, _), TypeValue(r, _)) => Ok(TypeValue(Type::Product(set!(l, r)), info)),
        (l, r) => Err(TError::TypeMismatch2(
            "&".to_string(),
            Box::new(l),
            Box::new(r),
            info,
        )),
    }
}

fn prim_type_or(l: Prim, r: Prim, info: Info) -> Res {
    use crate::types::Type;
    use Prim::*;
    match (l, r) {
        (TypeValue(l, _), TypeValue(r, _)) => Ok(TypeValue(Type::Union(set!(l, r)), info)),
        (l, r) => Err(TError::TypeMismatch2(
            "|".to_string(),
            Box::new(l),
            Box::new(r),
            info,
        )),
    }
}

pub fn prim_pow(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (I32(l, _), Bool(r, _)) => Ok(I32(if *r { *l } else { 1 }, info)),
        (I32(l, _), I32(r, _)) => Ok(I32(i32::pow(*l, *r as u32), info)), // TODO: require pos pow
        (l, r) => Err(TError::TypeMismatch2(
            "^".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

// TODO: Return nodes.
pub type Res = Result<Prim, TError>;
type State = Vec<Frame>;
impl<'a> Visitor<State, Prim, Prim> for Interpreter<'a> {
    fn visit_root(&mut self, db: &dyn Compiler, root: &Root) -> Res {
        let mut state = vec![HashMap::new()];
        self.visit(db, &mut state, &root.ast)
    }

    fn visit_sym(&mut self, db: &dyn Compiler, state: &mut State, expr: &Sym) -> Res {
        if db.debug() > 0 {
            eprintln!("evaluating let {}", expr.clone().to_node());
        }
        let name = &expr.name;
        let frame = || {
            let mut frame_vals: HashMap<String, Box<dyn Fn() -> Res>> = map!();
            for (name, val) in state.last().unwrap().clone().iter() {
                let val = val.clone();
                frame_vals.insert(name.to_string(), Box::new(move ||Ok(val.clone())));
            }
            return frame_vals;
        };
        let value = find_symbol(&state, name);
        if let Some(prim) = value {
            if db.debug() > 0 {
                eprintln!("from stack {}", prim.clone().to_node());
            }
            return Ok(prim.clone());
        }
        if db.debug() > 2 {
            eprintln!("checking for interpreter impl {}", expr.name.clone());
        }
        if let Some(extern_impl) = &mut self.impls.get_mut(name) {
            return extern_impl(db, frame(), expr.get_info());
        }
        if db.debug() > 2 {
            eprintln!("checking for default impl {}", expr.name.clone());
        }
        if let Some(default_impl) = crate::externs::get_implementation(name.to_owned()) {
            return default_impl(db, frame(), expr.get_info());
        }
        dbg!(state);
        Err(TError::UnknownSymbol(
            name.to_string(),
            expr.info.clone(),
            "interpreter::?".to_string(),
        ))
    }

    fn visit_prim(&mut self, _db: &dyn Compiler, _state: &mut State, expr: &Prim) -> Res {
        Ok(expr.clone())
    }

    fn visit_apply(&mut self, db: &dyn Compiler, state: &mut State, expr: &Apply) -> Res {
        if db.debug() > 0 {
            eprintln!("evaluating apply {}", expr.clone().to_node());
        }
        state.push(Frame::new());
        expr.args
            .iter()
            .map(|arg| self.visit_let(db, state, arg))
            .collect::<Result<Vec<Prim>, TError>>()?;
        // Retrive the inner
        let inner = self.visit(db, state, &*expr.inner)?;
        // Run the inner
        let res = match inner {
            Prim::Lambda(func) => self.visit(db, state, &*func)?,
            val => val,
        };
        state.pop();
        Ok(res)
    }

    fn visit_let(&mut self, db: &dyn Compiler, state: &mut State, expr: &Let) -> Res {
        if db.debug() > 0 {
            eprintln!("evaluating let {}", expr.clone().to_node());
        }

        if expr.args.is_some() {
            let val = Prim::Lambda(expr.value.clone());
            state
                .last_mut()
                .unwrap()
                .insert(expr.name.clone(), val.clone());
            return Ok(val);
        }
        // Add a new scope
        state.push(Frame::new());
        let result = self.visit(db, state, &expr.value)?;
        // Drop the finished scope
        state.pop();
        match state.last_mut() {
            None => panic!("there is no stack frame"),
            Some(frame) => {
                frame.insert(expr.name.clone(), result.clone());
                Ok(result)
            }
        }
    }

    fn visit_un_op(&mut self, db: &dyn Compiler, state: &mut State, expr: &UnOp) -> Res {
        use Prim::*;
        if db.debug() > 1 {
            eprintln!("evaluating unop {}", expr.clone().to_node());
        }
        let i = self.visit(db, state, &expr.inner)?;
        let info = expr.clone().get_info();
        match expr.name.as_str() {
            "!" => match i {
                Bool(n, _) => Ok(Bool(!n, info)),
                Lambda(_) => Ok(Lambda(Box::new(expr.clone().to_node()))),
                _ => Err(TError::TypeMismatch("!".to_string(), Box::new(i), info)),
            },
            "+" => match i {
                I32(n, _) => Ok(I32(n, info)),
                Lambda(_) => Ok(Lambda(Box::new(expr.clone().to_node()))),
                _ => Err(TError::TypeMismatch("+".to_string(), Box::new(i), info)),
            },
            "-" => match i {
                I32(n, _) => Ok(I32(-n, info)),
                Lambda(_) => Ok(Lambda(Box::new(expr.clone().to_node()))),
                _ => Err(TError::TypeMismatch("-".to_string(), Box::new(i), info)),
            },
            op => Err(TError::UnknownPrefixOperator(op.to_string(), info)),
        }
    }

    fn visit_bin_op(&mut self, db: &dyn Compiler, state: &mut State, expr: &BinOp) -> Res {
        use Prim::*;
        if db.debug() > 1 {
            eprintln!("evaluating binop {}", expr.clone().to_node());
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
            "&" => prim_type_and(l?, r()?, info),
            "|" => prim_type_or(l?, r()?, info),
            "," => {
                let left = l?;
                let right = r()?;
                dbg!(format!("{},{}", &left, &right));
                Ok(left.merge(right))
            }
            ";" => {
                let left = l?;
                let right = r()?;
                dbg!(format!("{};{}", &left, &right));
                Ok(left.merge(right))
            }
            "?" => match l {
                Err(_) => r(),
                l => l,
            },
            "-|" => match l {
                //TODO: Add pattern matching.
                Ok(Bool(false, info)) => Err(TError::RequirementFailure(info)),
                Ok(Lambda(_)) => Ok(Lambda(Box::new(expr.clone().to_node()))),
                Ok(_) => r(),
                l => l,
            },
            ":" => {
                let value = l?;
                let ty = r()?;
                let _type_of_value = infer(db, &value.clone().to_node());
                // Check subtyping relationship of type_of_value and ty.
                let sub_type = true;
                if sub_type {
                    return Ok(value);
                }
                Err(TError::TypeMismatch2(
                    "Failure assertion of type annotation at runtime".to_string(),
                    Box::new(value),
                    Box::new(ty.clone()),
                    ty.get_info(),
                ))
            }
            op => Err(TError::UnknownInfixOperator(op.to_string(), info)),
        }
    }

    fn handle_error(&mut self, _db: &dyn Compiler, _state: &mut State, expr: &Err) -> Res {
        Err(TError::FailedParse(expr.msg.to_string(), expr.get_info()))
    }
}

#[cfg(test)]
mod tests {
    use super::super::ast::*;
    use super::super::cli_options::Options;
    use super::super::database::{Compiler, DB};
    use super::{Interpreter, Res};
    use std::collections::HashMap;
    use Node::*;
    use Prim::*;

    #[test]
    fn eval_num() {
        let mut db = DB::default();
        db.set_options(Options::default());
        let tree = PrimNode(I32(12, Info::default()));
        assert_eq!(
            Interpreter::default().visit(&db, &mut vec![], &tree),
            Ok(I32(12, Info::default()))
        );
    }

    fn eval_str(s: String) -> Res {
        use std::sync::Arc;
        let mut db = DB::default();
        let filename = "test/file.tk";
        let module = db.module_name(filename.to_owned());
        db.set_file(filename.to_owned(), Ok(Arc::new(s)));
        db.set_options(Options::default());
        let ast = db.parse_file(module)?;
        dbg!(format!("{}", &ast));
        let mut state = vec![HashMap::new()];
        Interpreter::default().visit(&db, &mut state, &ast)
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
        assert_eq!(
            eval_str("true".to_string()),
            Ok(Bool(true, Info::default()))
        );
    }

    #[test]
    fn parse_and_eval_bool_and() {
        assert_eq!(
            eval_str("true&&true".to_string()),
            Ok(Bool(true, Info::default()))
        );
        assert_eq!(
            eval_str("false&&true".to_string()),
            Ok(Bool(false, Info::default()))
        );
        assert_eq!(
            eval_str("true&&false".to_string()),
            Ok(Bool(false, Info::default()))
        );
        assert_eq!(
            eval_str("false&&false".to_string()),
            Ok(Bool(false, Info::default()))
        );
    }

    #[test]
    fn parse_and_eval_bool_or() {
        assert_eq!(
            eval_str("true||true".to_string()),
            Ok(Bool(true, Info::default()))
        );
        assert_eq!(
            eval_str("false||true".to_string()),
            Ok(Bool(true, Info::default()))
        );
        assert_eq!(
            eval_str("true||false".to_string()),
            Ok(Bool(true, Info::default()))
        );
        assert_eq!(
            eval_str("false||false".to_string()),
            Ok(Bool(false, Info::default()))
        );
    }

    #[test]
    fn parse_and_eval_bool_eq() {
        assert_eq!(
            eval_str("true==true".to_string()),
            Ok(Bool(true, Info::default()))
        );
        assert_eq!(
            eval_str("false==true".to_string()),
            Ok(Bool(false, Info::default()))
        );
        assert_eq!(
            eval_str("true==false".to_string()),
            Ok(Bool(false, Info::default()))
        );
        assert_eq!(
            eval_str("false==false".to_string()),
            Ok(Bool(true, Info::default()))
        );
    }

    #[test]
    fn parse_and_eval_i32() {
        assert_eq!(eval_str("32".to_string()), Ok(I32(32, Info::default())));
    }

    #[test]
    fn parse_and_eval_i32_eq() {
        assert_eq!(
            eval_str("0==0".to_string()),
            Ok(Bool(true, Info::default()))
        );
        assert_eq!(
            eval_str("-1==1".to_string()),
            Ok(Bool(false, Info::default()))
        );
        assert_eq!(
            eval_str("1==123".to_string()),
            Ok(Bool(false, Info::default()))
        );
        assert_eq!(
            eval_str("1302==1302".to_string()),
            Ok(Bool(true, Info::default()))
        );
    }

    #[test]
    fn parse_and_eval_i32_pow() {
        assert_eq!(eval_str("2^3".to_string()), Ok(I32(8, Info::default())));
        assert_eq!(eval_str("3^2".to_string()), Ok(I32(9, Info::default())));
        assert_eq!(eval_str("-4^2".to_string()), Ok(I32(-16, Info::default())));
        assert_eq!(eval_str("(-4)^2".to_string()), Ok(I32(16, Info::default())));
        assert_eq!(eval_str("2^3^2".to_string()), Ok(I32(512, Info::default())));
    }

    #[test]
    fn parse_and_eval_str() {
        assert_eq!(
            eval_str("\"32\"".to_string()),
            Ok(Str("32".to_string(), Info::default()))
        );
    }

    #[test]
    fn parse_and_eval_let() {
        assert_eq!(eval_str("x=3;x".to_string()), Ok(I32(3, Info::default())));
    }

    #[test]
    fn parse_and_eval_let_with_args() {
        assert_eq!(
            eval_str("x(it)=it*2;x(3)".to_string()),
            Ok(I32(6, Info::default()))
        );
    }

    #[test]
    fn parse_and_eval_i32_type() {
        assert_eq!(
            eval_str("I32".to_string()),
            Ok(TypeValue(crate::types::i32_type(), Info::default()))
        );
    }

    #[test]
    fn parse_and_eval_number_type() {
        assert_eq!(
            eval_str("Number".to_string()),
            Ok(TypeValue(crate::types::number_type(), Info::default()))
        );
    }

    #[test]
    fn parse_and_eval_string_type() {
        assert_eq!(
            eval_str("String".to_string()),
            Ok(TypeValue(crate::types::string_type(), Info::default()))
        );
    }

    #[test]
    fn parse_and_eval_string_or_number_type() {
        use crate::types::{Type::*, *};
        assert_eq!(
            eval_str("String | Number".to_string()),
            Ok(TypeValue(
                Union(set![number_type(), string_type()]),
                Info::default()
            ))
        );
    }

    #[test]
    fn parse_and_eval_string_and_number_type() {
        use crate::types::{Type::*, *};
        assert_eq!(
            eval_str("String & Number".to_string()),
            Ok(TypeValue(
                Product(set![number_type(), string_type()]),
                Info::default()
            ))
        );
    }

    #[test]
    fn parse_and_eval_tagged_string_or_number_type() {
        use crate::types::*;
        assert_eq!(
            eval_str("String + I32".to_string()),
            Ok(TypeValue(
                sum(vec![string_type(), i32_type()]).unwrap(),
                Info::default()
            ))
        );
    }

    #[test]
    fn parse_and_eval_string_times_number_type() {
        use crate::types::*;
        assert_eq!(
            eval_str("String * I32".to_string()),
            Ok(TypeValue(
                record(vec![string_type(), i32_type()]).unwrap(),
                Info::default()
            ))
        );
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
                eval_str(format!("mul(x, y)=x+y;mul(x= {}, y= {})", num1, num2)),
                Ok(I32(res, Info::default()))
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
                eval_str(format!("mul(x, y)=x*y;mul(x= {}, y= {})", num1, num2)),
                Ok(I32(res, Info::default()))
            );
        }
    }
}
