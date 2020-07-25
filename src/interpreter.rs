use std::collections::HashMap;

use crate::ast::*;
use crate::database::Compiler;
use crate::errors::TError;

type Frame = HashMap<String, Node>;

// Walks the AST interpreting it.
pub struct Interpreter {
    pub debug: i32,
}

fn globals() -> Frame {
    map!(
        "println".to_string() => Node::BuiltIn("println".to_string())
    )
}

fn find_symbol<'a>(state: &'a [Frame], name: &str) -> Option<&'a Node> {
    for frame in state.iter().rev() {
        if let Some(val) = frame.get(name) {
            return Some(val); // This is the variable
        }
        // Not in this frame, go back up.
    }
    None
}

fn prim_add(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(I32(if *l { 1 } else { 0 } + if *r { 1 } else { 0 }, info)),
        (Bool(l, _), I32(r, _)) => Ok(I32(if *l { 1 } else { 0 } + r, info)),
        (Bool(l, _), Str(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (I32(l, _), Bool(r, _)) => Ok(I32(l + if *r { 1 } else { 0 }, info)),
        (I32(l, _), I32(r, _)) => Ok(I32(l + r, info)),
        (I32(l, _), Str(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (Str(l, _), Bool(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (Str(l, _), I32(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (Str(l, _), Str(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (l, r) => Err(TError::TypeMismatch2(
            "+".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
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
    use Prim::*;
    match (l, r) {
        (Bool(l, _), I32(r, _)) => Ok(I32(if *l { *r } else { 0 }, info)),
        (Bool(l, _), Str(r, _)) => Ok(Str(if *l { r.to_string() } else { "".to_string() }, info)),
        (I32(l, _), Bool(r, _)) => Ok(I32(if *r { *l } else { 0 }, info)),
        (I32(l, _), I32(r, _)) => Ok(I32(l * r, info)),
        (Str(l, _), Bool(r, _)) => Ok(Str(if *r { l.to_string() } else { "".to_string() }, info)),
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

fn prim_pow(l: &Prim, r: &Prim, info: Info) -> Res {
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
type Res = Result<Prim, TError>;
type State = Vec<Frame>;
impl Visitor<State, Prim, Prim> for Interpreter {
    fn new(db: &dyn Compiler) -> Interpreter {
        Interpreter { debug: db.options().debug }
    }

    fn visit_root(&mut self, root: &Root) -> Res {
        let mut state = vec![globals()];
        self.visit(&mut state, &root.ast)
    }

    fn visit_sym(&mut self, state: &mut State, expr: &Sym) -> Res {
        if self.debug > 0 {
            eprintln!("evaluating {}", expr.clone().to_node());
        }
        let name = &expr.name;
        let value = find_symbol(&state, name);
        match value {
            Some(Node::PrimNode(prim)) => {
                if self.debug > 0 {
                    eprintln!("from stack {}", prim.clone().to_node());
                }
                Ok(prim.clone())
            }
            Some(val) => {
                if self.debug > 0 {
                    eprintln!("running lambda {}", val);
                }
                let mut next = state.clone();
                let result = self.visit(&mut next, &val.clone())?;
                if self.debug > 0 {
                    eprintln!("got {}", result.clone().to_node());
                }
                Ok(result)
            } // This is the variable
            None => Err(TError::UnknownSymbol(name.to_string(), expr.info.clone())),
        }
    }

    fn visit_prim(&mut self, _state: &mut State, expr: &Prim) -> Res {
        Ok(expr.clone())
    }

    fn visit_apply(&mut self, state: &mut State, expr: &Apply) -> Res {
        state.push(Frame::new());
        for arg in expr.args.iter() {
            self.visit_let(state, arg)?;
        }
        // Visit the expr.inner
        let res = self.visit(state, &*expr.inner)?;
        state.pop();
        Ok(res)
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        if self.debug > 0 {
            eprintln!("evaluating let {}", expr.clone().to_node());
        }

        if expr.is_function {
            state
                .last_mut()
                .unwrap()
                .insert(expr.name.clone(), expr.value.clone().to_node());
            return Ok(Prim::Lambda(Box::new(expr.to_sym().to_node())));
        }
        // Add a new scope
        state.push(Frame::new());
        let result = self.visit(state, &expr.value)?;
        // Drop the finished scope
        state.pop();
        match state.last_mut() {
            None => panic!("there is no stack frame"),
            Some(frame) => {
                frame.insert(expr.name.clone(), result.clone().to_node());
                Ok(result)
            }
        }
    }

    fn visit_un_op(&mut self, state: &mut State, expr: &UnOp) -> Res {
        use Prim::*;
        if self.debug > 1 {
            eprintln!("evaluating unop {}", expr.clone().to_node());
        }
        let i = self.visit(state, &expr.inner)?;
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

    fn visit_bin_op(&mut self, state: &mut State, expr: &BinOp) -> Res {
        use Prim::*;
        if self.debug > 1 {
            eprintln!("evaluating binop {}", expr.clone().to_node());
        }
        let info = expr.clone().get_info();
        let l = self.visit(state, &expr.left);
        let mut r = || self.visit(state, &expr.right);
        match expr.name.as_str() {
            "+" => prim_add(&l?, &r()?, info),
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
            ";" => match (&l?, &r()?) {
                (_, r) => Ok(r.clone()),
            },
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
            ":" => match (&l?, &r()?) {
                (Bool(bool_val, inf), Str(ty, _)) => match ty.as_ref() {
                    "bool" => Ok(Bool(*bool_val, (*inf).clone())),
                    t => Err(TError::TypeMismatch(
                        t.to_string(),
                        Box::new(Bool(*bool_val, (*inf).clone())),
                        (*inf).clone(),
                    )),
                },
                (I32(int_val, inf), Str(ty, _)) => match ty.as_ref() {
                    "i32" => Ok(I32(*int_val, (*inf).clone())),
                    t => Err(TError::TypeMismatch(
                        t.to_string(),
                        Box::new(I32(*int_val, (*inf).clone())),
                        (*inf).clone(),
                    )),
                },
                (Str(str_val, inf), Str(ty, _)) => match ty.as_ref() {
                    "string" => Ok(Str((*str_val).clone(), (*inf).clone())),
                    t => Err(TError::TypeMismatch(
                        t.to_string(),
                        Box::new(Str((*str_val).clone(), (*inf).clone())),
                        (*inf).clone(),
                    )),
                },
                (_, t) => Err(TError::TypeMismatch(
                    "type".to_string(),
                    Box::new(t.clone()),
                    t.get_info(),
                )),
            },
            op => Err(TError::UnknownInfixOperator(op.to_string(), info)),
        }
    }

    fn visit_built_in(&mut self, state: &mut State, expr: &String) -> Res {
        let it_val = state.last().map_or_else(
            || Prim::Str("".to_string(), Info::default()).to_node(),
            |frame| frame.get("it").expect("println needs an argument").clone(),
        );
        match &expr[..] {
            "println" => match it_val {
                Node::PrimNode(Prim::Str(it_val, _)) => println!("{}", it_val),
                it_val => println!("{}", it_val),
            },
            _ => unimplemented!("interpreter built in"),
        }
        Ok(Prim::I32(0, Info::default()))
    }

    fn handle_error(&mut self, _state: &mut State, expr: &Err) -> Res {
        Err(TError::FailedParse(expr.msg.to_string(), expr.get_info()))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::parser::{lex, parse};
    use crate::database::{DB, Compiler};
    use crate::cli_options::Options;
    use super::Interpreter;
    use super::Res;
    use Node::*;
    use Prim::*;

    #[test]
    fn eval_num() {
        let mut db = DB::default();
        db.set_options(Options::default());
        let mut interp = Interpreter::new(&db);
        let tree = Root::new(PrimNode(I32(12, Info::default())));
        assert_eq!(interp.visit_root(&tree), Ok(I32(12, Info::default())));
    }

    fn eval_str(s: String) -> Res {
        use std::sync::Arc;
        let ast = parse(lex(None, Arc::new(s)));
        let mut db = DB::default();
        db.set_options(Options::default());
        let mut interp = Interpreter::new(&db);
        interp.visit_root(&Root::new(ast))
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

    #[quickcheck]
    fn tako_add_eq_rust_eq(num1: i32, num2: i32) -> bool {
        let res = num1 + num2;
        eprintln!("mul {:?} + {:?} = {:?}", num1, num2, res);
        eval_str(format!("mul(x, y)=x+y;mul(x= {}, y= {})", num1, num2))
            == Ok(I32(res, Info::default()))
    }

    #[quickcheck]
    fn tako_mul_eq_rust_eq(num1: i32, num2: i32) -> bool {
        let res = num1 * num2;
        eprintln!("mul {:?} * {:?} = {:?}", num1, num2, res);
        eval_str(format!("mul(x, y)=x*y;mul(x= {}, y= {})", num1, num2))
            == Ok(I32(res, Info::default()))
    }
}
