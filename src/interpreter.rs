use super::ast::*;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum InterpreterError {
    UnknownInfixOperator(String, Info),
    UnknownPrefixOperator(String, Info),
    UnknownSymbol(String, Info),
    FailedParse(String, Info),
    TypeMismatch(String, Prim, Info),
    TypeMismatch2(String, Prim, Prim, Info),
    RequirementFailure(Info),
}

type Frame = HashMap<String, Node>;

// Walks the AST interpreting it.
pub struct Interpreter {
    pub debug: i32,
}

impl Default for Interpreter {
    fn default() -> Interpreter {
        Interpreter { debug: 0 }
    }
}

fn globals() -> Frame {
    use Node::*;
    use Prim::*;
    map! {
        "true".to_string() => PrimNode(Bool(true, Info::default())),
        "false".to_string() => PrimNode(Bool(false, Info::default()))
    }
}

fn find_symbol<'a>(state: &'a State, name: &str) -> Option<&'a Node> {
    for frame in state.iter().rev() {
        if let Some(val) = frame.get(name) {
            return Some(val) // This is the variable
        }
        // Not in this frame, go back up.
    }
    None
}

fn prim_add(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => {
            Ok(I32(if *l { 1 } else { 0 } + if *r { 1 } else { 0 }, info))
        }
        (Bool(l, _), I32(r, _)) => Ok(I32(if *l { 1 } else { 0 } + r, info)),
        (Bool(l, _), Str(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (I32(l, _), Bool(r, _)) => Ok(I32(l + if *r { 1 } else { 0 }, info)),
        (I32(l, _), I32(r, _)) => Ok(I32(l + r, info)),
        (I32(l, _), Str(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (Str(l, _), Bool(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (Str(l, _), I32(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (Str(l, _), Str(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (l, r) => Err(InterpreterError::TypeMismatch2(
            "+".to_string(),
            (*l).clone(),
            (*r).clone(),
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
        (l, r) => Err(InterpreterError::TypeMismatch2(
            "==".to_string(),
            (*l).clone(),
            (*r).clone(),
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
        (l, r) => Err(InterpreterError::TypeMismatch2(
            "!=".to_string(),
            (*l).clone(),
            (*r).clone(),
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
        (l, r) => Err(InterpreterError::TypeMismatch2(
            ">".to_string(),
            (*l).clone(),
            (*r).clone(),
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
        (l, r) => Err(InterpreterError::TypeMismatch2(
            ">=".to_string(),
            (*l).clone(),
            (*r).clone(),
            info,
        )),
    }
}

fn prim_sub(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (I32(l, _), Bool(r, _)) => Ok(I32(l - if *r { 1 } else { 0 }, info)),
        (I32(l, _), I32(r, _)) => Ok(I32(l - r, info)),
        (l, r) => Err(InterpreterError::TypeMismatch2(
            "-".to_string(),
            (*l).clone(),
            (*r).clone(),
            info,
        )),
    }
}

fn prim_mul(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), I32(r, _)) => Ok(I32(if *l { *r } else { 0 }, info)),
        (Bool(l, _), Str(r, _)) => {
            Ok(Str(if *l { r.to_string() } else { "".to_string() }, info))
        }
        (I32(l, _), Bool(r, _)) => Ok(I32(if *r { *l } else { 0 }, info)),
        (I32(l, _), I32(r, _)) => Ok(I32(l * r, info)),
        (Str(l, _), Bool(r, _)) => {
            Ok(Str(if *r { l.to_string() } else { "".to_string() }, info))
        }
        (l, r) => Err(InterpreterError::TypeMismatch2(
            "*".to_string(),
            (*l).clone(),
            (*r).clone(),
            info,
        ))
    }
}

fn prim_div(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (I32(l, _), I32(r, _)) => Ok(I32(l / r, info)),
        (l, r) => Err(InterpreterError::TypeMismatch2(
            "/".to_string(),
            (*l).clone(),
            (*r).clone(),
            info,
        )),
    }
}

fn prim_mod(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (I32(l, _), I32(r, _)) => Ok(I32(l % r, info)),
        (l, r) => Err(InterpreterError::TypeMismatch2(
            "%".to_string(),
            (*l).clone(),
            (*r).clone(),
            info,
        )),
    }
}

fn prim_and(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(Bool(*l && *r, info)),
        (l, r) => Err(InterpreterError::TypeMismatch2(
            "&&".to_string(),
            (*l).clone(),
            (*r).clone(),
            info,
        )),
    }
}

fn prim_or(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(Bool(*l || *r, info)),
        (l, r) => Err(InterpreterError::TypeMismatch2(
            "||".to_string(),
            (*l).clone(),
            (*r).clone(),
            info,
        )),
    }
}

fn prim_pow(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (I32(l, _), Bool(r, _)) => Ok(I32(if *r { *l } else { 1 }, info)),
        (I32(l, _), I32(r, _)) => {
            Ok(I32(i32::pow(*l, *r as u32), info))
        } // TODO: require pos pow
        (l, r) => Err(InterpreterError::TypeMismatch2(
            "^".to_string(),
            (*l).clone(),
            (*r).clone(),
            info,
        )),
    }
}

// TODO: Return nodes.
type Res = Result<Prim, InterpreterError>;
type State = Vec<Frame>;
impl Visitor<State, Prim, Prim, InterpreterError> for Interpreter {
    fn visit_root(&mut self, root: &Root) -> Res {
        let mut state = vec![globals()];
        self.visit(&mut state, &root.ast)
    }

    fn visit_sym(&mut self, state: &mut State, expr: &Sym) -> Res {
        if self.debug > 0 {
            println!("evaluating {}", expr.clone().to_node());
        }
        let name = &expr.name;
        let value = find_symbol(&state, name);
        match value {
            Some(Node::PrimNode(prim)) => {
                if self.debug > 0 {
                    println!("from stack {}", prim.clone().to_node());
                }
                return Ok(prim.clone());
            }
            Some(val) => {
                if self.debug > 0 {
                    println!("running lambda {}", val);
                }
                let mut next = state.clone();
                let result = self.visit(&mut next, &val.clone())?;
                if self.debug > 0 {
                    println!("got {}", result.clone().to_node());
                }
                return Ok(result);
            } // This is the variable
            None => {
                return Err(InterpreterError::UnknownSymbol(name.to_string(), expr.info.clone()));
            }
        }
    }

    fn visit_prim(&mut self, _state: &mut State, expr: &Prim) -> Res {
        Ok(expr.clone())
    }

    fn visit_apply(&mut self, state: &mut State, expr: &Apply) -> Res {
        for arg in expr.args.iter() {
            self.visit_let(state, arg)?;
        }
        // Visit the expr.inner
        let res = self.visit(state, &*expr.inner)?;
        match res {
            Prim::Lambda(lambda) => self.visit(state, &lambda),
            _ => Ok(res),
        }
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        if self.debug > 0 {
            println!("evaluating let {}", expr.clone().to_node());
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
        if self.debug > 1 {
            println!("evaluating unop {}", expr.clone().to_node());
        }
        use Prim::*;
        let i = self.visit(state, &expr.inner)?;
        let info = expr.clone().get_info();
        match expr.name.as_str() {
            "!" => match i {
                Bool(n, _) => Ok(Bool(!n, info)),
                Lambda(_) => Ok(Lambda(Box::new(expr.clone().to_node()))),
                _ => Err(InterpreterError::TypeMismatch("!".to_string(), i, info)),
            },
            "+" => match i {
                I32(n, _) => Ok(I32(n, info)),
                Lambda(_) => Ok(Lambda(Box::new(expr.clone().to_node()))),
                _ => Err(InterpreterError::TypeMismatch("+".to_string(), i, info)),
            },
            "-" => match i {
                I32(n, _) => Ok(I32(-n, info)),
                Lambda(_) => Ok(Lambda(Box::new(expr.clone().to_node()))),
                _ => Err(InterpreterError::TypeMismatch("-".to_string(), i, info)),
            },
            op => Err(InterpreterError::UnknownPrefixOperator(
                op.to_string(),
                info,
            )),
        }
    }

    fn visit_bin_op(&mut self, state: &mut State, expr: &BinOp) -> Res {
        if self.debug > 1 {
            println!("evaluating binop {}", expr.clone().to_node());
        }
        use Prim::*;
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
                Ok(Bool(false, info)) => Err(InterpreterError::RequirementFailure(info)),
                Ok(Lambda(_)) => Ok(Lambda(Box::new(expr.clone().to_node()))),
                Ok(_) => r(),
                l => l,
            },
            ":" => match (&l?, &r()?) {
                (Bool(bool_val, inf), Str(ty, _)) => match ty.as_ref() {
                    "bool" => Ok(Bool(*bool_val, (*inf).clone())),
                    t => Err(InterpreterError::TypeMismatch(
                        t.to_string(),
                        Bool(*bool_val, (*inf).clone()),
                        (*inf).clone(),
                    )),
                },
                (I32(int_val, inf), Str(ty, _)) => match ty.as_ref() {
                    "i32" => Ok(I32(*int_val, (*inf).clone())),
                    t => Err(InterpreterError::TypeMismatch(
                        t.to_string(),
                        I32(*int_val, (*inf).clone()),
                        (*inf).clone(),
                    )),
                },
                (Str(str_val, inf), Str(ty, _)) => match ty.as_ref() {
                    "string" => Ok(Str((*str_val).clone(), (*inf).clone())),
                    t => Err(InterpreterError::TypeMismatch(
                        t.to_string(),
                        Str((*str_val).clone(), (*inf).clone()),
                        (*inf).clone(),
                    )),
                },
                (_, t) => Err(InterpreterError::TypeMismatch(
                    "type".to_string(),
                    t.clone(),
                    t.get_info(),
                )),
            },
            op => Err(InterpreterError::UnknownInfixOperator(op.to_string(), info)),
        }
    }

    fn handle_error(&mut self, _state: &mut State, expr: &Err) -> Res {
        Err(InterpreterError::FailedParse(
            expr.msg.to_string(),
            expr.get_info(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::super::ast::*;
    use super::super::parser;
    use super::Interpreter;
    use super::Res;
    use Node::*;
    use Prim::*;

    #[test]
    fn eval_num() {
        let mut interp = Interpreter::default();
        let tree = PrimNode(I32(12, Info::default())).to_root();
        assert_eq!(interp.visit_root(&tree), Ok(I32(12, Info::default())));
    }

    fn eval_str(s: String) -> Res {
        let ast = parser::parse_file("test".to_string(), s);
        let mut interp = Interpreter::default();
        interp.visit_root(&ast)
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
}
