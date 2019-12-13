use super::ast::*;
use super::tree::*;

use std::cmp;

#[derive(Debug)]
#[derive(PartialEq)]
pub enum InterpreterError {
    UnknownInfixOperator(String),
    UnknownPrefixOperator(String),
    FailedParse(String),
    TypeMismatch(String, PrimValue),
    TypeMismatch2(String, PrimValue, PrimValue),
}

pub struct Frame {

}

// Walks the AST interpreting it.
pub struct Interpreter {
    scope: Tree<LetNode>,
    stack: Vec<Frame>,
}

impl Interpreter {
    pub fn bind(&mut self, path: Vec<String>, binding: LetNode) -> Result<(), String> {
        Interpreter::bind_to(&mut self.scope, path, binding)
    }
    fn bind_to(curr: &mut Tree<LetNode>, path: Vec<String>, binding: LetNode) -> Result<(), String> {
        match path.split_at(cmp::min(1, path.len())) {
            ([], _) => {
                curr.value = binding;
            },
            ([name], rest) => {
                for child in &mut curr.children {
                    if child.value.name == *name {
                        return Interpreter::bind_to(child, rest.to_vec(), binding);
                    }
                }
                let mut new = Tree {
                    value: LetNode{name: name.to_string(), value: None},
                    children: vec![],
                };
                Interpreter::bind_to(&mut new, rest.to_vec(), binding)?;
                curr.children.push(new);
            }
            _ => panic!("unexpectedly long head of vector"),
        }
        return Ok(());
    }
    pub fn lookup(&self, path: Vec<String>) -> Option<LetNode> {
        let mut curr = &self.scope;

        'name: for name in path {
            for child in &curr.children {
                if child.value.name == name {
                    curr = child;
                    continue 'name;
                }
            }
            return None;
        }
        return Some((*curr).value.clone());
    }
}

impl Default for Interpreter {
    fn default() -> Interpreter {
        Interpreter {
            scope: Tree {
                value: LetNode{name: "".to_string(), value: None },
                children: vec![],
            },
            stack: vec![],
        }
    }
}

type Res = Result<PrimValue, InterpreterError>;
impl Visitor<PrimValue, PrimValue, InterpreterError> for Interpreter {

    fn visit_root(&mut self, expr: &Node) -> Res {
        self.visit(expr)
    }

    fn visit_call(&mut self, expr: &CallNode) -> Res {
        use PrimValue::*;
        match expr.name.as_str() {
            "true" => return Ok(Bool(true)),
            "false" => return Ok(Bool(false)),
            _ => panic!("Call not implemented in interpreter"),
        }
    }

    fn visit_prim(&mut self, expr: &PrimValue) -> Res {
        Ok(expr.clone())
    }

    fn visit_let(&mut self, expr: &LetNode) -> Res {
        panic!("Let not implemented in interpreter");
    }

    fn visit_un_op(&mut self, expr: &UnOpNode) -> Res {
        use PrimValue::*;
        let i = self.visit(&expr.inner)?;
        match expr.name.as_str() {
            "!" => match i {
                Bool(n) => Ok(Bool(!n)),
                _ => Err(InterpreterError::TypeMismatch("!".to_string(), i))
            },
            "+" => match i {
                I32(n) => Ok(I32(n)),
                _ => Err(InterpreterError::TypeMismatch("+".to_string(), i))

            }
            "-" => match i {
                I32(n) => Ok(I32(-n)),
                _ => Err(InterpreterError::TypeMismatch("-".to_string(), i))
            },
            op => Err(InterpreterError::UnknownPrefixOperator(op.to_string())),
        }
    }

    fn visit_bin_op(&mut self, expr: &BinOpNode) -> Res {
        use PrimValue::*;
        let l = self.visit(&expr.left)?;
        let r = self.visit(&expr.right)?;
        match expr.name.as_str() {
            "+" => match (&l, &r) {
                (Bool(l), Bool(r)) => Ok(I32(if *l {1} else {0} + if *r {1} else {0})),
                (Bool(l), I32(r)) => Ok(I32(if *l {1} else {0} + r)),
                (Bool(l), Str(r)) => Ok(Str(l.to_string() + &r.to_string())),
                (I32(l), Bool(r)) => Ok(I32(l + if *r {1} else {0})),
                (I32(l), I32(r)) => Ok(I32(l + r)),
                (I32(l), Str(r)) => Ok(Str(l.to_string() + &r.to_string())),
                (Str(l), Bool(r)) => Ok(Str(l.to_string() + &r.to_string())),
                (Str(l), I32(r)) => Ok(Str(l.to_string() + &r.to_string())),
                (Str(l), Str(r)) => Ok(Str(l.to_string() + &r.to_string())),
            },
            "==" => match (&l, &r) {
                (Bool(l), Bool(r)) => Ok(Bool(*l == *r)),
                (I32(l), I32(r)) => Ok(Bool(l == r)),
                (Str(l), Str(r)) => Ok(Bool(l.to_string() == r.to_string())),
                _ => Err(InterpreterError::TypeMismatch2("==".to_string(), l, r))
            },
            "!=" => match (&l, &r) {
                (Bool(l), Bool(r)) => Ok(Bool(*l != *r)),
                (I32(l), I32(r)) => Ok(Bool(l != r)),
                (Str(l), Str(r)) => Ok(Bool(l.to_string() != r.to_string())),
                _ => Err(InterpreterError::TypeMismatch2("!=".to_string(), l, r))
            },
            ">" => match (&l, &r) {
                (Bool(l), Bool(r)) => Ok(Bool(*l > *r)),
                (I32(l), I32(r)) => Ok(Bool(l > r)),
                (Str(l), Str(r)) => Ok(Bool(l.to_string() > r.to_string())),
                _ => Err(InterpreterError::TypeMismatch2(">".to_string(), l, r))
            },
            "<" => match (&l, &r) {
                (Bool(l), Bool(r)) => Ok(Bool(*l < *r)),
                (I32(l), I32(r)) => Ok(Bool(l < r)),
                (Str(l), Str(r)) => Ok(Bool(l.to_string() < r.to_string())),
                _ => Err(InterpreterError::TypeMismatch2("<".to_string(), l, r))
            },
            ">=" => match (&l, &r) {
                (Bool(l), Bool(r)) => Ok(Bool(*l >= *r)),
                (I32(l), I32(r)) => Ok(Bool(l >= r)),
                (Str(l), Str(r)) => Ok(Bool(l.to_string() >= r.to_string())),
                _ => Err(InterpreterError::TypeMismatch2(">=".to_string(), l, r))
            },
            "<=" => match (&l, &r) {
                (Bool(l), Bool(r)) => Ok(Bool(*l <= *r)),
                (I32(l), I32(r)) => Ok(Bool(l <= r)),
                (Str(l), Str(r)) => Ok(Bool(l.to_string() <= r.to_string())),
                _ => Err(InterpreterError::TypeMismatch2("<=".to_string(), l, r))
            },
            "-" => match (&l, &r) {
                (I32(l), Bool(r)) => Ok(I32(l - if *r {1} else {0})),
                (I32(l), I32(r)) => Ok(I32(l - r)),
                _ => Err(InterpreterError::TypeMismatch2("-".to_string(), l, r))
            },
            "*" => match (&l, &r) {
                (Bool(l), I32(r)) => Ok(I32(if *l {*r} else {0})),
                (Bool(l), Str(r)) => Ok(Str(if *l {r.to_string()} else {"".to_string()})),
                (I32(l), Bool(r)) => Ok(I32(if *r {*l} else {0})),
                (I32(l), I32(r)) => Ok(I32(l * r)),
                // (I32(l), Str(r)) => Ok(Str(l.to_string() * r)),
                (Str(l), Bool(r)) => Ok(Str(if *r {l.to_string()} else {"".to_string()})),
                // (Str(l), I32(r)) => Ok(Str(l * r.to_string())),
                _ => Err(InterpreterError::TypeMismatch2("*".to_string(), l, r))
            },
            "&&" => match (&l, &r) {
                (Bool(l), Bool(r)) => Ok(Bool(*l&&*r)),
                _ => Err(InterpreterError::TypeMismatch2("&&".to_string(), l, r))

            },
            "||" => match (&l, &r) {
                (Bool(l), Bool(r)) => Ok(Bool(*l||*r)),
                _ => Err(InterpreterError::TypeMismatch2("||".to_string(), l, r))

            },
            "^" => match (&l, &r) {
                (I32(l), Bool(r)) => Ok(I32(if *r {*l} else {1})),
                (I32(l), I32(r)) => Ok(I32(i32::pow(*l, *r as u32))), // TODO: require pos pow
                _ => Err(InterpreterError::TypeMismatch2("^".to_string(), l, r))
            },
            op => Err(InterpreterError::UnknownInfixOperator(op.to_string())),
        }
    }

    fn handle_error(&mut self, expr: &String) -> Res {
        Err(InterpreterError::FailedParse(expr.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::Interpreter;
    use super::Res;
    use super::super::parser;
    use super::super::ast::*;
    use PrimValue::*;
    use Node::*;

    #[test]
    fn eval_num() {
        let mut interp = Interpreter::default();
        let tree = Prim(I32(12));
        assert_eq!(interp.visit_root(&tree), Ok(I32(12)));
    }

    fn eval_str(s: String) -> Res {
        let ast = parser::parse(s);
        let mut interp = Interpreter::default();
        interp.visit_root(&ast)
    }

    #[test]
    fn parse_and_eval_bool() {
        assert_eq!(eval_str("true".to_string()), Ok(Bool(true)));
    }

    #[test]
    fn parse_and_eval_bool_and() {
        assert_eq!(eval_str("true&&true".to_string()), Ok(Bool(true)));
        assert_eq!(eval_str("false&&true".to_string()), Ok(Bool(false)));
        assert_eq!(eval_str("true&&false".to_string()), Ok(Bool(false)));
        assert_eq!(eval_str("false&&false".to_string()), Ok(Bool(false)));
    }

    #[test]
    fn parse_and_eval_bool_or() {
        assert_eq!(eval_str("true||true".to_string()), Ok(Bool(true)));
        assert_eq!(eval_str("false||true".to_string()), Ok(Bool(true)));
        assert_eq!(eval_str("true||false".to_string()), Ok(Bool(true)));
        assert_eq!(eval_str("false||false".to_string()), Ok(Bool(false)));
    }

    #[test]
    fn parse_and_eval_bool_eq() {
        assert_eq!(eval_str("true==true".to_string()), Ok(Bool(true)));
        assert_eq!(eval_str("false==true".to_string()), Ok(Bool(false)));
        assert_eq!(eval_str("true==false".to_string()), Ok(Bool(false)));
        assert_eq!(eval_str("false==false".to_string()), Ok(Bool(true)));
    }

    #[test]
    fn parse_and_eval_i32() {
        assert_eq!(eval_str("32".to_string()), Ok(I32(32)));
    }

    #[test]
    fn parse_and_eval_i32_eq() {
        assert_eq!(eval_str("0==0".to_string()), Ok(Bool(true)));
        assert_eq!(eval_str("-1==1".to_string()), Ok(Bool(false)));
        assert_eq!(eval_str("1==123".to_string()), Ok(Bool(false)));
        assert_eq!(eval_str("1302==1302".to_string()), Ok(Bool(true)));
    }

    #[test]
    fn parse_and_eval_str() {
        assert_eq!(eval_str("\"32\"".to_string()), Ok(Str("32".to_string())));
    }

    #[test]
    fn bind_sym() {
        let mut interp = Interpreter::default();
        let value = Prim(I32(12));
        let let_x = LetNode {
            name: "x".to_string(),
            value: Some(Box::new(value))
        };

        interp.bind(vec!["x".to_string()], let_x.clone()).expect("binding x failed");
        assert_eq!(interp.lookup(
                vec!["x".to_string()]
            ), Some(let_x));
    }

    #[test]
    fn bind_sym_near_other() {
        let mut interp = Interpreter::default();
        let value = Node::Prim(PrimValue::I32(12));
        let let_x = LetNode {
            name: "x".to_string(),
            value: Some(Box::new(value))
        };
        let let_y = LetNode {
            name: "y".to_string(),
            value: Some(Box::new(Prim(I32(13))))
        };


        interp.bind(vec!["x".to_string()], let_x.clone()).expect("binding x failed");
        interp.bind(vec!["y".to_string()], let_y.clone()).expect("binding y failed");
        assert_eq!(interp.lookup(
                vec!["x".to_string()]
            ), Some(let_x));
    }

    #[test]
    fn bind_sym_near_overlap() {
        let mut interp = Interpreter::default();
        let value = Node::Prim(PrimValue::I32(12));
        let let_x = LetNode {
            name: "x".to_string(),
            value: Some(Box::new(value))
        };
        let let_y = LetNode {
            name: "x".to_string(),
            value: Some(Box::new(Node::Prim(PrimValue::I32(13))))
        };


        interp.bind(vec!["x".to_string()], let_x.clone()).expect("binding x failed");
        interp.bind(vec!["inner".to_string(), "x".to_string()], let_y.clone()).expect("binding inner/x failed");
        assert_eq!(interp.lookup(
                vec!["x".to_string()]
            ), Some(let_x));
        assert_eq!(interp.lookup(
                vec!["inner".to_string(), "x".to_string()]
            ), Some(let_y));
    }
}
