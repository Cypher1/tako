use super::ast::*;
use super::tree::*;

use std::cmp;

#[derive(Debug)]
#[derive(PartialEq)]
pub enum InterpreterError {
    UnknownOperator(String),
    FailedParse(String),
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
    pub fn bind_to(curr: &mut Tree<LetNode>, path: Vec<String>, binding: LetNode) -> Result<(), String> {
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

impl Visitor<i32, i32, InterpreterError> for Interpreter {
    fn visit_root(&mut self, expr: &Node) -> Result<i32, InterpreterError> {
        self.visit(expr)
    }

    fn visit_call(&mut self, expr: &CallNode) -> Result<i32, InterpreterError> {
        panic!("Call not implemented in interpreter");
    }

    fn visit_num(&mut self, expr: &i32) -> Result<i32, InterpreterError> {
        Ok(expr.clone())
    }

    fn visit_let(&mut self, expr: &LetNode) -> Result<i32, InterpreterError> {
        panic!("Let not implemented in interpreter");
    }

    fn visit_un_op(&mut self, expr: &UnOpNode) -> Result<i32, InterpreterError> {
        let i = self.visit(&expr.inner)?;
        match expr.name.as_str() {
            "+" => Ok(i),
            "-" => Ok(-i),
            "!" => Ok(if i == 0 { 1 } else { 0 }), // TODO: bools
            op => Err(InterpreterError::UnknownOperator(op.to_string())),
        }
    }

    fn visit_bin_op(&mut self, expr: &BinOpNode) -> Result<i32, InterpreterError> {
        let l = self.visit(&expr.left)?;
        let r = self.visit(&expr.right)?;
        match expr.name.as_str() {
            "*" => Ok(l * r),
            "+" => Ok(l + r),
            "/" => Ok(l / r),
            "-" => Ok(l - r),
            "^" => Ok(i32::pow(l, r as u32)), // TODO: require pos pow
            op => Err(InterpreterError::UnknownOperator(op.to_string())),
        }
    }

    fn handle_error(&mut self, expr: &String) -> Result<i32, InterpreterError> {
        Err(InterpreterError::FailedParse(expr.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::Interpreter;
    use super::super::ast::*;

    #[test]
    fn eval_num() {
        let mut interp = Interpreter::default();
        let tree = Node::Num(12);
        assert_eq!(interp.visit_root(&tree), Ok(12));
    }

    #[test]
    fn bind_sym() {
        let mut interp = Interpreter::default();
        let value = Node::Num(12);
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
        let value = Node::Num(12);
        let let_x = LetNode {
            name: "x".to_string(),
            value: Some(Box::new(value))
        };
        let let_y = LetNode {
            name: "y".to_string(),
            value: Some(Box::new(Node::Num(13)))
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
        let value = Node::Num(12);
        let let_x = LetNode {
            name: "x".to_string(),
            value: Some(Box::new(value))
        };
        let let_y = LetNode {
            name: "x".to_string(),
            value: Some(Box::new(Node::Num(13)))
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
