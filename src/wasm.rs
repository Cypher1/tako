use super::tree::*;

/*
(module
  (func (export "addTwo") (param i32 i32) (result i32)
    local.get 0                                             local.get 1
    i32.const 3
    i32.mul
    i32.add))

        //TokenType::Local => {
            // return vec!["locals.get ".to_string() + &expr.value.value];
        // }
*/

use super::ast::*;

#[derive(Debug)]
#[derive(PartialEq)]
pub enum CompilerError {
    UnknownInfixOperator(String),
    UnknownPrefixOperator(String),
    FailedParse(String),
}

// Walks the AST compiling it to wasm.
pub struct Compiler;

impl Default for Compiler {
    fn default() -> Compiler {
        Compiler {}
    }
}

type Res = Result<Vec<String>, CompilerError>;
impl Visitor<Vec<String>, Tree<String>, CompilerError> for Compiler {
    fn visit_root(&mut self, expr: &Node) -> Result<Tree<String>, CompilerError> {
        let name = Tree {
            value: "\"run_main\"".to_string(),
            children: vec![],
        };
        let def = Tree {
            value: "export".to_string(),
            children: vec![name.clone()],
        };
        let node_i32 = Tree {
            value: "i32".to_string(),
            children: vec![],
        };
        let param = Tree {
            value: "param".to_string(),
            children: vec![node_i32.clone(), node_i32.clone()],
        };
        let result = Tree {
            value: "result".to_string(),
            children: vec![node_i32.clone()],
        };
        let mut children = vec![def, param, result];
        children.append(&mut to_tree(self.visit(&expr)?));
        let func = Tree {
            value: "func".to_string(),
            children: children,
        };
        return Ok(Tree {
            value: "module".to_string(),
            children: vec![func],
        });
    }

    fn visit_call(&mut self, expr: &CallNode) -> Res {
        panic!("Call not implemented in wasm");
    }

    fn visit_prim(&mut self, expr: &PrimValue) -> Res {
        use PrimValue::*;
        match expr {
            I32(n) => Ok(vec!["i32.const ".to_string() + &n.to_string()]),
            _ => unimplemented!(),
        }
    }

    fn visit_let(&mut self, expr: &LetNode) -> Res {
        panic!("Let not implemented in wasm");
    }

    fn visit_un_op(&mut self, expr: &UnOpNode) -> Res {
        let mut res = Vec::new();
        res.append(&mut self.visit(&expr.inner)?);
        match expr.name.as_str() {
            "+" => {}
            "-" => res.push("i32.sub".to_string()),
            op => return Err(CompilerError::UnknownPrefixOperator(op.to_string())),
        };
        return Ok(res);
    }
    fn visit_bin_op(&mut self, expr: &BinOpNode) -> Res {
        let mut res = Vec::new();
        res.append(&mut self.visit(&expr.left)?);
        res.append(&mut self.visit(&expr.right)?);
        // TODO: require 2 children
        let s = match expr.name.as_str() {
            "*" => "i32.mul".to_string(),
            "+" => "i32.add".to_string(),
            "/" => "i32.div_s".to_string(), // TODO: require divisibility
            "-" => "i32.sub".to_string(),
            "^" => "i32.pow".to_string(), // TODO: require pos pow
            op => return Err(CompilerError::UnknownInfixOperator(op.to_string())),
        };
        res.push(s);
        return Ok(res);
    }

    fn handle_error(&mut self, expr: &String) -> Res {
        Err(CompilerError::FailedParse(expr.to_string()))
    }
}
