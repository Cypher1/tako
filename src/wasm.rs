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

#[derive(Debug, PartialEq)]
pub enum CompilerError {
    UnknownInfixOperator(String, Info),
    UnknownPrefixOperator(String, Info),
    FailedParse(String, Info),
}

// Walks the AST compiling it to wasm.
pub struct Compiler;

impl Default for Compiler {
    fn default() -> Compiler {
        Compiler {}
    }
}

type Res = Result<Tree<String>, CompilerError>;
type State = ();
impl Visitor<State, Tree<String>, Tree<String>, CompilerError> for Compiler {
    fn visit_root(&mut self, root: &Root) -> Result<Tree<String>, CompilerError> {
        let name = to_root(&"\"run_main\"".to_string());
        let def = Tree {
            value: "export".to_string(),
            children: vec![name],
        };
        let node_i32 = to_root(&"i32".to_string());
        let param = Tree {
            value: "param".to_string(),
            children: vec![node_i32.clone(), node_i32.clone()],
        };
        let result = Tree {
            value: "result".to_string(),
            children: vec![node_i32],
        };
        let mut children = vec![def, param, result];

        for def in root.graph.iter() {
            eprintln!("def {:?}", def);
            // children.push();
        }
        children.push(self.visit(&mut (), &root.ast)?);
        let func = Tree {
            value: "func".to_string(),
            children,
        };
        Ok(Tree {
            value: "module".to_string(),
            children: vec![func],
        })
    }

    fn visit_sym(&mut self, _state: &mut State, expr: &Sym) -> Res {
        let name = format!("${}", expr.name.to_string());
        let namet = to_root(&name);
        Ok(Tree {
            value: "local.get".to_string(),
            children: vec![namet],
        })
    }

    fn visit_prim(&mut self, _state: &mut State, expr: &Prim) -> Res {
        use Prim::*;
        match expr {
            I32(n, _) => Ok(Tree {
                value: "i32.const".to_string(),
                children: vec![to_root(&n.to_string())]
            }),
            _ => unimplemented!(),
        }
    }

    fn visit_apply(&mut self, _state: &mut State, _expr: &Apply) -> Res {
        panic!("Apply not implemented in wasm");
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        let name = format!("${}", expr.name.to_string());
        let namet = to_root(&name);
        Ok(Tree {
            value: "local.set".to_string(),
            children: vec![namet, self.visit(state, &expr.value)?]
        })
    }

    fn visit_un_op(&mut self, state: &mut State, expr: &UnOp) -> Res {
        use Prim::*;
        let mut res = Vec::new();
        let inner = self.visit(state, &expr.inner)?;
        let info = expr.get_info();
        match expr.name.as_str() {
            "+" => {
                Ok(inner)
            }
            "-" => {
                res.push(self.visit_prim(state, &I32(0, expr.clone().get_info()))?);
                res.push(inner);
                Ok(Tree{
                    value: "i32.sub".to_string(),
                    children: res
                })
            }
            op => Err(CompilerError::UnknownPrefixOperator(op.to_string(), info))
        }
    }
    fn visit_bin_op(&mut self, state: &mut State, expr: &BinOp) -> Res {
        let info = expr.get_info();
        let mut res = Vec::new();
        res.push(self.visit(state, &expr.left.clone())?);
        res.push(self.visit(state, &expr.right.clone())?);
        // TODO: require 2 children
        let s = match expr.name.as_str() {
            "*" => "i32.mul".to_string(),
            "+" => "i32.add".to_string(),
            "/" => "i32.div_s".to_string(), // TODO: require divisibility
            "-" => "i32.sub".to_string(),
            "^" => "i32.pow".to_string(), // TODO: require pos pow
            ";" => "block".to_string(),
            op => return Err(CompilerError::UnknownInfixOperator(op.to_string(), info)),
        };
        Ok(Tree{
            value: s,
            children: res
        })
    }

    fn handle_error(&mut self, _state: &mut State, expr: &Err) -> Res {
        Err(CompilerError::FailedParse(
            expr.msg.clone(),
            expr.get_info(),
        ))
    }
}
