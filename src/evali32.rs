use super::tree::Tree;
use super::ast::*;

// Walks the AST interpreting it.
pub struct Interpreter;

impl Default for Interpreter {
    fn default () -> Interpreter {
        Interpreter{}
    }
}

impl Visitor<i32, i32> for Interpreter {
    fn visit_root(&mut self, expr: &Node) -> i32 {
        self.visit(expr)
    }

    fn visit_num(&mut self, expr: &i32) -> i32 {
        expr.clone()
    }

    fn visit_un_op(&mut self, expr: &UnOpNode) -> i32 {
        let i = self.visit(&expr.inner);
        match expr.name.as_str() {
            "+" => i,
            "-" => -i,
            "!" => if i == 0 { 1 } else { 0 }, // TODO: bools
            _x => {
                return -6; // x.to_string()+"?"
            }
        }
    }

    fn visit_bin_op(&mut self, expr: &BinOpNode) -> i32 {
        let l = self.visit(&expr.left);
        let r = self.visit(&expr.right);
        match expr.name.as_str() {
            "*" => l*r,
            "+" => l+r,
            "/" => l/r,
            "-" => l-r,
            "^" => i32::pow(l, r as u32), // TODO: require pos pow
            _x => {
                return -6; // x.to_string()+"?"
            }
        }
    }

    fn handle_error(&mut self, expr: &String) -> i32 {
        -6 // TODO use result
    }
}
