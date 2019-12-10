use super::ast::*;

#[derive(Debug)]
pub enum InterpreterError {
    UnknownOperator(String),
    FailedParse(String),
}

// Walks the AST interpreting it.
pub struct Interpreter;

impl Default for Interpreter {
    fn default() -> Interpreter {
        Interpreter {}
    }
}

impl Visitor<i32, i32, InterpreterError> for Interpreter {
    fn visit_root(&mut self, expr: &Node) -> Result<i32, InterpreterError> {
        self.visit(expr)
    }

    fn visit_num(&mut self, expr: &i32) -> Result<i32, InterpreterError> {
        Ok(expr.clone())
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
