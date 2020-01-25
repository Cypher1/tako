use super::ast::*;
use std::fmt::Write;

#[derive(Debug)]
#[derive(PartialEq)]
pub enum PrettyPrintError {
    FailedParse(String, Info),
}

// Walks the AST interpreting it.
pub struct PrettyPrint {
}

impl Default for PrettyPrint {
    fn default() -> PrettyPrint {
        PrettyPrint {}
    }
}

// TODO: Return nodes.
type Res = Result<(), PrettyPrintError>;
type State = String;
impl Visitor<State, (), String, PrettyPrintError> for PrettyPrint {
    fn visit_root(&mut self, expr: &Node) -> Result<String, PrettyPrintError> {
        let mut state: String = "".to_string();
        self.visit(&mut state, expr)?;
        Ok(state)
    }

    fn visit_sym(&mut self, state: &mut State, expr: &Sym) -> Res {
        write!(state, "{}", expr.name);
        Ok(())
    }

    fn visit_prim(&mut self, state: &mut State, expr: &Prim) -> Res {
        use Prim::*;
        match expr {
            Unit(_) => {
                write!(state, "()");
                Ok(())
            },
            Bool(val, _) => {
                write!(state, "{}", val);
                Ok(())
            },
            I32(val, _) => {
                write!(state, "{}", val);
                Ok(())
            },
            Str(val, _) => {
                write!(state, "'{}'", val);
                Ok(())
            },
            Lambda(val) => {
                self.visit(state, val)?;
                Ok(())
            },
        }
    }

    fn visit_apply(&mut self, state: &mut State, expr: &Apply) -> Res {
        self.visit(state, &*expr.inner)?;
        write!(state, "(");
        let mut is_first = true;
        for arg in expr.args.iter() {
            if !is_first {
                write!(state, ", ");
            }
            self.visit_let(state, &arg)?;
            is_first = false;
        }
        write!(state, ")");
        Ok(())
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        write!(state, "{}", expr.name);
        match &expr.value {
            Some(val) => {
                write!(state, "=");
                self.visit(state, &*val)?
            },
            None => {}
        }
        Ok(())
    }

    fn visit_un_op(&mut self, state: &mut State, expr: &UnOp) -> Res {
        write!(state, "({}", expr.name);
        self.visit(state, &*expr.inner)?;
        write!(state, ")");
        Ok(())
    }

    fn visit_bin_op(&mut self, state: &mut State, expr: &BinOp) -> Res {
        write!(state, "(");
        self.visit(state, &*expr.left)?;
        write!(state, "{}", expr.name);
        self.visit(state, &*expr.right)?;
        write!(state, ")");
        Ok(())
    }

    fn handle_error(&mut self, _state: &mut State, expr: &Err) -> Res {
        Err(PrettyPrintError::FailedParse(expr.msg.to_string(), expr.get_info()))
    }
}

#[cfg(test)]
mod tests {
    use super::PrettyPrint;
    use super::Res;
    use super::super::parser;
    use super::super::ast::*;
    use Prim::*;
    use Node::*;
}
