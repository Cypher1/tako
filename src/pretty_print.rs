use super::ast::*;
use super::database::Compiler;
use super::errors::TError;
use std::fmt::Write;

// Walks the AST interpreting it.
#[derive(Default)]
pub struct PrettyPrint {}

// TODO: Return nodes.
type Res = Result<(), TError>;
type State = String;

impl Visitor<State, (), String, Node> for PrettyPrint {
    fn visit_root(&mut self, db: &dyn Compiler, expr: &Node) -> Result<String, TError> {
        let mut state: String = "".to_string();
        self.visit(db, &mut state, &expr)?;
        Ok(state)
    }

    fn visit_sym(&mut self, _db: &dyn Compiler, state: &mut State, expr: &Sym) -> Res {
        if let Some(def_at) = expr.get_info().defined_at {
            let path: Vec<String> = def_at.iter().map(|p| format!("{}", p)).collect();
            write!(state, "::{}", path.join("::")).unwrap();
        } else {
            write!(state, "{}", expr.name).unwrap();
        }
        Ok(())
    }

    fn visit_prim(&mut self, db: &dyn Compiler, state: &mut State, expr: &Prim) -> Res {
        use Prim::*;
        match expr {
            Bool(val, _) => {
                write!(state, "{}", val).unwrap();
                Ok(())
            }
            I32(val, _) => {
                write!(state, "{}", val).unwrap();
                Ok(())
            }
            Str(val, _) => {
                write!(state, "{:?}", val).unwrap();
                Ok(())
            }
            Lambda(val) => {
                self.visit(db, state, val)?;
                Ok(())
            }
            TypeValue(val, _) => {
                write!(state, "{}", val).unwrap();
                Ok(())
            }
        }
    }

    fn visit_apply(&mut self, db: &dyn Compiler, state: &mut State, expr: &Apply) -> Res {
        self.visit(db, state, &*expr.inner)?;
        write!(state, "(").unwrap();
        let mut is_first = true;
        for arg in expr.args.iter() {
            if !is_first {
                write!(state, ", ").unwrap();
            }
            self.visit_let(db, state, &arg)?;
            is_first = false;
        }
        write!(state, ")").unwrap();
        Ok(())
    }

    fn visit_let(&mut self, db: &dyn Compiler, state: &mut State, expr: &Let) -> Res {
        if let Some(def_at) = expr.get_info().defined_at {
            let path: Vec<String> = def_at.iter().map(|p| format!("{}", p)).collect();
            write!(state, "::{}", path.join("::")).unwrap();
        } else {
            write!(state, "{}", expr.name).unwrap();
        }
        match &expr.args {
            Some(reqs) => {
                if !reqs.is_empty() {
                    write!(state, "(").unwrap();
                }
                let mut is_first = true;
                for arg in reqs {
                    if !is_first {
                        write!(state, ", ").unwrap();
                    }
                    self.visit_sym(db, state, &arg)?;
                    is_first = false;
                }
                if !reqs.is_empty() {
                    write!(state, ")").unwrap();
                }
            }
            None => {}
        }
        write!(state, "=").unwrap();
        self.visit(db, state, &*expr.value)
    }

    fn visit_un_op(&mut self, db: &dyn Compiler, state: &mut State, expr: &UnOp) -> Res {
        write!(state, "({}", expr.name).unwrap();
        self.visit(db, state, &*expr.inner)?;
        write!(state, ")").unwrap();
        Ok(())
    }

    fn visit_bin_op(&mut self, db: &dyn Compiler, state: &mut State, expr: &BinOp) -> Res {
        write!(state, "(").unwrap();
        self.visit(db, state, &*expr.left)?;
        write!(state, "{}", expr.name).unwrap();
        self.visit(db, state, &*expr.right)?;
        write!(state, ")").unwrap();
        Ok(())
    }

    fn handle_error(&mut self, _db: &dyn Compiler, _state: &mut State, expr: &Err) -> Res {
        Err(TError::FailedParse(expr.msg.to_string(), expr.get_info()))
    }
}

#[cfg(test)]
mod tests {}
