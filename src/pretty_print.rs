use super::ast::*;
use super::database::Compiler;
use super::errors::TError;
use super::primitives::Prim;
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
            write!(state, "::{}", path_to_string(&def_at)).unwrap();
        } else {
            write!(state, "{}", expr.name).unwrap();
        }
        Ok(())
    }

    fn visit_prim(&mut self, db: &dyn Compiler, state: &mut State, expr: &Prim) -> Res {
        write!(state, "{}", &expr).unwrap();
        Ok(())
    }

    fn visit_apply(&mut self, db: &dyn Compiler, state: &mut State, expr: &Apply) -> Res {
        self.visit(db, state, &*expr.inner)?;
        write!(state, "(").unwrap();
        let mut is_first = true;
        for arg in expr.args.iter() {
            self.visit_let(db, state, &arg)?;
            if is_first {
                is_first = true;
            } else {
                write!(state, ", ").unwrap();
            }
        }
        write!(state, ")").unwrap();
        Ok(())
    }

    fn visit_let(&mut self, db: &dyn Compiler, state: &mut State, expr: &Let) -> Res {
        if let Some(def_at) = expr.get_info().defined_at {
            write!(state, "::{}", path_to_string(&def_at)).unwrap();
        } else {
            write!(state, "{}", expr.name).unwrap();
        }

        if let Some(args) = &expr.args {
            write!(state, "(").unwrap();
            let mut is_first = true;
            for arg in args.iter() {
                self.visit_let(db, state, &arg)?;
                if is_first {
                    is_first = true;
                } else {
                    write!(state, ", ").unwrap();
                }
            }
            write!(state, ")").unwrap();
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

    fn handle_error(&mut self, _db: &dyn Compiler, _state: &mut State, expr: &TError) -> Res {
        Err(expr.clone())
    }
}

#[cfg(test)]
mod tests {}
