use super::ast::*;
use super::database::Compiler;
use super::errors::TError;
use super::primitives::Val;
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
            write!(state, ".{}", path_to_string(&def_at))?;
        } else {
            write!(state, "{}", expr.name)?;
        }
        Ok(())
    }

    fn visit_val(&mut self, _db: &dyn Compiler, state: &mut State, expr: &Val) -> Res {
        write!(state, "{}", &expr)?;
        Ok(())
    }

    fn visit_apply(&mut self, db: &dyn Compiler, state: &mut State, expr: &Apply) -> Res {
        write!(state, "(")?;
        self.visit(db, state, &*expr.inner)?;
        write!(state, ")(")?;
        let mut is_first = true;
        for arg in expr.args.iter() {
            if is_first {
                is_first = false;
            } else {
                write!(state, ", ")?;
            }
            self.visit_let(db, state, &arg)?;
        }
        write!(state, ")")?;
        Ok(())
    }

    fn visit_abs(&mut self, db: &dyn Compiler, state: &mut State, expr: &Abs) -> Res {
        write!(state, "{}|-", expr.name)?;
        self.visit(db, state, &*expr.value)
    }

    fn visit_let(&mut self, db: &dyn Compiler, state: &mut State, expr: &Let) -> Res {
        if let Some(def_at) = expr.get_info().defined_at {
            write!(state, ".{}", path_to_string(&def_at))?;
        } else {
            write!(state, "{}", expr.name)?;
        }

        if let Some(args) = &expr.args {
            write!(state, "(")?;
            let mut is_first = true;
            for arg in args.iter() {
                if !is_first {
                    write!(state, ", ")?;
                }
                self.visit_let(db, state, &arg)?;
                is_first = false;
            }
            write!(state, ")")?;
        }
        write!(state, "=")?;
        self.visit(db, state, &*expr.value)
    }

    fn visit_un_op(&mut self, db: &dyn Compiler, state: &mut State, expr: &UnOp) -> Res {
        write!(state, "({}", expr.name)?;
        self.visit(db, state, &*expr.inner)?;
        write!(state, ")")?;
        Ok(())
    }

    fn visit_bin_op(&mut self, db: &dyn Compiler, state: &mut State, expr: &BinOp) -> Res {
        write!(state, "(")?;
        self.visit(db, state, &*expr.left)?;
        write!(state, "{}", expr.name)?;
        self.visit(db, state, &*expr.right)?;
        write!(state, ")")?;
        Ok(())
    }

    fn handle_error(&mut self, _db: &dyn Compiler, _state: &mut State, expr: &TError) -> Res {
        Err(expr.clone())
    }
}

#[cfg(test)]
mod tests {}
