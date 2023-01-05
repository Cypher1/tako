use crate::ast::{path_to_string, Abs, Apply, BinOp, HasInfo, Let, Node, Sym, UnOp, Visitor};
use crate::database::DBStorage;
use crate::errors::TError;
use crate::primitives::Val;
use std::fmt::Write;

// Walks the AST interpreting it.
#[derive(Default)]
pub struct PrettyPrint {}

type Res = Result<(), TError>;
type State = String;

impl Visitor<State, (), String, Node> for PrettyPrint {
    fn visit_root(&mut self, storage: &mut DBStorage, expr: &Node) -> Result<String, TError> {
        let mut state: String = "".to_string();
        self.visit(storage, &mut state, expr)?;
        Ok(state)
    }

    fn visit_sym(&mut self, _storage: &mut DBStorage, state: &mut State, expr: &Sym) -> Res {
        if let Some(def_at) = &expr.get_info().defined_at {
            write!(state, ".{}", path_to_string(def_at))?;
        } else {
            write!(state, "{}", expr.name)?;
        }
        Ok(())
    }

    fn visit_val(&mut self, _storage: &mut DBStorage, state: &mut State, expr: &Val) -> Res {
        write!(state, "{}", &expr)?;
        Ok(())
    }

    fn visit_apply(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Apply) -> Res {
        write!(state, "(")?;
        self.visit(storage, state, &*expr.inner)?;
        write!(state, ")(")?;
        let mut is_first = true;
        for arg in &expr.args {
            if is_first {
                is_first = false;
            } else {
                write!(state, ", ")?;
            }
            self.visit_let(storage, state, arg)?;
        }
        write!(state, ")")?;
        Ok(())
    }

    fn visit_abs(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Abs) -> Res {
        write!(state, "{}|-", expr.name)?;
        self.visit(storage, state, &*expr.value)
    }

    fn visit_let(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Let) -> Res {
        if let Some(def_at) = &expr.get_info().defined_at {
            write!(state, ".{}", path_to_string(def_at))?;
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
                self.visit_let(storage, state, arg)?;
                is_first = false;
            }
            write!(state, ")")?;
        }
        write!(state, "=")?;
        self.visit(storage, state, &*expr.value)
    }

    fn visit_un_op(&mut self, storage: &mut DBStorage, state: &mut State, expr: &UnOp) -> Res {
        write!(state, "({}", expr.name)?;
        self.visit(storage, state, &*expr.inner)?;
        write!(state, ")")?;
        Ok(())
    }

    fn visit_bin_op(&mut self, storage: &mut DBStorage, state: &mut State, expr: &BinOp) -> Res {
        write!(state, "(")?;
        self.visit(storage, state, &*expr.left)?;
        write!(state, "{}", expr.name)?;
        self.visit(storage, state, &*expr.right)?;
        write!(state, ")")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {}
