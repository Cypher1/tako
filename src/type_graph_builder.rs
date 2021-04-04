use crate::ast::*;
use crate::database::Compiler;
use crate::errors::TError;
use crate::primitives::Prim;

use crate::type_graph::*;

// Walks the AST interpreting it.
#[derive(Default)]
pub struct TypeGraphBuilder {}

// TODO: Return graphs.
type Res = Result<(), TError>;

#[derive(Debug, Clone)]
pub struct State {
    pub graph: TypeGraph,
    pub path: Path,
}

impl Visitor<State, (), TypeGraph, Path> for TypeGraphBuilder {
    fn visit_root(&mut self, db: &dyn Compiler, module: &Path) -> Result<TypeGraph, TError> {
        let expr = &db.parse_file(module.clone())?;
        if db.debug_level() > 0 {
            eprintln!(
                "building symbol table & type graph for file... {}",
                path_to_string(&module)
            );
        }
        let mut state = State {
            path: module.clone(),
            graph: TypeGraph::default(),
        };
        self.visit(db, &mut state, &expr)?;
        Ok(state.graph)
    }

    fn visit_sym(&mut self, db: &dyn Compiler, state: &mut State, expr: &Sym) -> Res {
        if db.debug_level() > 1 {
            eprintln!(
                "visiting sym {} {}",
                path_to_string(&state.path),
                &expr.name
            );
        }
        Ok(())
    }

    fn visit_prim(&mut self, _db: &dyn Compiler, _state: &mut State, _expr: &Prim) -> Res {
        Ok(())
    }

    fn visit_apply(&mut self, db: &dyn Compiler, state: &mut State, expr: &Apply) -> Res {
        state.path.push(Symbol::Anon());
        expr.args
            .iter()
            .map(|arg| self.visit_let(db, state, &arg))
            .collect::<Result<Vec<()>, TError>>()?;
        self.visit(db, state, &*expr.inner)?;
        state.path.pop();
        Ok(())
    }

    fn visit_abs(&mut self, db: &dyn Compiler, state: &mut State, expr: &Abs) -> Res {
        if db.debug_level() > 1 {
            eprintln!("visiting {} {}", path_to_string(&state.path), &expr.name);
        }
        self.visit(db, state, &expr.value)
    }

    fn visit_let(&mut self, db: &dyn Compiler, state: &mut State, expr: &Let) -> Res {
        if db.debug_level() > 1 {
            eprintln!("visiting {} {}", path_to_string(&state.path), &expr.name);
        }
        let path_name = Symbol::new(expr.name.clone());
        state.path.push(path_name);
        if let Some(args) = &expr.args {
            args.iter()
                .map(|arg| self.visit_let(db, state, &arg))
                .collect::<Result<Vec<()>, TError>>()?;
        }
        self.visit(db, state, &expr.value)?;
        state.path.pop();
        Ok(())
    }

    fn visit_un_op(&mut self, db: &dyn Compiler, state: &mut State, expr: &UnOp) -> Res {
        self.visit(db, state, &expr.inner)
    }

    fn visit_bin_op(&mut self, db: &dyn Compiler, state: &mut State, expr: &BinOp) -> Res {
        self.visit(db, state, &expr.left)?;
        self.visit(db, state, &expr.right)
    }

    fn handle_error(&mut self, _db: &dyn Compiler, _state: &mut State, expr: &TError) -> Res {
        Err(expr.clone())
    }
}

#[cfg(test)]
mod tests {}
