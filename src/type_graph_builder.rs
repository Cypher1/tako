use crate::ast::*;
use crate::database::Compiler;
use crate::errors::TError;
use crate::primitives::Prim;
use std::collections::HashMap;

// Walks the AST interpreting it.
#[derive(Default)]
pub struct TypeGraphBuilder {}

// TODO: Return graphs.
type Res = Result<(), TError>;

type Id = i32; // TODO: Make this a vec of Id so it can be treated as a stack

#[derive(Debug, Clone)]
pub struct State {
    pub graph: TypeGraph,
    pub path: Path,
}

#[derive(Default, Debug, Clone)]
pub struct TypeGraph {
    // Map from paths to Ids (to avoid mapping from Paths to other things)
    pub symbols: HashMap<Path, Id>,

    pub types: HashMap<Id, Prim>,

    // A counter used for generating new type variables... (probably a bad idea)
    pub counter: Id,
}

impl TypeGraph {
    fn get_new_id(&mut self) -> Id {
        let curr = self.counter;
        self.counter +=1;
        curr
    }

    fn id_for_path(&mut self, path: &Path) -> Option<&Id> {
        self.symbols.get(path).clone()
    }

    fn get_id_for_path(&mut self, path: &Path) -> Id {
        let id = self.symbols.get(path).clone();
        if let Some(id) = id {
            id.clone()
        } else {
            let new = self.get_new_id();
            self.symbols.insert(path.clone(), new.clone());
            new
        }
    }

    fn get_type(&self, path: &Path) -> Option<Prim> {
        let id = self.symbols.get(path); // TODO: Use get_id_for_path?
        if let Some(id) = id {
            self.types.get(&id).cloned()
        } else {
            None
        }
    }

    fn restrict_type_for_id(&mut self, id: &Id, ty: &Prim) -> Result<(), TError> {
        // TODO: Merge with existing types
        let curr_ty = self.types.get(id);
        let m = if let Some(curr_ty) = curr_ty {
            use crate::interpreter::prim_type_and;
            // TODO: Unify not product...
            prim_type_and(curr_ty.clone(), ty.clone())?
        } else {
            ty.clone()
        };

        // Check overlap?

        self.types.insert(id.clone(), m);
        Ok(())
    }

    pub fn restrict_type(&mut self, path: &Path, ty: &Prim) -> Result<(), TError> {
        let id = self.get_id_for_path(path);
        self.restrict_type_for_id(&id, ty)
    }
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
        expr
            .args
            .iter()
            .map(|arg| {
                self.visit_let(db, state, &arg)
            })
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
mod tests {
    use super::TypeGraph;
    use crate::ast::{Path, Symbol::*};
    use crate::primitives::{i32_type, variable, number_type, Prim::*};
    use crate::errors::TError;

    fn test_path() -> Path {
        vec![
            Named("root".to_string(), None),
            Named("file".to_string(), Some("tk".to_string())),
            Named("main".to_string(), None)
        ]
    }

    #[test]
    fn adding_type_gets_a_new_id() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &i32_type())?;
        let id = tgb.id_for_path(&path);
        assert_ne!(id, None, "Should get a new Id for this path");
        Ok(())
    }

    #[test]
    fn adding_a_second_type_gets_the_same_id() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &i32_type())?;
        let id = tgb.id_for_path(&path).cloned();
        tgb.restrict_type(&path, &number_type())?;
        let id2 = tgb.id_for_path(&path).cloned();
        assert_eq!(id, id2, "Should get the same Id for this path");
        Ok(())
    }

    #[test]
    fn round_trips_type_single_i32() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &i32_type())?;

        let ty = tgb.get_type(&path);
        assert_eq!(ty, Some(i32_type()));

        Ok(())
    }

    #[test]
    fn merges_types_for_same_path() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &i32_type())?;
        tgb.restrict_type(&path, &number_type())?;

        let ty = tgb.get_type(&path);
        assert_eq!(ty, Some(Product(set![i32_type(), number_type()])));
        Ok(())
    }

    #[test]
    fn unifies_variables_in_types() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &variable("a"))?;
        tgb.restrict_type(&path, &i32_type())?;

        let ty = tgb.get_type(&path);
        assert_eq!(ty, Some(i32_type()));
        let var_ty_a = tgb.get_type(&path);
        assert_eq!(var_ty_a, Some(i32_type()));
        Ok(())
    }
}
