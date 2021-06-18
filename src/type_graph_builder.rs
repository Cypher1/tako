use crate::ast::*;
use crate::database::Compiler;
use crate::errors::TError;
use crate::interpreter::Interpreter;
use crate::primitives::{Prim::*, Val::*, *};
use std::collections::BTreeSet;

use crate::type_graph::*;

// Walks the AST interpreting it.
#[derive(Default)]
pub struct TypeGraphBuilder {}

// TODO: Return types.
type Res = Result<Val, TError>;

#[derive(Debug, Clone)]
pub struct State {
    pub graph: TypeGraph,
    pub path: Path,
}

impl Visitor<State, Val, TypeGraph, Path> for TypeGraphBuilder {
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
        let ty = self.visit(db, &mut state, &expr)?;
        state.graph.require_assignable(&state.path, &ty)?;
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
        // TODO: lookup type in state
        Ok(Variable(format!("typeof({})", expr.name)))
    }

    fn visit_val(&mut self, db: &dyn Compiler, state: &mut State, expr: &Val) -> Res {
        match expr {
            Product(vals) => {
                let mut tys: BTreeSet<Val> = set![];
                for val in vals.iter() {
                    tys.insert(self.visit_val(db, state, &val)?);
                }
                Ok(Product(tys))
            }
            Union(vals) => {
                let mut tys: BTreeSet<Val> = set![];
                for val in vals.iter() {
                    tys.insert(self.visit_val(db, state, &val)?);
                }
                Ok(Union(tys))
            }
            PrimVal(I32(_)) => Ok(i32_type()),
            PrimVal(Bool(_)) => Ok(bit_type()),
            PrimVal(Str(_)) => Ok(string_type()),
            Lambda(node) => {
                state.path.push(Symbol::Anon());
                let ty = self.visit(db, state, &node);
                state.path.pop();
                ty
            }
            Struct(vals) => {
                let mut tys: Vec<(String, Val)> = vec![];
                for val in vals.iter() {
                    tys.push((val.0.clone(), self.visit_val(db, state, &val.1)?));
                }
                Ok(Struct(tys))
            }
            _ty => Ok(Val::Variable("Type".to_string())),
        }
    }

    fn visit_apply(&mut self, db: &dyn Compiler, state: &mut State, expr: &Apply) -> Res {
        state.path.push(Symbol::Anon());
        let mut arg_tys = vec![];
        for arg in &expr.args {
            let ty = self.visit_let(db, state, &arg)?;
            arg_tys.push((arg.name.clone(), ty));
        }
        let result_ty = self.visit(db, state, &*expr.inner)?;
        state.path.pop();
        Ok(App {
            inner: Box::new(result_ty),
            arguments: Box::new(Struct(arg_tys)),
        })
    }

    fn visit_abs(&mut self, db: &dyn Compiler, state: &mut State, expr: &Abs) -> Res {
        if db.debug_level() > 1 {
            eprintln!("visiting {} {}", path_to_string(&state.path), &expr.name);
        }
        Ok(WithRequirement(
            Box::new(self.visit(db, state, &expr.value)?),
            vec![expr.name.clone()],
        ))
    }

    fn visit_let(&mut self, db: &dyn Compiler, state: &mut State, expr: &Let) -> Res {
        if db.debug_level() > 1 {
            eprintln!("visiting {} {}", path_to_string(&state.path), &expr.name);
        }
        let path_name = Symbol::new(&expr.name);
        state.path.push(path_name);
        let args = if let Some(args) = &expr.args {
            let mut arg_tys = vec![];
            for arg in args {
                let ty = self.visit_let(db, state, &arg)?;
                arg_tys.push((arg.name.clone(), ty));
            }
            Some(arg_tys)
        } else {
            None
        };
        let val_ty = self.visit(db, state, &expr.value)?;
        // TODO: consider pushing args into the ty via the Function ty.
        // TODO: put the type in the type graph
        let ty = if let Some(args) = args {
            Function {
                intros: Pack::new(),
                arguments: Box::new(Struct(args)),
                results: Box::new(val_ty),
            }
        } else {
            val_ty
        };
        state.graph.require_assignable(&state.path, &ty)?;
        state.path.pop();
        Ok(Struct(vec![(expr.name.clone(), ty)]))
    }

    fn visit_un_op(&mut self, db: &dyn Compiler, state: &mut State, expr: &UnOp) -> Res {
        let op = db
            .get_extern(expr.name.to_string())?
            .expect("operator should exist");
        let ty = op.ty;
        let arg_ty = self.visit(db, state, &expr.inner)?;

        eprintln!("un op {}: {}, {}", &expr.name, &ty, &arg_ty);
        Interpreter::default().visit_apply(
            db,
            &mut vec![],
            &Apply {
                inner: Box::new(ty),
                args: vec![Let::new("it", arg_ty)],
                info: expr.get_info(),
            },
        )
    }

    fn visit_bin_op(&mut self, db: &dyn Compiler, state: &mut State, expr: &BinOp) -> Res {
        let op = db
            .get_extern(expr.name.to_string())?
            .expect("operator should exist");
        let ty = op.ty;
        let left_ty = self.visit(db, state, &expr.left)?;
        let right_ty = self.visit(db, state, &expr.right)?;

        eprintln!("bin op {}: {}, {} {}", &expr.name, &ty, &left_ty, &right_ty);
        Interpreter::default().visit_apply(
            db,
            &mut vec![],
            &Apply {
                inner: Box::new(ty),
                args: vec![Let::new("left", left_ty), Let::new("right", right_ty)],
                info: expr.get_info(),
            },
        )
    }

    fn handle_error(&mut self, _db: &dyn Compiler, _state: &mut State, expr: &TError) -> Res {
        Err(expr.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Symbol;
    use crate::cli_options::Options;
    use crate::database::{Compiler, DB};
    use crate::errors::TError;
    use crate::primitives::{i32_type, string_type};
    use crate::type_graph::TypeGraph;
    use pretty_assertions::assert_eq;
    use std::sync::Arc;

    type Test = Result<(), TError>;

    fn filename() -> String {
        module_root().to_filename()
    }

    fn module_root() -> Symbol {
        Symbol::Named("test".to_owned(), Some("tk".to_owned()))
    }

    fn get_db() -> DB {
        let mut db = DB::default();
        db.set_options(Options::default());
        db
    }

    fn get_tg(s: &str) -> Result<TypeGraph, TError> {
        let mut db = get_db();
        db.set_file(filename(), Ok(Arc::new(s.to_string())));
        let module = db.module_name(filename());
        let mut tgb = TypeGraphBuilder::default();
        let tg: TypeGraph = tgb.visit_root(&db, &module)?;
        Ok(tg)
    }

    #[test]
    fn type_of_int_literal_is_i32() -> Test {
        let tg = &mut get_tg("0")?;
        assert_eq!(tg.get_type(&[module_root()])?, i32_type());
        Ok(())
    }

    #[test]
    fn type_of_variable_of_int_literal_is_i32() -> Test {
        let tg = &mut get_tg("x=0")?;
        assert_eq!(
            tg.get_type(&[module_root(), Symbol::new("x")])?,
            i32_type(),
            "x has type i32"
        );
        assert_eq!(
            tg.get_type(&[module_root()])?,
            rec!("x" => i32_type()),
            "program has type i32"
        );
        Ok(())
    }

    #[test]
    fn type_of_struct_int_and_string() -> Test {
        let tg = &mut get_tg("x=0, y='hi'")?;
        assert_eq!(
            tg.get_type(&[module_root(), Symbol::new("x")])?,
            i32_type(),
            "x has type i32"
        );
        assert_eq!(
            tg.get_type(&[module_root(), Symbol::new("y")])?,
            string_type(),
            "y has type str"
        );
        assert_eq!(
            format!("{}", tg.get_type(&[module_root()])?),
            format!(
                "{}",
                Product(set![rec!("x" => i32_type()), rec!("y" => string_type())])
            ),
            "program has struct type {{x: i32, y: string}}"
        );
        Ok(())
    }
}
