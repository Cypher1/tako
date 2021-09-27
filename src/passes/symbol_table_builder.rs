use crate::ast::{
    path_to_string, Abs, Apply, BinOp, HasInfo, Let, Node, Path, Root, Sym, Symbol, ToNode, UnOp,
    Visitor,
};
use crate::database::DBStorage;
use crate::errors::TError;
use crate::primitives::Val;
use crate::symbol_table::Table;
use log::{debug, info};

// Walks the AST interpreting it.
#[derive(Default)]
pub struct SymbolTableBuilder {}

// TODO: Return nodes.
type Res = Result<Node, TError>;

#[derive(Debug, Clone)]
pub struct State {
    pub table: Table,
    pub path: Vec<Symbol>,
}

impl Visitor<State, Node, Root, Path> for SymbolTableBuilder {
    fn visit_root(&mut self, storage: &mut DBStorage, module: &Path) -> Result<Root, TError> {
        let (expr, entity) = &storage.parse_file(module)?;
        info!("Building symbol table... {}", path_to_string(module));

        let mut table = Table::default();
        let mut main_at = module.clone();
        main_at.push(Symbol::new("main"));

        let main_symbol = table.get_mut(&main_at);
        main_symbol.value.uses.insert(module.clone());

        // Add in the globals here!
        // TODO: Inject needs for bootstrapping here (e.g. import function).
        let globals: Vec<Path> = storage
            .get_extern_names()
            .iter()
            .map(|x| vec![Symbol::new(x)])
            .collect();
        for global in globals {
            table.get_mut(&global);
        }

        let mut state = State {
            table,
            path: module.clone(),
        };

        debug!("table: {:?}", state.table);
        Ok(Root {
            ast: self.visit(storage, &mut state, expr)?,
            entity: *entity,
            table: state.table,
        })
    }

    fn visit_sym(&mut self, _storage: &mut DBStorage, _state: &mut State, expr: &Sym) -> Res {
        Ok(expr.clone().into_node())
    }

    fn visit_val(&mut self, _storage: &mut DBStorage, _state: &mut State, expr: &Val) -> Res {
        Ok(expr.clone().into_node())
    }

    fn visit_apply(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Apply) -> Res {
        state.path.push(Symbol::Anon);
        let args = expr
            .args
            .iter()
            .map(|arg| self.visit_let(storage, state, arg)?.as_let())
            .collect::<Result<_, _>>()?;
        let inner = Box::new(self.visit(storage, state, &*expr.inner)?);
        state.path.pop();

        Ok(Apply {
            inner,
            args,
            info: expr.get_info().clone(),
        }
        .into_node())
    }

    fn visit_abs(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Abs) -> Res {
        debug!("visiting {} {}", path_to_string(&state.path), &expr.name);

        // Visit definition.
        let mut info = expr.get_info().clone();
        state.path.push(Symbol::new(&expr.name));
        info.defined_at = Some(state.path.clone());
        state.table.get_mut(&state.path);

        let value = Box::new(self.visit(storage, state, &expr.value)?);
        state.path.pop();

        Ok(Abs {
            name: expr.name.clone(),
            value,
            info,
        }
        .into_node())
    }

    fn visit_let(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Let) -> Res {
        debug!("visiting {} {}", path_to_string(&state.path), &expr.name);

        // Visit definition.
        let mut info = expr.get_info().clone();
        state.path.push(Symbol::new(&expr.name));
        info.defined_at = Some(state.path.clone());
        state.table.get_mut(&state.path);

        // Consider the function arguments defined in this scope.
        let args = if let Some(args) = &expr.args {
            Some(
                args.iter()
                    .map(|arg| self.visit_let(storage, state, arg)?.as_let())
                    .collect::<Result<_, _>>()?,
            )
        } else {
            None
        };
        let value = Box::new(self.visit(storage, state, &expr.value)?);
        state.path.pop();

        Ok(Let {
            name: expr.name.clone(),
            value,
            args,
            info,
        }
        .into_node())
    }

    fn visit_un_op(&mut self, storage: &mut DBStorage, state: &mut State, expr: &UnOp) -> Res {
        let inner = Box::new(self.visit(storage, state, &expr.inner)?);
        Ok(UnOp {
            name: expr.name.clone(),
            inner,
            info: expr.get_info().clone(),
        }
        .into_node())
    }

    fn visit_bin_op(&mut self, storage: &mut DBStorage, state: &mut State, expr: &BinOp) -> Res {
        let left = Box::new(self.visit(storage, state, &expr.left)?);
        let right = Box::new(self.visit(storage, state, &expr.right)?);
        Ok(BinOp {
            name: expr.name.clone(),
            left,
            right,
            info: expr.get_info().clone(),
        }
        .into_node())
    }
}

#[cfg(test)]
mod tests {}
