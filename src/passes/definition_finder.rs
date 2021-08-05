use crate::ast::*;
use crate::components::{DefinedAt, SymbolRef};
use crate::database::DBStorage;
use crate::errors::TError;
use crate::externs::get_externs;
use crate::passes::symbol_table_builder::State;
use crate::primitives::Val;
use log::*;
use specs::prelude::*;
use std::collections::HashMap;

// Walks the AST interpreting it.
#[derive(Default)]
pub struct DefinitionFinder {}

// TODO: Return nodes.
type Res = Result<Node, TError>;

#[derive(Debug, Clone)]
pub struct Namespace {
    name: Symbol,
    info: Entry,
}

struct DefinitionFinderSystem {
    path_to_entity: HashMap<Path, Entity>,
}

impl<'a> System<'a> for DefinitionFinderSystem {
    type SystemData = (ReadStorage<'a, SymbolRef>, WriteStorage<'a, DefinedAt>);

    fn run(&mut self, (symbols, mut defined_at_map): Self::SystemData) {
        // dbg!(&self.path_to_entity);
        for (symbol, defined_at) in (&symbols, &mut defined_at_map).join() {
            let get_entity = || {
                let mut context = symbol.context.clone();
                loop {
                    let mut search_path = context.clone();
                    search_path.extend(symbol.name.clone());
                    if let Some(entity) = self.path_to_entity.get(&search_path) {
                        return Some((entity, search_path));
                    }
                    if context.is_empty() {
                        return None;
                    }
                    context.pop(); // go back
                }
            };
            if defined_at.0 != None {
                continue; // this one is 'pre' defined
            }
            if let Some((entity, found_path)) = get_entity() {
                debug!(
                    "Found symbol: {:?} -> {:?} @ {:?}",
                    &symbol, &entity, &found_path
                );
                defined_at.0 = Some(found_path);
            } else if let Some(ext) = get_externs().unwrap().get(&path_to_string(&symbol.name)) {
                debug!("Found extern: {:?} -> {:?}", &symbol, &ext);
            } else {
                warn!("Couldn't find symbol: {:?}", &symbol);
            }
        }
    }
}

impl Visitor<State, Node, Root, Path> for DefinitionFinder {
    fn visit_root(&mut self, storage: &mut DBStorage, module: &Path) -> Result<Root, TError> {
        let expr = storage.build_symbol_table(module.clone())?;
        info!(
            "looking up definitions in file... {}",
            path_to_string(module)
        );
        let mut definition_finder = DefinitionFinderSystem {
            path_to_entity: storage.path_to_entity.clone(),
        };
        definition_finder.run_now(&storage.world);

        let mut state = State {
            path: module.clone(),
            table: expr.table.clone(),
        };
        let ast = self.visit(storage, &mut state, &expr.ast)?;
        Ok(Root {
            ast,
            table: state.table,
        })
    }

    fn visit_sym(&mut self, _storage: &mut DBStorage, state: &mut State, expr: &Sym) -> Res {
        debug!(
            "visiting sym {} {}",
            path_to_string(&state.path),
            &expr.name
        );
        let mut search: Vec<Symbol> = state.path.clone();
        loop {
            if let Some(Symbol::Anon) = search.last() {
                search.pop(); // Cannot look inside an 'anon'.
            }
            search.push(Symbol::new(&expr.name));
            let node = state.table.find_mut(&search);
            match node {
                Some(node) => {
                    node.value.uses.insert(state.path.clone());
                    debug!(
                        "FOUND {} at {}\n",
                        expr.name.clone(),
                        path_to_string(&search)
                    );
                    let mut res = expr.clone();
                    res.info.defined_at = Some(search);
                    return Ok(res.into_node());
                }
                None => {
                    search.pop(); // Strip the name off.
                    debug!(
                        "   not found {} at {}",
                        expr.name.clone(),
                        path_to_string(&search)
                    );
                    if search.is_empty() {
                        return Err(TError::UnknownSymbol(
                            expr.name.clone(),
                            expr.get_info(),
                            path_to_string(&state.path),
                        ));
                    }
                    search.pop(); // Up one, go again.
                }
            }
        }
    }

    fn visit_val(&mut self, _storage: &mut DBStorage, _state: &mut State, expr: &Val) -> Res {
        Ok(expr.clone().into_node())
    }

    fn visit_apply(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Apply) -> Res {
        state.path.push(Symbol::Anon);
        let args = expr
            .args
            .iter()
            .map(|arg| {
                let val = self.visit_let(storage, state, arg)?.as_let();
                let mut search = state.path.clone();
                search.push(Symbol::new(&arg.name));
                let node = state.table.find_mut(&search);
                if let Some(node) = node {
                    node.value.uses.insert(state.path.clone());
                }
                val
            })
            .collect::<Result<Vec<Let>, TError>>()?;
        let inner = Box::new(self.visit(storage, state, &*expr.inner)?);
        state.path.pop();
        Ok(Apply {
            inner,
            args,
            info: expr.get_info(),
        }
        .into_node())
    }

    fn visit_abs(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Abs) -> Res {
        debug!("visiting {} {}", path_to_string(&state.path), &expr.name);
        let value = Box::new(self.visit(storage, state, &expr.value)?);
        Ok(Abs {
            name: expr.name.clone(),
            value,
            info: expr.info.clone(),
        }
        .into_node())
    }

    fn visit_let(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Let) -> Res {
        debug!("visiting {} {}", path_to_string(&state.path), &expr.name);
        let path_name = Symbol::new(&expr.name);
        state.path.push(path_name);
        let args = if let Some(args) = &expr.args {
            Some(
                args.iter()
                    .map(|arg| self.visit_let(storage, state, arg)?.as_let())
                    .collect::<Result<Vec<Let>, TError>>()?,
            )
        } else {
            None
        };
        let value = Box::new(self.visit(storage, state, &expr.value)?);
        state.path.pop();
        Ok(Let {
            name: expr.name.clone(),
            args,
            value,
            info: expr.info.clone(),
        }
        .into_node())
    }

    fn visit_un_op(&mut self, storage: &mut DBStorage, state: &mut State, expr: &UnOp) -> Res {
        let inner = Box::new(self.visit(storage, state, &expr.inner)?);
        Ok(UnOp {
            name: expr.name.clone(),
            inner,
            info: expr.get_info(),
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
            info: expr.get_info(),
        }
        .into_node())
    }
}

#[cfg(test)]
mod tests {}
