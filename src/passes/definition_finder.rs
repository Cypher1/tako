use crate::ast::{
    path_to_string, Abs, Apply, BinOp, HasInfo, Let, Node, Path, Root, Sym, Symbol, ToNode, UnOp,
    Visitor,
};
use crate::components::SymbolRef;
use crate::database::DBStorage;
use crate::errors::TError;
use crate::passes::symbol_table_builder::State;
use crate::primitives::Val;
use log::{debug, info};
use specs::prelude::*;
use std::collections::HashMap;

// Walks the AST interpreting it.
#[derive(Default)]
pub struct DefinitionFinder {}

// TODO: Return nodes.
type Res = Result<Node, TError>;

struct DefinitionFinderSystem {
    path_to_entity: HashMap<Path, Entity>,
}

impl<'a> System<'a> for DefinitionFinderSystem {
    type SystemData = WriteStorage<'a, SymbolRef>;

    fn run(&mut self, mut symbols: Self::SystemData) {
        // dbg!(&self.path_to_entity);
        for symbol in (&mut symbols).join() {
            let get_entity = || {
                let mut context = symbol.context.clone();
                loop {
                    let mut search_path = context.clone();
                    search_path.extend(symbol.name.clone());
                    if let Some(entity) = self.path_to_entity.get(&search_path) {
                        debug!(
                            "Found symbol: {:?} -> {:?} @ {:?}",
                            &symbol, &entity, &search_path
                        );
                        return Some(*entity);
                    }
                    if context.is_empty() {
                        return None;
                    }
                    context.pop(); // go back
                }
            };
            if symbol.definition != None {
                continue; // this one is 'pre' defined
            }
            if let Some(entity) = get_entity() {
                symbol.definition = Some(entity);
            } else {
                debug!("Couldn't find symbol: {:?}", &symbol);
            }
        }
    }
}

impl Visitor<State, Node, Root, Path> for DefinitionFinder {
    fn visit_root(&mut self, storage: &mut DBStorage, module: &Path) -> Result<Root, TError> {
        let root = storage.build_symbol_table(module)?;
        info!("Looking up definitions... {}", path_to_string(module));
        let mut definition_finder = DefinitionFinderSystem {
            path_to_entity: storage.path_to_entity.clone(),
        };
        debug!("Running definition_finder");
        definition_finder.run_now(&storage.world);
        debug!("Done definition_finder");

        let mut state = State {
            path: module.clone(),
            table: root.table.clone(),
        };
        let ast = self.visit(storage, &mut state, &root.ast)?;
        Ok(Root {
            ast,
            entity: root.entity,
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
            if let Some(node) = node {
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
            search.pop(); // Strip the name off.
            debug!(
                "   not found {} at {}",
                expr.name.clone(),
                path_to_string(&search)
            );
            if search.is_empty() {
                return Err(TError::UnknownSymbol(
                    expr.name.clone(),
                    expr.get_info().clone(),
                    path_to_string(&state.path),
                ));
            }
            search.pop(); // Up one, go again.
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
            info: expr.get_info().clone(),
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
mod tests {
    use super::*;
    use crate::components::*;
    use crate::database::DBStorage;
    use crate::errors::TError;
    use crate::matcher::Matcher;
    use crate::pretty_assertions::assert_no_err;
    use crate::primitives::Prim;

    type Test = Result<(), TError>;

    static TEST_FN: &str = "test/prog.tk";
    fn test_path() -> Path {
        path!("test", "prog.tk")
    }

    fn test_path_and(suffix: Path) -> Path {
        let mut path = test_path();
        path.extend(suffix);
        path
    }

    fn find_definitions_entities(contents: &str) -> Result<(Entity, DBStorage), TError> {
        let mut storage = DBStorage::default();
        storage.set_file(TEST_FN, contents.to_owned());
        let module = storage.module_name(TEST_FN);
        let root = DefinitionFinder::process(&module, &mut storage)?;
        Ok((root.entity, storage))
    }

    #[test]
    fn matcher_use_local_definition() -> Test {
        let (_root, storage) = find_definitions_entities("x=23;x")?;
        assert_no_err(
            HasValue::new(Prim::I32(23))
                .one()
                .with(|n_23| {
                    Definition {
                        names: vec![path!("x")],
                        params: None,
                        implementations: vec![*n_23],
                        path: test_path(),
                    }
                    .one()
                })
                .with(|(_n_23, n_x)| {
                    SymbolRef {
                        name: path!("x"),
                        context: test_path(),
                        definition: Some(*n_x),
                    }
                    .one()
                })
                .run(&storage),
        )?;
        Ok(())
    }

    #[test]
    fn matcher_use_closest_definition() -> Test {
        let (_root, storage) = find_definitions_entities("x=23;y=(x=45;x)")?;
        assert_no_err(
            HasValue::new(Prim::I32(45))
                .one()
                .with(|n_23| {
                    Definition {
                        names: vec![path!("x")],
                        params: None,
                        implementations: vec![*n_23],
                        path: test_path_and(path!("y")),
                    }
                    .one()
                })
                .with(|(_n_23, n_x)| {
                    SymbolRef {
                        name: path!("x"),
                        context: test_path_and(path!("y")),
                        definition: Some(*n_x),
                    }
                    .one()
                })
                .run(&storage),
        )?;
        Ok(())
    }
}
