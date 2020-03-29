use super::ast::*;
use super::symbol_table_builder::State;

#[derive(Debug, PartialEq)]
pub enum DefinitionFinderError {
    FailedParse(String, Info),
    FailedSymbolLookup(String, Info),
}

// Walks the AST interpreting it.
pub struct DefinitionFinder {
    pub debug: i32,
}

impl Default for DefinitionFinder {
    fn default() -> DefinitionFinder {
        DefinitionFinder { debug: 0 }
    }
}

// TODO: Return nodes.
type Res = Result<Node, DefinitionFinderError>;

#[derive(Debug, Clone)]
pub struct Namespace {
    name: ScopeName,
    info: Entry,
}

impl Visitor<State, Node, Root, DefinitionFinderError> for DefinitionFinder {
    fn visit_root(&mut self, expr: &Root) -> Result<Root, DefinitionFinderError> {
        let mut state = State {
            path: vec![],
            table: expr
                .table
                .clone()
                .expect("Definition finder requires an initialized symbol table"),
            counter: 1,
        };
        let ast = self.visit(&mut state, &expr.ast)?;
        if self.debug > 3 {
            eprintln!("table {:?}", state.table.clone());
        }
        Ok(Root {
            ast,
            table: Some(state.table),
        })
    }

    fn visit_sym(&mut self, state: &mut State, expr: &Sym) -> Res {
        let mut search = state.path.clone();
        let mut res = expr.clone();
        loop {
            // TODO(cypher1): Matching with non-id'd data
            search.push(ScopeName::Named(expr.name.clone(), -1));
            let node = state.table.find(&search);
            search.pop(); // Strip the name off, then replace it.
            match node {
                Some(node) => {
                    // The name is in scope.
                    search.push(node.value.name.clone());
                    res.info.defined_at = Some(search);
                    break;
                }
                None => {
                    if search.is_empty() {
                        return Err(DefinitionFinderError::FailedSymbolLookup(
                            expr.name.clone(),
                            res.info,
                        ));
                    }
                    search.pop(); // Up one, go again.
                }
            }
        }
        Ok(res.to_node())
    }

    fn visit_prim(&mut self, _state: &mut State, expr: &Prim) -> Res {
        Ok(expr.clone().to_node())
    }

    fn visit_apply(&mut self, state: &mut State, expr: &Apply) -> Res {
        let id = state.get_unique_id();
        state.path.push(ScopeName::Anon(id));
        let mut args = vec![];
        for arg in expr.args.iter() {
            args.push(self.visit_let(state, arg)?);
        }
        let inner = Box::new(self.visit(state, &*expr.inner)?);
        state.path.pop();
        Ok(Apply {
            inner,
            args: expr.args.clone(),
            info: expr.get_info(),
        }
        .to_node())
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        let mut info = expr.get_info();
        eprintln!("defining {} at {:?}", expr.name.clone(), state.path.clone());
        let id = state.get_unique_id();
        let path_name = ScopeName::Named(expr.name.clone(), id);
        let mut path = state.path.clone();
        path.push(path_name.clone());
        info.defined_at = Some(path);
        if expr.is_function {
            state.path.push(path_name);
        }
        let value = Box::new(self.visit(state, &expr.value)?);
        state.path.pop();
        Ok(Let {
            name: expr.name.clone(),
            args: expr.args.clone(),
            value,
            is_function: expr.is_function,
            info: expr.get_info(),
        }
        .to_node())
    }

    fn visit_un_op(&mut self, state: &mut State, expr: &UnOp) -> Res {
        let inner = Box::new(self.visit(state, &expr.inner)?);
        Ok(UnOp {
            name: expr.name.clone(),
            inner,
            info: expr.get_info(),
        }
        .to_node())
    }

    fn visit_bin_op(&mut self, state: &mut State, expr: &BinOp) -> Res {
        let left = Box::new(self.visit(state, &expr.left)?);
        let right = Box::new(self.visit(state, &expr.right)?);
        Ok(BinOp {
            name: expr.name.clone(),
            left,
            right,
            info: expr.get_info(),
        }
        .to_node())
    }

    fn handle_error(&mut self, _state: &mut State, expr: &Err) -> Res {
        Err(DefinitionFinderError::FailedParse(
            expr.msg.to_string(),
            expr.get_info(),
        ))
    }
}

#[cfg(test)]
mod tests {}
