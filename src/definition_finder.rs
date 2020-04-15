use super::ast::*;
use super::cli_options::Options;
use super::errors::TError;
use super::symbol_table_builder::State;

// Walks the AST interpreting it.
pub struct DefinitionFinder {
    pub debug: i32,
}

// TODO: Return nodes.
type Res = Result<Node, TError>;

#[derive(Debug, Clone)]
pub struct Namespace {
    name: ScopeName,
    info: Entry,
}

impl Visitor<State, Node, Root> for DefinitionFinder {
    fn new(opts: &Options) -> DefinitionFinder {
        DefinitionFinder { debug: opts.debug }
    }

    fn visit_root(&mut self, expr: &Root) -> Result<Root, TError> {
        let mut state = State {
            path: vec![],
            table: expr
                .table
                .clone()
                .expect("Definition finder requires an initialized symbol table"),
            counter: 1,
        };
        let ast = self.visit(&mut state, &expr.ast)?;
        Ok(Root {
            ast,
            table: Some(state.table),
        })
    }

    fn visit_sym(&mut self, state: &mut State, expr: &Sym) -> Res {
        let path: Vec<String> = state.path.iter().map(|p| format!("{}", p)).collect();
        eprintln!("\nSYM {:?} in {:?}", expr.name.clone(), path.join("/"));
        let mut search = state.path.clone();
        let mut res = expr.clone();
        loop {
            // TODO(cypher1): Matching with non-id'd data
            search.push(ScopeName::Named(expr.name.clone()));
            let node = state.table.find(&search);
            search.pop(); // Strip the name off, then replace it.
            match node {
                Some(node) => {
                    // The name is in scope.
                    search.push(node.value.name.clone());
                    eprintln!("FOUND {} at {:?}", expr.name.clone(), search.clone());
                    res.info.defined_at = Some(search);
                    return Ok(res.to_node());
                }
                None => {
                    eprintln!("   not found {} at {:?}", expr.name.clone(), search.clone());
                    if search.is_empty() {
                        return Err(TError::UnknownSymbol(expr.name.clone(), res.info));
                    }
                    search.pop(); // Up one, go again.
                }
            }
        }
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
        let path_name = ScopeName::Named(expr.name.clone());
        state.path.push(path_name);
        let mut expr_args = None;
        if let Some(e_args) = &expr.args {
            let mut args = vec![];
            for arg in e_args.iter() {
                let mut arg_path = state.path.clone();
                arg_path.push(ScopeName::Named(arg.name.clone()));
                let sym = arg.clone();
                sym.get_info().defined_at = Some(arg_path);
                args.push(sym);
            }
            expr_args = Some(args);
        }
        let value = Box::new(self.visit(state, &expr.value)?);
        state.path.pop();
        Ok(Let {
            name: expr.name.clone(),
            args: expr_args,
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
        Err(TError::FailedParse(expr.msg.to_string(), expr.get_info()))
    }
}

#[cfg(test)]
mod tests {}
