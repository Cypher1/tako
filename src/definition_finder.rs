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
        if self.debug > 1 {
            eprintln!("visiting sym {:?} {}", state.path.clone(), &expr.name);
        }
        let mut search = state.path.clone();
        if let Some(ScopeName::Anon(_)) = search.last() {
            // Jump out of first anon to avoid args using value of other args.
            search.pop();
        }
        loop {
            search.push(ScopeName::Named(expr.name.clone()));
            let node = state.table.find(&search);
            search.pop(); // Strip the name off, then replace it.
            match node {
                Some(node) => {
                    // The name is in scope.
                    search.push(node.value.name.clone());
                    eprintln!("FOUND {} at {:?}\n", expr.name.clone(), search.clone());
                    let mut res = expr.clone();
                    res.info.defined_at = Some(search);
                    return Ok(res.to_node());
                }
                None => {
                    eprintln!("   not found {} at {:?}", expr.name.clone(), search.clone());
                    if search.is_empty() {
                        return Err(TError::UnknownSymbol(expr.name.clone(), expr.get_info()));
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
            match self.visit_let(state, arg)? {
                Node::LetNode(let_node) => {
                    let mut defined_arg = let_node.clone();
                    let mut path = state.path.clone();
                    // Inject the value into the 'anon' stack frame.
                    path.push(ScopeName::Named(let_node.name));
                    eprintln!("defining arg at {:?}", path.clone());
                    defined_arg.info.defined_at = Some(path);
                    args.push(defined_arg)
                }
                _ => panic!("InternalError: definition_finder converted let node to other node."),
            }
        }
        let inner = Box::new(self.visit(state, &*expr.inner)?);
        state.path.pop();
        Ok(Apply {
            inner,
            args,
            info: expr.get_info(),
        }
        .to_node())
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        if self.debug > -1 {
            eprintln!("visiting {:?} {}", state.path.clone(), &expr.name);
        }
        let path_name = ScopeName::Named(expr.name.clone());
        state.path.push(path_name);
        let mut args = None;
        if let Some(e_args) = &expr.args {
            let mut arg_vec = vec![];
            for arg in e_args.iter() {
                let mut arg_path = state.path.clone();
                arg_path.push(ScopeName::Named(arg.name.clone()));
                let mut sym = arg.clone();
                eprintln!(
                    "visiting let arg {:?} {}",
                    arg_path.clone(),
                    &sym.name.clone()
                );
                sym.info.defined_at = Some(arg_path);
                arg_vec.push(sym);
            }
            args = Some(arg_vec);
        }
        let value = Box::new(self.visit(state, &expr.value)?);
        state.path.pop();
        Ok(Let {
            name: expr.name.clone(),
            args,
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
