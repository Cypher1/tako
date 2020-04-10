use super::ast::*;
use super::cli_options::Options;
use super::errors::TError;
use super::tree::{to_hash_root, HashTree};

// Walks the AST interpreting it.
pub struct SymbolTableBuilder {
    pub debug: i32,
}

// TODO: Return nodes.
type Res = Result<(), TError>;

#[derive(Debug, Clone)]
pub struct State {
    pub table: Table,
    pub path: Vec<ScopeName>,
    pub counter: i32, // used for ensuring uniqueness in new variables and scope names
    // TODO: Store an hashmap/array of definitions by some id
    // Each definition can then reference its children.
}

impl Table {
    pub fn find<'a>(
        self: &'a mut Table,
        path: &[ScopeName],
    ) -> Option<&'a mut Table> {
        // eprintln!("find in {:?}", self.value);
        if path.is_empty() {
            return Some(self);
        }
        if let Some(child) = self.children.get_mut(&path[0]) {
            child.find(&path[1..])
        } else {
            None
        }
    }

    fn get_child<'a>(self: &'a mut Table, find: &ScopeName) -> &'a mut HashTree<ScopeName, Symbol> {
        self.children.entry(find.clone()).or_insert(
            to_hash_root(Symbol {
                name: find.clone(),
                info: Entry::default(),
            }))
    }

    pub fn get<'a>(self: &'a mut Table, path: &[ScopeName]) -> &'a mut HashTree<ScopeName, Symbol> {
        if path.is_empty() {
            return self;
        }
        self.get_child(&path[0]).get(&path[1..])
    }
}

impl State {
    pub fn get_unique_id(self: &mut State) -> i32 {
        let c = self.counter;
        self.counter += 1;
        c
    }
}

impl Visitor<State, (), Root> for SymbolTableBuilder {
    fn new(opts: &Options) -> SymbolTableBuilder {
        SymbolTableBuilder { debug: opts.debug }
    }

    fn visit_root(&mut self, expr: &Root) -> Result<Root, TError> {
        let mut state = State {
            table: to_hash_root(Symbol {
                name: ScopeName::Named("project".to_string()), // TODO(cypher1): Pass around the project name.
                info: Entry::default(),
            }),
            path: vec![],
            counter: 1,
        };
        self.visit(&mut state, &expr.ast)?;
        if self.debug > 3 {
            eprintln!("graph {:?}", Some(state.table.clone()));
        }

        eprintln!("table: {:?}", state.table.clone());

        Ok(Root {
            ast: expr.ast.clone(),
            table: Some(state.table),
        })
    }

    fn visit_sym(&mut self, _state: &mut State, _expr: &Sym) -> Res {
        Ok(())
    }

    fn visit_prim(&mut self, _state: &mut State, _expr: &Prim) -> Res {
        Ok(())
    }

    fn visit_apply(&mut self, state: &mut State, expr: &Apply) -> Res {
        let arg_scope_name = ScopeName::Anon(state.get_unique_id());

        state.path.push(arg_scope_name);
        for arg in expr.args.iter() {
            self.visit_let(state, arg)?;
        }
        self.visit(state, &*expr.inner)?;
        state.path.pop();

        Ok(())
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        let let_name = ScopeName::Named(expr.name.clone());
        if self.debug > 1 {
            eprintln!("visiting {:?} {}", state.path.clone(), &let_name);
        }

        // Visit definition.
        state.path.push(let_name);
        state.table.get(&state.path);

        // Consider the function arguments defined in this scope.
        for arg in expr.args.clone().unwrap_or_else(|| vec![]) {
            let mut arg_path = state.path.clone();
            arg_path.push(ScopeName::Named(arg.name));
            state.table.get(&arg_path);
        }

        self.visit(state, &expr.value)?;
        state.path.pop();

        Ok(())
    }

    fn visit_un_op(&mut self, state: &mut State, expr: &UnOp) -> Res {
        self.visit(state, &expr.inner)
    }

    fn visit_bin_op(&mut self, state: &mut State, expr: &BinOp) -> Res {
        self.visit(state, &expr.left)?;
        self.visit(state, &expr.right)
    }

    fn handle_error(&mut self, _state: &mut State, expr: &Err) -> Res {
        Err(TError::FailedParse(expr.msg.to_string(), expr.get_info()))
    }
}

#[cfg(test)]
mod tests {}
