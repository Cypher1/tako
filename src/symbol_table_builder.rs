use super::ast::*;
use super::tree::to_root;
use super::tree::Tree;

#[derive(Debug, PartialEq)]
pub enum SymbolTableBuilderError {
    FailedParse(String, Info),
}

// Walks the AST interpreting it.
pub struct SymbolTableBuilder {
    pub debug: i32,
}

impl Default for SymbolTableBuilder {
    fn default() -> SymbolTableBuilder {
        SymbolTableBuilder { debug: 0 }
    }
}

// TODO: Return nodes.
type Res = Result<(), SymbolTableBuilderError>;

#[derive(Debug, Clone)]
pub struct State {
    pub table: Tree<Symbol>,
    pub path: Vec<ScopeName>,
    pub counter: i32, // used for ensuring uniqueness in new variables and scope names
}

fn matches(left: &ScopeName, right: &ScopeName) -> bool {
    use ScopeName::*;
    match (left, right) {
        (Anon(n), Anon(m)) => n == m, // can only be accessed from inside.
        (Anon(_), _) => false,
        (_, Anon(_)) => false,
        (Unknown(_), _) => true,
        (_, Unknown(_)) => true,
        (Named(n, c), Named(m, k)) => n == m && (c == k || *c == -1 || *k == -1),
    }
}

impl Tree<Symbol> {
    fn find_child(self: &mut Tree<Symbol>, find: &ScopeName) -> Option<&mut Tree<Symbol>> {
        let n = self
            .children
            .iter()
            .position(|child| matches(&child.value.name, find));
        if let Some(n) = n {
            Some(&mut self.children[n])
        } else {
            None
        }
    }

    pub fn find<'a>(
        self: &'a mut Tree<Symbol>,
        path: &[ScopeName],
    ) -> Option<&'a mut Tree<Symbol>> {
        if path.is_empty() {
            return Some(self);
        }
        if let Some(child) = self.find_child(&path[0]) {
            child.find(&path[1..])
        } else {
            None
        }
    }

    fn get_child(self: &mut Tree<Symbol>, find: &ScopeName) -> &mut Tree<Symbol> {
        let mut n = self
            .children
            .iter()
            .position(|child| matches(&child.value.name, find));
        if n == None {
            let leaf = to_root(Symbol {
                name: find.clone(),
                info: Entry::default(),
            });
            self.children.push(leaf);
            n = Some(self.children.len() - 1);
        }
        &mut self.children[n.unwrap()]
    }

    pub fn get<'a>(self: &'a mut Tree<Symbol>, path: &[ScopeName]) -> &'a mut Tree<Symbol> {
        if path.is_empty() {
            return self;
        }
        self.get_child(&path[0]).get(&path[1..])
    }

    pub fn insert(self: &mut Tree<Symbol>, path: &[ScopeName], node: Tree<Symbol>) {
        self.get(path).children.push(node);
    }
}

impl State {
    pub fn get_unique_id(self: &mut State) -> i32 {
        let c = self.counter;
        self.counter += 1;
        c
    }
}

impl Visitor<State, (), Root, SymbolTableBuilderError> for SymbolTableBuilder {
    fn visit_root(&mut self, expr: &Root) -> Result<Root, SymbolTableBuilderError> {
        let mut state = State {
            table: to_root(Symbol {
                name: ScopeName::Named("project".to_string(), 0), // TODO(cypher1): Pass around the project name.
                info: Entry::default(),
            }),
            path: vec![],
            counter: 1,
        };
        self.visit(&mut state, &expr.ast)?;
        if self.debug > 3 {
            eprintln!("graph {:?}", Some(state.table.clone()));
        }

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
        let arg_scope_name = ScopeName::Unknown(state.get_unique_id());

        state.table.children.push(to_root(Symbol {
            name: arg_scope_name.clone(),
            info: Entry::default(),
        }));

        state.path.push(arg_scope_name);
        for arg in expr.args.iter() {
            self.visit_let(state, arg)?;
        }
        self.visit(state, &*expr.inner)?;
        state.path.pop();

        Ok(())
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        let let_name = ScopeName::Named(expr.name.clone(), state.get_unique_id());

        if self.debug > 1 {
            eprintln!("visiting {:?}", state.path.clone());
        }

        let letnode = to_root(Symbol {
            name: let_name.clone(),
            info: Entry::default(),
        });
        state.table.insert(&state.path, letnode);

        // Visit definition.
        state.path.push(let_name);

        // Consider the function arguments defined in this scope.
        for arg in expr.args.clone().unwrap_or_else(|| vec![]) {
            let arg_name = ScopeName::Named(arg.name, state.get_unique_id());
            let arg_node = to_root(Symbol {
                name: arg_name.clone(),
                info: Entry::default(),
            });
            let mut arg_path = state.path.clone();
            arg_path.push(arg_name);
            state.table.insert(&arg_path, arg_node);
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
        Err(SymbolTableBuilderError::FailedParse(
            expr.msg.to_string(),
            expr.get_info(),
        ))
    }
}

#[cfg(test)]
mod tests {}
