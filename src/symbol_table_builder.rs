use crate::ast::*;
use crate::database::Compiler;
use crate::errors::TError;
use crate::tree::{to_hash_root, HashTree};

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

impl Table {
    pub fn new() -> Table {
       to_hash_root(Entry { uses: vec![] }) 
    }
    
    pub fn find<'a>(self: &'a mut Table, path: &[Symbol]) -> Option<&'a mut Table> {
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

    fn get_child<'a>(self: &'a mut Table, find: &Symbol) -> &'a mut HashTree<Symbol, Entry> {
        self.children
            .entry(find.clone())
            .or_insert(to_hash_root(Entry { uses: vec![] }))
    }

    pub fn get<'a>(self: &'a mut Table, path: &[Symbol]) -> &'a mut HashTree<Symbol, Entry> {
        if path.is_empty() {
            return self;
        }
        self.get_child(&path[0]).get(&path[1..])
    }
}

impl Visitor<State, Node, Root, Node> for SymbolTableBuilder {
    fn visit_root(&mut self, db: &dyn Compiler, expr: &Node) -> Result<Root, TError> {
        let mut state = State {
            table: to_hash_root(Entry { uses: vec![] }),
            path: vec![],
        };

        // TODO: Inject needs for bootstrapping here (e.g. import function).
        let println_path = [Symbol::Named("println".to_string())];
        state.table.get(&println_path);

        Ok(Root {
            ast: self.visit(db, &mut state, &expr)?,
            table: state.table,
        })
    }

    fn visit_sym(&mut self, db: &dyn Compiler, _state: &mut State, expr: &Sym) -> Res {
        Ok(expr.clone().to_node())
    }

    fn visit_prim(&mut self, db: &dyn Compiler, _state: &mut State, expr: &Prim) -> Res {
        Ok(expr.clone().to_node())
    }

    fn visit_apply(&mut self, db: &dyn Compiler, state: &mut State, expr: &Apply) -> Res {
        let arg_scope_name = Symbol::Anon();

        state.path.push(arg_scope_name);
        let mut args = Vec::new();
        for arg in expr.args.iter() {
            args.push(match self.visit_let(db, state, arg)? {
                Node::LetNode(let_node) => Ok(let_node),
                node => Err(TError::InternalError(
                    "Symbol table builder converted let into non let".to_owned(),
                    node,
                )),
            }?);
        }
        let inner = Box::new(self.visit(db, state, &*expr.inner)?);
        state.path.pop();

        Ok(Apply {
            inner,
            args,
            info: expr.get_info(),
        }
        .to_node())
    }

    fn visit_let(&mut self, db: &dyn Compiler, state: &mut State, expr: &Let) -> Res {
        let let_name = Symbol::Named(expr.name.clone());
        if db.debug() > 1 {
            eprintln!("visiting {:?} {}", state.path.clone(), &let_name);
        }

        // Visit definition.
        let mut info = expr.get_info();
        state.path.push(let_name);
        info.defined_at = Some(state.path.clone());
        state.table.get(&state.path);

        // Consider the function arguments defined in this scope.
        for arg in expr.args.clone().unwrap_or_else(|| vec![]) {
            let mut arg_path = state.path.clone();
            arg_path.push(Symbol::Named(arg.name));
            state.table.get(&arg_path);
        }

        let value = Box::new(self.visit(db, state, &expr.value)?);
        state.path.pop();

        Ok(Let {
            name: expr.name.clone(),
            value,
            args: expr.args.clone(),
            is_function: expr.is_function,
            info,
        }
        .to_node())
    }

    fn visit_un_op(&mut self, db: &dyn Compiler, state: &mut State, expr: &UnOp) -> Res {
        let inner = Box::new(self.visit(db, state, &expr.inner)?);
        Ok(UnOp {
            name: expr.name.clone(),
            inner,
            info: expr.get_info(),
        }
        .to_node())
    }

    fn visit_bin_op(&mut self, db: &dyn Compiler, state: &mut State, expr: &BinOp) -> Res {
        let left = Box::new(self.visit(db, state, &expr.left)?);
        let right = Box::new(self.visit(db, state, &expr.right)?);
        Ok(BinOp {
            name: expr.name.clone(),
            left,
            right,
            info: expr.get_info(),
        }
        .to_node())
    }

    fn visit_built_in(&mut self, db: &dyn Compiler, _state: &mut State, expr: &String) -> Res {
        Ok(Node::BuiltIn(expr.to_string()))
    }

    fn handle_error(&mut self, db: &dyn Compiler, _state: &mut State, expr: &Err) -> Res {
        Err(TError::FailedParse(expr.msg.to_string(), expr.get_info()))
    }
}

#[cfg(test)]
mod tests {}
