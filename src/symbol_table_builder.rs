use super::ast::*;
use super::database::Compiler;
use super::errors::TError;
use super::tree::{to_hash_root, HashTree};

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
        // TODO: This smells a bit?
        to_hash_root(Entry::default())
    }

    pub fn find<'a>(self: &'a Table, path: &[Symbol]) -> Option<&'a Table> {
        // eprintln!("find in {:?}", self.value);
        if path.is_empty() {
            return Some(self);
        }
        if let Some(child) = self.children.get(&path[0]) {
            child.find(&path[1..])
        } else {
            None
        }
    }

    pub fn find_mut<'a>(self: &'a mut Table, path: &[Symbol]) -> Option<&'a mut Table> {
        // eprintln!("find in {:?}", self.value);
        if path.is_empty() {
            return Some(self);
        }
        if let Some(child) = self.children.get_mut(&path[0]) {
            child.find_mut(&path[1..])
        } else {
            None
        }
    }

    fn get_child_mut<'a>(self: &'a mut Table, find: &Symbol) -> &'a mut HashTree<Symbol, Entry> {
        self.children.entry(find.clone()).or_insert_with(Table::new)
    }

    pub fn get_mut<'a>(self: &'a mut Table, path: &[Symbol]) -> &'a mut HashTree<Symbol, Entry> {
        if path.is_empty() {
            return self;
        }
        self.get_child_mut(&path[0]).get_mut(&path[1..])
    }
}

impl Visitor<State, Node, Root, Path> for SymbolTableBuilder {
    fn visit_root(&mut self, db: &dyn Compiler, module: &Path) -> Result<Root, TError> {
        let expr = &db.parse_file(module.clone())?;
        if db.debug() > 0 {
            eprintln!("building symbol table for file... {:?}", &module);
        }

        let mut table = Table::new();
        let mut main_at = module.clone();
        main_at.push(Symbol::new("main".to_string()));

        let main_symb = table.get_mut(&main_at);
        main_symb.value.uses.insert(module.clone());

        // Add in the globals here!
        // TODO: Inject needs for bootstrapping here (e.g. import function).
        let globals: Vec<Path> = ["print", "argc", "argv"]
            .iter()
            .map(|x| vec![Symbol::new(x.to_string())])
            .collect();
        for global in globals {
            table.get_mut(&global);
        }

        let mut state = State {
            table,
            path: module.clone(),
        };

        if db.debug() > 0 {
            eprintln!("table: {:?}", state.table);
        }

        Ok(Root {
            ast: self.visit(db, &mut state, &expr)?,
            table: state.table,
        })
    }

    fn visit_sym(&mut self, _db: &dyn Compiler, _state: &mut State, expr: &Sym) -> Res {
        Ok(expr.clone().to_node())
    }

    fn visit_prim(&mut self, _db: &dyn Compiler, _state: &mut State, expr: &Prim) -> Res {
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
        let let_name = Symbol::new(expr.name.clone());
        if db.debug() > 1 {
            eprintln!("visiting {:?} {}", state.path.clone(), &let_name);
        }

        // Visit definition.
        let mut info = expr.get_info();
        state.path.push(let_name);
        info.defined_at = Some(state.path.clone());
        state.table.get_mut(&state.path);

        // Consider the function arguments defined in this scope.
        let args = if let Some(e_args) = &expr.args {
            let mut args = vec![];
            for arg in e_args.iter() {
                let mut arg_path = state.path.clone();
                arg_path.push(Symbol::new(arg.name.clone()));
                state.table.get_mut(&arg_path);
                let mut sym = arg.clone();
                if db.debug() > 1 {
                    eprintln!(
                        "visiting let arg {:?} {}",
                        arg_path.clone(),
                        &sym.name.clone()
                    );
                }
                sym.info.defined_at = Some(arg_path);
                args.push(sym);
            }
            Some(args)
        } else {
            None
        };

        let value = Box::new(self.visit(db, state, &expr.value)?);
        state.path.pop();

        Ok(Let {
            name: expr.name.clone(),
            value,
            args,
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

    fn handle_error(&mut self, _db: &dyn Compiler, _state: &mut State, expr: &Err) -> Res {
        Err(TError::FailedParse(expr.msg.to_string(), expr.get_info()))
    }
}

#[cfg(test)]
mod tests {}
