use crate::ast::*;
use crate::database::Compiler;
use crate::errors::TError;
use crate::symbol_table_builder::State;

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

impl Visitor<State, Node, Root, Path> for DefinitionFinder {
    fn visit_root(&mut self, db: &dyn Compiler, module: &Path) -> Result<Root, TError> {
        let expr = db.build_symbol_table(module.clone())?;
        if db.debug() > 0 {
            eprintln!("looking up definitions in file... {:?}", &module);
        }
        let mut state = State {
            path: module.clone(),
            table: expr.table.clone(),
        };
        let ast = self.visit(db, &mut state, &expr.ast)?;
        Ok(Root {
            ast,
            table: state.table,
        })
    }

    fn visit_sym(&mut self, db: &dyn Compiler, state: &mut State, expr: &Sym) -> Res {
        if db.debug() > 1 {
            eprintln!("visiting sym {:?} {}", state.path.clone(), &expr.name);
        }
        let mut search: Vec<Symbol> = state.path.clone();
        loop {
            if let Some(Symbol::Anon()) = search.last() {
                search.pop(); // Cannot look inside an 'anon'.
            }
            search.push(Symbol::new(expr.name.clone()));
            let node = state.table.find_mut(&search);
            match node {
                Some(node) => {
                    node.value.uses.insert(state.path.clone());
                    if db.debug() > 1 {
                        eprintln!("FOUND {} at {:?}\n", expr.name.clone(), search.clone());
                    }
                    let mut res = expr.clone();
                    res.info.defined_at = Some(search);
                    return Ok(res.to_node());
                }
                None => {
                    search.pop(); // Strip the name off.
                    if db.debug() > 1 {
                        eprintln!("   not found {} at {:?}", expr.name.clone(), search.clone());
                    }
                    if search.is_empty() {
                        return Err(TError::UnknownSymbol(expr.name.clone(), expr.get_info(), state.path.clone()));
                    }
                    search.pop(); // Up one, go again.
                }
            }
        }
    }

    fn visit_prim(&mut self, _db: &dyn Compiler, _state: &mut State, expr: &Prim) -> Res {
        Ok(expr.clone().to_node())
    }

    fn visit_apply(&mut self, db: &dyn Compiler, state: &mut State, expr: &Apply) -> Res {
        state.path.push(Symbol::Anon());
        let mut args = vec![];
        for arg in expr.args.iter() {
            args.push(match self.visit_let(db, state, &arg)? {
                Node::LetNode(arg) => arg,
                arg => panic!("Definition finder converted let node to {:?}", arg),
            });
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
        if db.debug() > 1 {
            eprintln!("visiting {:?} {}", state.path.clone(), &expr.name);
        }
        let mut info = expr.info.clone();
        info.defined_at = Some(state.path.clone());
        let path_name = Symbol::new(expr.name.clone());
        state.path.push(path_name);
        let value = Box::new(self.visit(db, state, &expr.value)?);
        state.path.pop();
        Ok(Let {
            name: expr.name.clone(),
            args: expr.args.clone(),
            value,
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
