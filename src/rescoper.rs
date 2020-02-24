use super::ast::*;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum ReScoperError {
    FailedParse(String, Info),
}

// Walks the AST interpreting it.
pub struct ReScoper {
    pub debug: i32,
    pub graph: CallGraph,
}

impl Default for ReScoper {
    fn default() -> ReScoper {
        ReScoper {
            debug: 0,
            graph: HashMap::new(),
        }
    }
}

// TODO: Return nodes.
type Res = Result<Node, ReScoperError>;

pub struct Namespace {
    name: ScopeName,
    info: Definition,
}

fn globals() -> Namespace {
    let mut defines = HashMap::new();
    defines.insert(Sym::new("true".to_string()), vec![]);
    defines.insert(Sym::new("false".to_string()), vec![]);
    Namespace {
        name: ScopeName::Unknown(0),
        info: Definition {
        requires: vec![],
        defines
        }
    }
}

pub struct State {
    stack: Vec<Namespace>,
    requires: Vec<Sym>,
    counter: i32, // used for ensuring uniqueness in new variables and scope names
}

impl Visitor<State, Node, Root, ReScoperError> for ReScoper {
    fn visit_root(&mut self, expr: &Root) -> Result<Root, ReScoperError> {
        let mut state = State {
            stack: vec![globals()],
            requires: vec![],
            counter: 1,
        };
        let mut res = self.visit(&mut state, &expr.ast)?.to_root();
        // Check requires
        if !state.requires.is_empty() {
            eprintln!("{:?} not declared", state.requires);
        }
        // TODO(cypher1): Avoid this copy (use swaps?)
        res.graph = self.graph.clone();
        Ok(res)
    }

    fn visit_sym(&mut self, state: &mut State, expr: &Sym) -> Res {
        let mut found = false;
        let mut info = expr.get_info();

        let mut space = vec![];
        for namespace in state.stack.iter().rev() {
            space.push(namespace.name.clone());
            for name in namespace.info.defines.iter() {
                if *name.0.name == expr.name {
                    // The name is in scope.
                    found = true;
                    info.defined_at = Some(space.clone());
                }
            }
        }

        let depth = if !found && !state.requires.iter().any(|r| r.name == expr.name) {
            state.requires.push(expr.clone());
            None
        } else {
            Some(space.len() as i32)
        };
        Ok(Sym {
            name: expr.name.clone(),
            depth,
            info,
        }
        .to_node())
    }

    fn visit_prim(&mut self, _state: &mut State, expr: &Prim) -> Res {
        Ok(expr.clone().to_node())
    }

    fn visit_apply(&mut self, state: &mut State, expr: &Apply) -> Res {
        state.stack.push(Namespace {
            name: ScopeName::Anon(state.counter),
            info: Definition{defines: HashMap::new(), requires: vec![]},
        });
        state.counter += 1;
        let mut args = vec![];
        for arg in expr.args.iter() {
            let new_arg = self.visit_let(state, arg)?;
            match new_arg {
                Node::LetNode(letter) => args.push(letter),
                letter => panic!(format!("Rescoper built {} from let", letter)),
            }
        }
        let inner = Box::new(self.visit(state, &*expr.inner)?);
        state.stack.pop();
        Ok(Apply {
            inner,
            args,
            info: expr.get_info(),
        }
        .to_node())
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        let mut space = vec![];
        for namespace in state.stack.iter() {
            space.push(namespace.name.clone());
        }
        {
            let frame = state.stack.last_mut().unwrap();
            frame.info.defines.insert(expr.to_sym(), space.clone());
        }

        state.stack.push(Namespace {
            name: ScopeName::Named(expr.name.clone(), state.counter),
            info: Definition {
                defines: HashMap::new(),
                requires: expr.args.clone().unwrap_or(vec![]),
            }
        });
        state.counter += 1;

        // Find new graph node
        let mut node = Definition {
            requires: vec![],
            defines: HashMap::new(),
        };
        std::mem::swap(&mut node.requires, &mut state.requires);
        let value = Box::new(self.visit(state, &expr.value)?);
        std::mem::swap(&mut node.requires, &mut state.requires);

        for req in state.requires.iter() {
            if !node.requires.iter().any(|r| r.name == req.name) {
                node.requires.push(req.clone());
            }
        }

        // Now that the variable has been defined we can use it.
        for req in node.requires.iter() {
            if !state.requires.iter().any(|r| r.name == req.name) {
                state.requires.push(req.clone());
            }
        }
        // Now that the variable has been defined we can use it.
        // if !recursive // frame.push(expr.name.clone());

        self.graph.insert(space.clone(), node.clone());
        if self.debug > 1 {
            eprintln!("visiting {:?}", space);
        }

        state.stack.pop();
        let mut info = expr.get_info();
        info.defined_at = Some(space);

        Ok(Let {
            name: expr.name.clone(),
            is_function: expr.is_function,
            args: Some(node.requires),
            value,
            info,
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
        Err(ReScoperError::FailedParse(
            expr.msg.to_string(),
            expr.get_info(),
        ))
    }
}

#[cfg(test)]
mod tests {}
