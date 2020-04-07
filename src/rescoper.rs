use super::ast::*;
use super::cli_options::Options;
use super::errors::TError;
use std::collections::HashMap;

// Walks the AST interpreting it.
pub struct ReScoper {
    pub debug: i32,
    pub graph: CallGraph,
}

// TODO: Return nodes.
type Res = Result<Node, TError>;

#[derive(Debug, Clone)]
pub struct Namespace {
    name: ScopeName,
    info: Definition,
}

fn globals() -> Namespace {
    let defines = HashMap::new();
    Namespace {
        name: ScopeName::Unknown(0),
        info: Definition {
            requires: vec![],
            defines,
        },
    }
}

#[derive(Debug, Clone)]
pub struct State {
    stack: Vec<Namespace>,
    requires: Vec<Sym>,
    counter: i32, // used for ensuring uniqueness in new variables and scope names
}

impl Visitor<State, Node, Root> for ReScoper {
    fn new(opts: &Options) -> ReScoper {
        ReScoper {
            debug: opts.debug,
            graph: HashMap::new(),
        }
    }

    fn visit_root(&mut self, expr: &Root) -> Result<Root, TError> {
        let mut state = State {
            stack: vec![globals()],
            requires: vec![],
            counter: 1,
        };
        let mut res = self.visit(&mut state, &expr.ast)?.to_root();
        // TODO(cypher1): Avoid this copy (use swaps?)
        res.graph = self.graph.clone();
        // Check requires
        if !state.requires.is_empty() {
            return Err(TError::FailedSymbolLookup(
                format!("{:?} not declared", state.requires),
                expr.ast.get_info(),
            ));
        }
        if self.debug > 3 {
            eprintln!("graph {:?}", res.graph.clone());
        }

        Ok(res)
    }

    fn visit_sym(&mut self, state: &mut State, expr: &Sym) -> Res {
        let mut info = expr.get_info();
        let mut found = false;

        for namespace in state.stack.iter().rev() {
            for name in namespace.info.defines.iter() {
                if *name.0.name == expr.name {
                    found = true;
                    // The name is in scope.
                    info.defined_at = Some((*name.1).clone());
                }
            }
        }

        if !found {
            eprintln!(
                "Warning: {} found but not previously defined.",
                expr.name.clone()
            );
            state.requires.push(expr.clone());
        }

        Ok(Sym {
            name: expr.name.clone(),
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
            info: Definition {
                defines: HashMap::new(),
                requires: vec![],
            },
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
        space.push(ScopeName::Named(expr.name.clone(), state.counter));
        state.counter += 1;

        if self.debug > 1 {
            eprintln!("visiting {:?}", space.clone());
        }

        let recursive = expr.is_function;
        if recursive {
            // Let this node and its siblings call this node.
            match state.stack.last_mut() {
                Some(frame) => {
                    frame.info.defines.insert(expr.to_sym(), space.clone());
                }
                None => panic!("here1"),
            }
        }

        // Find new graph node
        let mut node = Definition {
            defines: HashMap::new(),
            requires: vec![],
        };

        // Consider the function arguments defined in this scope.
        for mut arg in expr.args.clone().unwrap_or_else(|| vec![]) {
            let mut arg_space = space.clone();
            arg_space.push(ScopeName::Named(arg.name.clone(), state.counter));
            state.counter += 1;
            arg.info.defined_at = Some(arg_space.clone());
            node.requires.push(arg.clone());
            node.defines.insert(arg, arg_space);
        }

        // Push the scope onto the stack.
        state.stack.push(Namespace {
            name: ScopeName::Named(expr.name.clone(), state.counter),
            info: node,
        });
        state.counter += 1;

        // Examine the body of the let binding.
        let value = Box::new(self.visit(state, &expr.value)?);

        // Finish the scope. Retrieve any information from.the stack.
        node = state.stack.pop().unwrap().info;
        // Now that the variable has been defined we can use it.
        if !recursive {
            // Let this node and its siblings call this node.
            match state.stack.last_mut() {
                Some(frame) => {
                    frame.info.defines.insert(expr.to_sym(), space.clone());
                }
                None => panic!("here1"),
            }
        }

        self.graph.insert(space.clone(), node.clone());

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
        // Find new graph node
        let mut left_node = Definition {
            requires: vec![],
            defines: HashMap::new(),
        };
        std::mem::swap(&mut left_node.requires, &mut state.requires);
        let left = Box::new(self.visit(state, &expr.left)?);
        std::mem::swap(&mut left_node.requires, &mut state.requires);

        // Find new graph node
        let mut right_node = Definition {
            requires: vec![],
            defines: HashMap::new(),
        };
        std::mem::swap(&mut right_node.requires, &mut state.requires);
        let right = Box::new(self.visit(state, &expr.right)?);
        std::mem::swap(&mut right_node.requires, &mut state.requires);

        for req in right_node.requires.iter() {
            if expr.name != "?" || left_node.requires.iter().any(|l| l.name == req.name) {
                state.requires.push(req.clone());
            }
        }
        for req in left_node.requires.iter() {
            if expr.name != "?" || right_node.requires.iter().any(|l| l.name == req.name) {
                state.requires.push(req.clone());
            }
        }

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
