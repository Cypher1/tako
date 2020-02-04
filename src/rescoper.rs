use super::ast::*;
#[derive(Debug)]
#[derive(PartialEq)]
pub enum ReScoperError {
    FailedParse(String, Info),
}

// Walks the AST interpreting it.
pub struct ReScoper {
}

impl Default for ReScoper {
    fn default() -> ReScoper {
        ReScoper {}
    }
}

fn globals() -> Vec<String> {
    vec!{
        "true".to_string(),
        "false".to_string(),
    }
}

// TODO: Return nodes.
type Res = Result<Node, ReScoperError>;
type Frame = Vec<String>;
pub struct State {
    stack: Vec<Frame>,
    requires: Vec<String>,
}
impl Visitor<State, Node, Node, ReScoperError> for ReScoper {

    fn visit_root(&mut self, expr: &Node) -> Res {
        let mut state = State{stack: vec![globals()], requires: vec![]};
        let res = self.visit(&mut state, expr)?;
        // Check requires
        Ok(res)
    }

    fn visit_sym(&mut self, state: &mut State, expr: &Sym) -> Res {
        let mut look_depth = 0;
        let mut found = false;

        'walk_stack: for frame in state.stack.iter() {
            for name in frame.iter() {
                if *name == expr.name {
                    // The name is in scope.
                    found = true;
                    break 'walk_stack;
                }
            }
            look_depth += 1;
        }

        let mut depth = Some(look_depth);
        if !found && !state.requires.contains(&expr.name) {
            state.requires.push(expr.name.clone());
            depth = None;
        }
        Ok(Sym {name: expr.name.clone(), depth, info: expr.get_info()}.to_node())
    }

    fn visit_prim(&mut self, _state: &mut State, expr: &Prim) -> Res {
        Ok(expr.clone().to_node())
    }

    fn visit_apply(&mut self, state: &mut State, expr: &Apply) -> Res {
        let mut args = vec![];
        for arg in expr.args.iter() {
            let new_arg = self.visit_let(state, arg)?;
            match new_arg {
                Node::LetNode(letter) => args.push(letter),
                letter => panic!(format!("Rescoper built {} from let", letter)),
            }
        }
        let inner = Box::new(self.visit(state, &*expr.inner)?);
        Ok(Apply{inner, args, info: expr.get_info()}.to_node())
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        let frame = state.stack.last_mut().unwrap();
        frame.push("self".to_string()); // For recursion

        let mut requires = vec![];
        std::mem::swap(&mut requires, &mut state.requires);
        let value = Box::new(self.visit(state, &expr.value)?);
        std::mem::swap(&mut requires, &mut state.requires);

        for name in requires.iter() {
            if !state.requires.contains(&name) {
                state.requires.push(name.to_string());
            }
        }
        // Now that the variable has been defined we can use it.
        state.stack.last_mut().unwrap().push(expr.name.clone());

        Ok(Let{name: expr.name.clone(), requires: Some(requires), value, info: expr.get_info()}.to_node())
    }

    fn visit_un_op(&mut self, state: &mut State, expr: &UnOp) -> Res {
        let inner = Box::new(self.visit(state, &expr.inner)?);
        Ok(UnOp{name: expr.name.clone(), inner, info: expr.get_info()}.to_node())
    }

    fn visit_bin_op(&mut self, state: &mut State, expr: &BinOp) -> Res {
        let left = Box::new(self.visit(state, &expr.left)?);
        let right = Box::new(self.visit(state, &expr.right)?);
        Ok(BinOp{name: expr.name.clone(), left, right, info: expr.get_info()}.to_node())
    }

    fn handle_error(&mut self, state: &mut State, expr: &Err) -> Res {
        Err(ReScoperError::FailedParse(expr.msg.to_string(), expr.get_info()))
    }
}

#[cfg(test)]
mod tests {
}
