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
type State = Vec<Frame>;
impl Visitor<State, Node, Node, ReScoperError> for ReScoper {

    fn visit_root(&mut self, expr: &Node) -> Res {
        let mut state = vec![globals()];
        self.visit(&mut state, expr)
    }

    fn visit_sym(&mut self, state: &mut State, expr: &Sym) -> Res {
        let mut depth = 0;
        // let mut found = false;
        'walk_stack: for frame in state.iter() {
            for name in frame.iter() {
                if *name == expr.name {
                    // The name is in scope.
                    // found = true;
                    break 'walk_stack;
                }
            }
            depth += 1;
        }
        Ok(Sym {name: expr.name.clone(), depth: Some(depth), info: expr.get_info()}.to_node())
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
        let frame = state.last_mut().unwrap();
        frame.push("self".to_string()); // For recursion

        let value = Box::new(self.visit(state, &expr.value)?);
        match state.last_mut() {
            Some(frame) => {
                // Now that the variable has been defined we can use it.
                frame.push(expr.name.clone());
            },
            None => {}
        }
        Ok(Let{name: expr.name.clone(), value, info: expr.get_info()}.to_node())
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
