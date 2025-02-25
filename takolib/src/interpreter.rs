use crate::ast::string_interner::{StrId, StringInterner};
use crate::ast::{Ast, Call, Definition, LiteralId, Node, NodeData, NodeId, OpId};
use crate::error::TError;
use crate::parser::semantics::Literal;
use crate::parser::tokens::Symbol;
use crate::primitives::Prim;
use log::trace;
use std::collections::HashMap;
use std::convert::TryInto;
use std::path::Path;

type Name = StrId;

#[derive(Default, Clone, Debug)]
struct State {
    names: HashMap<Name, Prim>,
}

impl State {
    fn get_binding(&self, name: &Name) -> Option<&Prim> {
        self.names.get(name)
    }
}

struct Ctx<'a> {
    ast: &'a Ast,
    literals: &'a StringInterner,
    state: State,
}

pub fn run(path: &Path, ast: &Ast, root: Option<NodeId>) -> Result<Prim, TError> {
    let start = if let Some(root) = root {
        root
    } else if ast.roots.len() == 1 {
        ast.roots[0]
    } else if ast.roots.is_empty() {
        return Err(TError::InternalError {
            message: format!(
                "Ambiguous run command: No root found for {path}",
                path = path.display()
            ),
            location: None,
        });
    } else {
        return Err(TError::InternalError {
            message: format!(
                "Ambiguous run command: Multiple roots found for {path}",
                path = path.display()
            ),
            location: None,
        });
    };
    let mut ctx = Ctx {
        ast,
        literals: &ast.string_interner,
        state: State::default(),
    };
    ctx.eval(start)
}

impl<'a> Ctx<'a> {
    pub fn eval2(&mut self, args: &[NodeId]) -> Result<[Prim; 2], TError> {
        let l = args.first().expect("requires a left argument");
        let r = args.get(1).expect("requires a right argument");
        let l = self.eval(*l)?;
        let r = self.eval(*r)?;
        Ok([l, r])
    }

    pub fn eval(&mut self, node: NodeId) -> Result<Prim, TError> {
        trace!("Eval: {}", self.ast.pretty_node(node));
        // TODO(core): implement evaluation of the AST
        let node = node.get(&self.ast.nodes);
        match node.id {
            NodeData::Identifier(ident) => {
                let (_id, name) = self.ast[ident];
                let Some(value) = self.state.get_binding(&name) else {
                    let Some(name) = self.ast.string_interner.get_str(name) else {
                        panic!("Not found (unknown name): {name:?}");
                    };
                    panic!("Not found: {name}");
                };
                Ok(value.clone())
            }
            NodeData::Atom(_id) => todo!(),
            NodeData::Call(call) => {
                let (_id, Call { inner, args }) = &self.ast[call];
                for arg in args.iter() {
                    self.eval(*arg)?;
                }
                self.eval(*inner)
            }
            NodeData::Op(id) => self.eval_op(node, id),
            NodeData::Definition(def) => {
                let (
                    _id,
                    Definition {
                        mode: _,
                        name,
                        arguments: bindings,
                        implementation,
                    },
                ) = &self.ast[def];
                match bindings {
                    None => {
                        let value =
                            self.eval(implementation.expect("Should have implementation"))?;
                        self.state.names.insert(*name, value);
                        Ok(Prim::Unit)
                    }
                    _ => todo!("Handle a binding with: {name:?}, {bindings:?}, {implementation:?}"),
                }
            }
            NodeData::Literal(id) => self.eval_lit(node, id),
            NodeData::Warning(_id) => todo!(),
        }
    }
    pub fn eval_lit(&mut self, node: &Node, id: LiteralId) -> Result<Prim, TError> {
        let (_node_id, lit) = id.get(&self.ast.literals);
        let s = self.literals.get_str_by_loc(node.location.start);
        Ok(match lit {
            Literal::Bool => todo!("{lit:?} {s:?}"),
            Literal::Numeric => {
                Prim::I32(s.expect("Should have string for literal").parse::<i32>()?)
            }
            Literal::String => todo!("string {lit:?} {s:?}"),
            Literal::Color => todo!("color {lit:?} {s:?}"),
            Literal::Array => todo!("array {lit:?} {s:?}"),
            Literal::Map => todo!("map {lit:?} {s:?}"),
        })
    }
    pub fn eval_op(&mut self, _node: &Node, id: OpId) -> Result<Prim, TError> {
        let (_node_id, op) = id.get(&self.ast.ops);
        Ok(match op.op {
            Symbol::Add => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::I32(l + r),
                _ => todo!(),
            },
            Symbol::Sub => {
                if op.args.get(1).is_some() {
                    match self.eval2(&op.args)? {
                        [Prim::I32(l), Prim::I32(r)] => Prim::I32(l - r),
                        _ => todo!(),
                    }
                } else {
                    let arg = self.eval(
                        *op.args
                            .first()
                            .expect("Sub should have at least one operand"),
                    )?;
                    match arg {
                        Prim::I32(r) => Prim::I32(-r),
                        _ => todo!(),
                    }
                }
            }
            Symbol::Mul => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::I32(l * r),
                _ => todo!(),
            },
            Symbol::Div => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::I32(l / r),
                _ => todo!(),
            },
            Symbol::Exp => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::I32(l.pow(r.try_into().unwrap())),
                _ => todo!(),
            },
            Symbol::Modulo => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::I32(l % r),
                _ => todo!(),
            },
            Symbol::And => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::I32(l & r),
                _ => todo!(),
            },
            Symbol::BitXor => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::I32(l ^ r),
                _ => todo!(),
            },
            Symbol::Or => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::I32(l | r),
                _ => todo!(),
            },
            Symbol::Eqs => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::Bool(l == r),
                _ => todo!(),
            },
            Symbol::NotEqs => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::Bool(l != r),
                _ => todo!(),
            },
            Symbol::Lt => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::Bool(l < r),
                _ => todo!(),
            },
            Symbol::LtEqs => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::Bool(l <= r),
                _ => todo!(),
            },
            Symbol::LeftShift => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::I32(l << r),
                _ => todo!(),
            },
            Symbol::Gt => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::Bool(l > r),
                _ => todo!(),
            },
            Symbol::GtEqs => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::Bool(l >= r),
                _ => todo!(),
            },
            Symbol::RightShift => match self.eval2(&op.args)? {
                [Prim::I32(l), Prim::I32(r)] => Prim::I32(l >> r),
                _ => todo!(),
            },
            Symbol::BitNot => todo!(),
            Symbol::LogicalNot => todo!(),
            Symbol::LogicalAnd => todo!(),
            Symbol::LogicalOr => todo!(),
            Symbol::GetAddress => todo!(),
            Symbol::LeftPipe => todo!(),
            Symbol::RightPipe => todo!(),
            Symbol::HasType => todo!(),
            Symbol::Arrow | Symbol::DoubleArrow => {
                // TODO(clarity): Type arrow vs value arrow?
                let Some(_l) = op.args.first() else {
                    panic!("-> expects a left and a right. Left not found");
                };
                let Some(r) = op.args.get(1) else {
                    panic!("-> expects a left and a right. Right not found");
                };
                self.eval(*r)?
            }
            Symbol::Forall => todo!(),
            Symbol::Exists => todo!(),
            Symbol::Lambda => todo!(),
            Symbol::Sigma => todo!(),
            Symbol::Pi => todo!(),
            Symbol::Try => todo!(),
            Symbol::Dot => todo!(),
            Symbol::Range => todo!(),
            Symbol::Spread => todo!(),
            Symbol::Sequence => {
                let Some(l) = op.args.first() else {
                    panic!("; expects a left and a right. Neither found");
                };
                let Some(r) = op.args.get(1) else {
                    panic!("; expects a left and a right. Right not found");
                };
                let _l = self.eval(*l)?; // TODO: Keep `l` in the context.
                self.eval(*r)?
            }
            Symbol::OpenCurly => todo!(),
            Symbol::CloseCurly => todo!(),
            Symbol::OpenParen => todo!(),
            Symbol::CloseParen => todo!(),
            Symbol::OpenBracket => todo!(),
            Symbol::CloseBracket => todo!(),
            Symbol::Hash => todo!(),
            Symbol::Shebang => todo!(),
            Symbol::Comment => todo!(),
            Symbol::MultiCommentOpen => todo!(),
            Symbol::MultiCommentClose => todo!(),
            Symbol::Assign
            | Symbol::AddAssign
            | Symbol::SubAssign
            | Symbol::DivAssign
            | Symbol::MulAssign
            | Symbol::AndAssign
            | Symbol::BitXorAssign
            | Symbol::OrAssign
            | Symbol::LogicalAndAssign
            | Symbol::LogicalOrAssign
            | Symbol::ModuloAssign => todo!("Support assignment"),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::TError;
    use crate::parser::parse;
    use crate::parser::tokens::lex;
    use std::path::PathBuf;

    fn test_path() -> PathBuf {
        "test.tk".into()
    }

    fn setup(s: &str) -> Result<Ast, TError> {
        crate::ensure_initialized();
        let tokens = lex(s)?;
        parse(&test_path(), s, &tokens)
    }

    #[test]
    fn literal_evals_to_itself() -> Result<(), TError> {
        let ast = setup("123")?;
        let res = run(&test_path(), &ast, None);
        assert_eq!(res, Ok(Prim::I32(123)));
        Ok(())
    }

    #[test]
    fn literal_negatives_multiply_out() -> Result<(), TError> {
        let ast = setup("-3*-2")?;
        let res = run(&test_path(), &ast, None);
        assert_eq!(res, Ok(Prim::I32(6)));
        Ok(())
    }

    #[test]
    fn exp_mul_evals_16() -> Result<(), TError> {
        let ast = setup("2**3*2")?;
        let res = run(&test_path(), &ast, None);
        assert_eq!(res, Ok(Prim::I32(16)));
        Ok(())
    }

    #[test]
    fn exp_exp_evals_512() -> Result<(), TError> {
        let ast = setup("2**3**2")?;
        let res = run(&test_path(), &ast, None);
        assert_eq!(res, Ok(Prim::I32(512)));
        Ok(())
    }

    #[test]
    fn exp_var_and_use() -> Result<(), TError> {
        let ast = setup("x=2;x")?;
        let res = run(&test_path(), &ast, None);
        assert_eq!(res, Ok(Prim::I32(2)));
        Ok(())
    }

    #[test]
    fn exp_var_from_expr_and_use() -> Result<(), TError> {
        let ast = setup("x=3+2;x")?;
        let res = run(&test_path(), &ast, None);
        assert_eq!(res, Ok(Prim::I32(5)));
        Ok(())
    }

    #[test]
    fn exp_nested_vars() -> Result<(), TError> {
        let ast = setup("x=(y=3;2*y);x")?;
        let res = run(&test_path(), &ast, None);
        assert_eq!(res, Ok(Prim::I32(6)));
        Ok(())
    }

    #[test]
    fn exp_multiple_statements() -> Result<(), TError> {
        let ast = setup("x=3;y=x+4;2*y")?;
        let res = run(&test_path(), &ast, None);
        assert_eq!(res, Ok(Prim::I32(14)));
        Ok(())
    }

    #[test]
    fn exp_multiple_statements_as_lambdas() -> Result<(), TError> {
        let ast = setup("(x->(y->(2*y))(y=x+4))(x=3)")?;
        let res = run(&test_path(), &ast, None);
        assert_eq!(res, Ok(Prim::I32(14)));
        Ok(())
        // TODO: Remove!
    }
}
