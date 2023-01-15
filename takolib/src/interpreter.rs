use crate::ast::*;
use crate::error::TError;
use crate::parser::semantics::Literal;
use crate::parser::tokens::Symbol;
use crate::primitives::Prim;
use crate::string_interner::StringInterner;
use log::*;
use std::convert::TryInto;
use std::path::Path;

struct Ctx<'a> {
    ast: &'a Ast,
    literals: &'a StringInterner,
}

pub fn run(path: &Path, ast: &Ast, root: Option<NodeId>) -> Result<Prim, TError> {
    let start = root.unwrap_or_else(|| {
        if ast.roots.len() == 1 {
            ast.roots[0]
        } else {
            error!(
                "Ambiguous run command: Which root should be run for {path}",
                path = path.display()
            );
            todo!()
        }
    });
    let mut ctx = Ctx {
        ast,
        literals: &ast.string_interner,
    };
    ctx.eval(start)
}

impl<'a> Ctx<'a> {
    pub fn eval2(&mut self, args: &[NodeId]) -> Result<[Prim; 2], TError> {
        let l = args.get(0).expect("requires a left argument");
        let r = args.get(1).expect("requires a right argument");
        let l = self.eval(*l)?;
        let r = self.eval(*r)?;
        Ok([l, r])
    }

    pub fn eval(&mut self, node: NodeId) -> Result<Prim, TError> {
        // TODO(core): implement evaluation of the AST
        let node = node.get(&self.ast.nodes);
        match node.id {
            NodeData::NodeRef(_id) => todo!(),
            NodeData::Identifier(_id) => todo!(),
            NodeData::Atom(_id) => todo!(),
            NodeData::Binding(_id) => todo!(),
            NodeData::Call(_id) => todo!(),
            NodeData::Op(id) => self.eval_op(node, id),
            NodeData::Definition(_id) => todo!(),
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
            Literal::Text => todo!("text {lit:?} {s:?}"),
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
                if op.args.get(0).is_some() {
                    match self.eval2(&op.args)? {
                        [Prim::I32(l), Prim::I32(r)] => Prim::I32(l - r),
                        _ => todo!(),
                    }
                } else {
                    let arg = self.eval(
                        *op.args
                            .get(1)
                            .expect("Sub should have at least a right operand"),
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
            Symbol::DivRounding => match self.eval2(&op.args)? {
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
            Symbol::Escape => todo!(),
            Symbol::HasType => todo!(),
            Symbol::Arrow => todo!(),
            Symbol::Forall => todo!(),
            Symbol::Exists => todo!(),
            Symbol::Lambda => todo!(),
            Symbol::Sigma => todo!(),
            Symbol::Pi => todo!(),
            Symbol::DoubleArrow => todo!(),
            Symbol::Try => todo!(),
            Symbol::Dot => todo!(),
            Symbol::Range => todo!(),
            Symbol::Spread => todo!(),
            Symbol::Comma => todo!(),
            Symbol::Sequence => todo!(),
            Symbol::OpenCurly => todo!(),
            Symbol::CloseCurly => todo!(),
            Symbol::OpenParen => todo!(),
            Symbol::CloseParen => todo!(),
            Symbol::OpenBracket => todo!(),
            Symbol::CloseBracket => todo!(),
            Symbol::Assign
            | Symbol::AddAssign
            | Symbol::SubAssign
            | Symbol::DivAssign
            | Symbol::DivRoundingAssign
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
}
