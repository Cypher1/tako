use crate::ast::*;
use crate::error::TError;
use crate::literal_values::LiteralValues;
use crate::primitives::Prim;
use log::*;
use std::path::Path;

struct Ctx<'a> {
    ast: &'a Ast,
    literals: &'a LiteralValues,
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
        literals: &ast.literal_values,
    };
    ctx.eval(start)
}

impl<'a> Ctx<'a> {
    pub fn eval(&mut self, node: NodeId) -> Result<Prim, TError> {
        // TODO: ???
        let node = node.get(&self.ast.nodes);
        match node.id {
            NodeData::NodeRef(_id) => todo!(),
            NodeData::NamedSymbol(_id) => todo!(),
            NodeData::Call(_id) => todo!(),
            NodeData::Op(_id) => todo!(),
            NodeData::Definition(_id) => todo!(),
            NodeData::Literal(id) => {
                let (_id, lit) = id.get(&self.ast.literals);
                let s = self.literals.get_str_by_loc(node.location.start);
                Ok(match lit {
                    Literal::Bool => todo!("{lit:?} {s:?}"),
                    Literal::Numeric => {
                        Prim::I32(s.expect("Should have string for literal").parse::<i32>()?)
                    }
                    Literal::Text => todo!("{lit:?} {s:?}"),
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::TError;
    use crate::parser::parse;
    use crate::tokens::lex;
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
