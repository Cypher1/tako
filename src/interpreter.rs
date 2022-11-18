use std::path::Path;
use std::collections::HashMap;
use crate::ast::*;
use crate::error::TError;
use crate::primitives::Prim;
use log::*;

pub fn run(path: &Path, ast: &Ast, root: Option<NodeId>) -> Result<Prim, TError> {
    let start = root.unwrap_or_else(|| {
        if ast.roots.len() == 1 {
            ast.roots[0]
        } else {
            error!("Ambiguous run command: Which root should be run for {path}", path=path.display());
            todo!()
        }
    });
    let mut mem = HashMap::new();
    eval(&mut mem, ast, start)
}

pub fn eval(mem: &mut HashMap<usize, Prim>, ast: &Ast, node: NodeId) -> Result<Prim, TError> {
    // TODO: ???
    match node.get(&ast.nodes).id {
        NodeData::NodeRef(_id) => todo!(),
        NodeData::Symbol(_id) => todo!(),
        NodeData::NamedSymbol(_id) => todo!(),
        NodeData::Call(_id) => todo!(),
        NodeData::Op(_id) => todo!(),
        NodeData::Definition(_id) => todo!(),
        NodeData::Literal(id) => {
            let lit = id.get(&ast.literals);
            todo!("{:?}", lit);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::lex;
    use crate::parser::parse;
    use crate::error::TError;
    use std::path::PathBuf;

    fn test_path() -> PathBuf {
        "test.tk".into()
    }

    fn setup(s: &str) -> Result<Ast, TError> {
        let tokens = lex(s)?;
        parse(&test_path(), &tokens)
    }

    #[test]
    fn literal_evals_to_itself() -> Result<(), TError> {
        let ast = setup("123")?;
        let res = run(&test_path(), &ast, None);
        assert_eq!(res, Ok(Prim::I32(123)));
        Ok(())
    }

}
