use crate::ast::{Ast, NodeId, Node, Contains};
use crate::error::TError;
use llamada::base_types::Empty;
use llamada::ext;
use llamada::{Expr, Llamada};
use std::path::Path;

pub fn lower(_path: &Path, ast: &Ast, root: Option<NodeId>) -> Result<Llamada, TError> {
    let mut ast = ast.clone();
    let mut expr = Llamada::new(ext(24), Empty);

    for (nodeid, val) in ast.literals.iter() {
        let e_id = expr.add(ext(val));
        let node: &mut Node = ast.get_mut(*nodeid);
        node.lowered_to.insert(e_id);
    }
    // TODO: Others...
    // For every abs var and cons
    // Map them in and track them with `.lowered_to`
    let root = ast.get(root.expect("No root")).lowered_to.expect("root wasn't lowered");
    *expr.root_mut() = root;
    Ok(expr)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use crate::parser::tokens::lex;
    use std::path::PathBuf;

    fn test_path() -> PathBuf {
        "test.tk".into()
    }

    fn setup(s: &str) -> Result<Ast, TError> {
        crate::ensure_initialized();
        let tokens = lex(s)?;
        let ast = parse(&test_path(), s, &tokens)?;
        Ok(ast)
    }

    #[test]
    fn lower_gives_constant_from_id_ap_constant() -> Result<(), TError> {
        let ast = setup("(x=>x)(x=2)")?;
        let out = lower(&test_path(), &ast, None)?;
        dbg!(&out);

        eprintln!("{out}");
        assert_eq!(format!("{out}"), "(a => a)(2)");
        let mut out = out;
        Expr::reduce(&mut out);
        assert_eq!(format!("{out}"), "2");
        assert_eq!(1, 2);
        Ok(())
    }
}
