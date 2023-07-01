use crate::ast::Ast;
use crate::ast::NodeId;
use crate::error::TError;
use llamada::base_types::Empty;
use llamada::ext;
use llamada::{Expr, Llamada};
use std::path::Path;

pub fn lower(_path: &Path, _ast: &Ast, _root: Option<NodeId>) -> Result<Llamada, TError> {
    // TODO: ???
    let /*mut*/ expr = Llamada::new(ext(24), Empty);
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
        let ast = setup("(x->x)(x=2)")?;
        let out = lower(&test_path(), &ast, None)?;
        dbg!(&out);

        eprintln!("{out}");
        assert_eq!(format!("{out}"), "(a. a)(2)");
        let mut out = out;
        Expr::reduce(&mut out);
        assert_eq!(format!("{out}"), "2");
        assert_eq!(1, 2);
        Ok(())
    }
}

