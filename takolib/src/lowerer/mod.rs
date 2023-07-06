use crate::ast::{Ast, Contains, Node, NodeId};
use crate::error::TError;
use crate::parser::semantics::Literal;
use better_std::todo;
use llamada::base_types::Empty;
use llamada::Term::Ext;
use llamada::{Expr, Llamada};
use log::trace;
use std::path::Path;

pub fn lower(_path: &Path, og_ast: &Ast, root: NodeId) -> Result<Llamada, TError> {
    let mut ast = og_ast.clone();
    let mut expr = Llamada::new(Ext(666.into()), Empty);

    for (nodeid, val) in og_ast.literals.iter() {
        let val = match val {
            Literal::Numeric => {
                let location = ast.get(*nodeid).location;
                let str = ast.string_interner.get_str_by_loc(location.start);
                trace!("GOT NUMERIC AT {:?} => {:?}", &location, str);
                let str = str.expect("Got nothing for the string");
                let val = str
                    .parse::<u32>()
                    .expect("Could not parse string as number");
                val.into()
            }
            _ => todo!("Unhandled value type {val:?}"),
        };
        let e_id = expr.add(Ext(val));
        let node: &mut Node = ast.get_mut(*nodeid);
        node.lowered_to = Some(e_id);
    }
    for (nodeid, val) in og_ast.calls.iter() {
        let val = match val {
            Literal::Numeric => {
                let location = ast.get(*nodeid).location;
                let str = ast.string_interner.get_str_by_loc(location.start);
                trace!("GOT NUMERIC AT {:?} => {:?}", &location, str);
                let str = str.expect("Got nothing for the string");
                let val = str
                    .parse::<u32>()
                    .expect("Could not parse string as number");
                val.into()
            }
            _ => todo!("Unhandled value type {val:?}"),
        };
        let e_id = expr.add(Ext(val));
        let node: &mut Node = ast.get_mut(*nodeid);
        node.lowered_to = Some(e_id);
    }
    // TODO: Others...
    // For every abs var and cons
    // Map them in and track them with `.lowered_to`
    let root = ast.get(root).lowered_to.unwrap_or_else(||
        todo!("root wasn't lowered: {:?}\n{}", ast.get(root), ast.pretty_node(root))
    );
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
    fn lower_gives_constant_from_constant() -> Result<(), TError> {
        let ast = setup("21")?;
        let out = lower(&test_path(), &ast, ast.roots[0])?;
        dbg!(&out);

        eprintln!("{out}");
        assert_eq!(format!("{out}"), "21");
        let mut out = out;
        Expr::reduce(&mut out);
        assert_eq!(format!("{out}"), "21");
        Ok(())
    }

    #[test]
    fn lower_gives_constant_from_id_ap_constant() -> Result<(), TError> {
        let ast = setup("(x=>x)(x=2)")?;
        let out = lower(&test_path(), &ast, ast.roots[0])?;
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
