use crate::ast::{Ast, Contains, Node, NodeData, NodeId};
use crate::error::TError;
use crate::parser::semantics::Literal;
use crate::parser::tokens::Symbol;
use better_std::todo;
use llamada::base_types::Empty;
use llamada::Term::Ext;
use llamada::{Expr, Llamada};
use log::trace;
use std::collections::HashMap;
use std::path::Path;

type Value = <Llamada as Expr>::Value;
type Index = <Llamada as Expr>::Index;
type Term = llamada::Term<Value, Index>;

pub fn lower(_path: &Path, og_ast: &Ast, root: NodeId) -> Result<Llamada, TError> {
    let mut ast = og_ast.clone();
    let mut expr = Llamada::new(Term::Var(0), Empty);
    let mut ast_to_expr = HashMap::new();
    let mut get_expr = move |expr: &mut Llamada, id: NodeId, term: Option<Term>| {
        let e_id = ast_to_expr
            .entry(id)
            .or_insert_with(|| expr.add(Term::Var(0)));
        if let Some(term) = term {
            *expr.get_mut(e_id) = term;
        }
        *e_id
    };

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
        let e_id = get_expr(&mut expr, *nodeid, Some(Ext(val)));
        eprintln!("VAL is {:?} = {:?}", e_id, val);
        let node: &mut Node = ast.get_mut(*nodeid);
        node.lowered_to = Some(e_id);
    }
    for (nodeid, op) in og_ast.ops.iter() {
        let location = ast.get(*nodeid).location;
        eprintln!("OP: {}", ast.pretty_node(*nodeid));
        if op.op == Symbol::DoubleArrow {
            trace!("GOT DoubleArrow OP AT {:?}", &location);
            let [_over, inner] = op.args[..] else {
                todo!("WHAT?")
            };
            eprintln!("ABS OVER {}", ast.pretty_node(inner));
            let curr = get_expr(&mut expr, inner, Some(Term::Var(1))); // TODO: Reassociate...
            let curr = get_expr(&mut expr, *nodeid, Some(Term::Abs(None, curr))); // TODO: Add the type info?
            let node: &mut Node = ast.get_mut(*nodeid);
            node.lowered_to = Some(curr);
            continue;
        }
        // TODO!?
    }
    for (nodeid, call) in og_ast.calls.iter() {
        let location = ast.get(*nodeid).location;
        trace!("GOT CALL AT {:?}", &location);
        let inner = call.inner;
        let mut curr = get_expr(&mut expr, inner, None);
        for arg in &call.args {
            let arg_node: &Node = ast.get(*arg);

            // Get the implementation of the definition...
            let arg_def_id = match arg_node.id {
                NodeData::Definition(def) => def,
                _ => todo!("WAT"),
            };
            let (arg, arg_def) = ast.get(arg_def_id);
            eprintln!("ARG: {}", ast.pretty_node(*arg));

            // Get the llamada expr to match...
            let arg = get_expr(&mut expr, arg_def.implementation.expect("WHAT?"), None);
            let new_curr = expr.add(Term::App(curr, arg));
            eprintln!("CALL is {:?} = {:?} {:?}", new_curr, curr, arg);
            curr = new_curr;
        }
        let node: &mut Node = ast.get_mut(*nodeid);
        node.lowered_to = Some(curr);
    }
    // TODO: Others...
    // For every abs var and cons
    // Map them in and track them with `.lowered_to`
    let mut node = ast.get(root);
    while let Some(equi) = node.equivalents {
        node = ast.get(equi);
    }
    let root = node.lowered_to.unwrap_or_else(|| {
        todo!(
            "root wasn't lowered: {:?}\n{}",
            ast.get(root),
            ast.pretty_node(root)
        )
    });
    *expr.root_mut() = root;
    Ok(expr)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::desugarer::desugar;
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
        assert_eq!(format!("{out}"), "((a => a) 2)");
        let mut out = out;
        Expr::reduce(&mut out);
        assert_eq!(format!("{out}"), "2");
        Ok(())
    }

    #[test]
    fn lower_gives_constant_from_use_var_after_decl() -> Result<(), TError> {
        let ast = setup("x=2; x")?;
        let ast = desugar(&test_path(), &ast, None)?;
        eprintln!("Desugared to: {}", ast.pretty());
        let out = lower(&test_path(), &ast, ast.roots[0])?;
        dbg!(&out);

        eprintln!("{out}");
        assert_eq!(format!("{out}"), "((a => a) 2)");
        let mut out = out;
        Expr::reduce(&mut out);
        assert_eq!(format!("{out}"), "2");
        Ok(())
    }

    #[test]
    fn lower_gives_constant_from_id_id_ap_constant() -> Result<(), TError> {
        let ast = setup("(x=>x)((x=(x=>x))(x=2))")?;
        let out = lower(&test_path(), &ast, ast.roots[0])?;
        dbg!(&out);

        eprintln!("{out}");
        assert_eq!(format!("{out}"), "(((a => a) (a => a)) 2)");
        let mut out = out;
        Expr::reduce(&mut out);
        assert_eq!(format!("{out}"), "2");
        Ok(())
    }
}
