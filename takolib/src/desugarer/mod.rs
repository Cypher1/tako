use crate::ast::NodeData::Definition;
use crate::ast::{Ast, Contains};
use crate::ast::{Call, NodeId, Op};
use crate::error::TError;
use crate::parser::tokens::Symbol;
use std::path::Path;

pub fn desugar(_path: &Path, old_ast: &Ast, _root: Option<NodeId>) -> Result<Ast, TError> {
    let mut ast = old_ast.clone();
    let mut new_seqs = vec![];
    for (node_id, op) in &ast.ops {
        if op.op == Symbol::Sequence {
            new_seqs.push((*node_id, op.clone()));
        }
    }

    for (node_id, op) in new_seqs {
        let [left, right] = &op.args[0..2] else {
            todo!("Unexpected arguments to ';' operator: {op:?}");
        };
        // debug!("desugar Definition: {} ; {}", ast.pretty_node(*left), ast.pretty_node(*right));
        let left_node = ast.get(*left);
        let location = left_node.location;
        let name = match left_node.id {
            Definition(id) => {
                let (_node, left_node) = ast.get(id);
                left_node.name
            }
            _ => {
                todo!("Unexpected arguments to ';' operator: {op:?}");
            }
        };
        // let var:  = ast.get(left);
        let name = ast.add_identifier(name, location);
        let inner = Op {
            op: Symbol::Arrow,
            args: vec![name, *right],
        };
        let inner = ast.add_op(inner, location);
        let apply = Call {
            inner,
            args: vec![*left],
        };
        let apply = ast.add_call(apply, location);
        ast.add_equivalent(node_id, apply);
        // eprintln!("{}", ast.pretty_node(apply));
    }
    Ok(ast)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use crate::parser::tokens::lex;
    use crate::pretty_printer::pretty;
    use std::path::PathBuf;

    fn test_path() -> PathBuf {
        "test.tk".into()
    }

    fn setup(s: &str, e: &str) -> Result<(Ast, Ast), TError> {
        crate::ensure_initialized();
        let tokens_s = lex(s)?;
        let ast = parse(&test_path(), s, &tokens_s)?;
        let tokens_e = lex(e)?;
        let exp = parse(&test_path(), e, &tokens_e)?;
        Ok((ast, exp))
    }

    fn desugars_to(s: &str, e: &str) -> Result<(), TError> {
        let (ast, exp) = setup(s, e)?;
        let res = desugar(&test_path(), &ast, None)?;
        let res_pretty = format!("{}", pretty(&res));
        let exp_pretty = format!("{}", pretty(&exp));
        assert_eq!(res_pretty, exp_pretty);
        Ok(())
    }

    fn is_desugared(s: &str) -> Result<(), TError> {
        desugars_to(s, s)
    }

    #[test]
    fn literal_desugars_to_itself() -> Result<(), TError> {
        is_desugared("123")
    }

    #[test]
    fn exp_mul_desugars_16() -> Result<(), TError> {
        desugars_to("2**3*2", "(2**3)*2")
    }

    #[test]
    fn exp_exp_desugars_512() -> Result<(), TError> {
        desugars_to("2**3**2", "2**(3**2)")
    }

    #[test]
    fn exp_var_and_use() -> Result<(), TError> {
        desugars_to("x=2;x", "(x->x)(x=2)")
    }

    #[test]
    fn exp_var_from_expr_and_use() -> Result<(), TError> {
        desugars_to("x=3+2;x", "(x->x)(x=3+2)")
    }

    #[test]
    fn exp_nested_vars() -> Result<(), TError> {
        desugars_to("x=(y=3;2*y);x", "(x->x)(x=((y->(2*y))(y=3)))")
    }
}
