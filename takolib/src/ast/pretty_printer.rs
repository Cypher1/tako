#![allow(unused)]
use crate::ast::{string_interner::Identifier, Ast, Definition, Node, NodeData, NodeId};
use crate::parser::semantics::BindingMode;
use better_std::as_context;
use entity_component_slab::ContainsSlab;
use smallvec::SmallVec;
use std::fmt;
use std::fmt::Write;

as_context!(NodeId, Ast, PrintNode);
as_context!(Vec<NodeId>, Ast, PrintNodes);

pub fn pretty(ast: &Ast) -> impl fmt::Display + fmt::Debug + '_ {
    let mut nodes = vec![];
    for node in ast.roots.iter() {
        nodes.push(*node)
    }
    PrintNodes::in_context(ast, nodes)
}

pub fn pretty_node(ast: &Ast, node: NodeId) -> impl fmt::Display + fmt::Debug + '_ {
    PrintNode::in_context(ast, node)
}

impl PrintNode<'_> {
    fn print_ty(&self, f: &mut fmt::Formatter<'_>, ty: &mut Option<NodeId>) -> fmt::Result {
        if let Some(ty) = ty.take() {
            write!(f, ": {}", self.child(ty))?;
        }
        Ok(())
    }

    fn print_definition_head<'a, T: std::iter::IntoIterator<Item = &'a NodeId>>(
        &self,
        f: &mut fmt::Formatter<'_>,
        mode: BindingMode,
        name: Identifier,
        arguments: Option<T>,
        ty: &mut Option<NodeId>,
    ) -> fmt::Result {
        if mode != BindingMode::Given {
            write!(f, "{mode} ");
        }
        self.print_identifier(f, name)?;
        if let Some(arguments) = arguments {
            let mut implicits = String::new();
            let mut explicits = String::new();
            for binding in arguments.into_iter() {
                let into = if let NodeData::Definition(def) = self.context().get(*binding).id {
                    let (_nodeid, def) = self.context().get(def);
                    if def.mode != BindingMode::Given {
                        &mut implicits
                    } else {
                        &mut explicits
                    }
                } else {
                    &mut explicits
                };
                if !into.is_empty() {
                    write!(into, ", ")?;
                }
                write!(into, "{}", self.child(*binding))?;
            }
            write!(f, "(")?;
            if !implicits.is_empty() {
                write!(f, "{implicits}")?;
                if !explicits.is_empty() {
                    write!(f, ", ")?;
                }
            }
            write!(f, "{explicits})")?;
        }
        self.print_ty(f, ty)
    }

    fn print_identifier(&self, f: &mut fmt::Formatter<'_>, ident: Identifier) -> fmt::Result {
        let s = self.context().string_interner.get_str(ident);
        if let Some(s) = s {
            write!(f, "{s}")
        } else {
            write!(f, "/*Missing Identifier {ident:?}*/")
        }
    }
}

impl fmt::Display for PrintNodes<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for node in &**self {
            if !first {
                writeln!(f);
            }
            write!(f, "{}", PrintNode::in_context(self.context(), *node));
            first = false;
        }
        write!(f, "")
    }
}

impl std::fmt::Display for PrintNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Node {
            id: node,
            equivalents,
            ty,
            location,
            lowered_to: _,
        } = self.context().get(**self);
        if let Some(eq) = equivalents {
            return write!(f, "{}", self.child(*eq)); // Watch out for cycles...
        }
        let mut ty = *ty; // This is a copy
        match node {
            NodeData::Warning(node) => {
                let (_node_id, node) = self.context().get(*node);
                write!(f, "Warning {node:?}")?;
            }
            NodeData::Call(node) => {
                let (_node_id, node) = self.context().get(*node);
                let is_ident = matches!(self.context().get(node.inner).id, NodeData::Identifier(_));
                if !is_ident {
                    write!(f, "(")?;
                }
                write!(f, "{}", self.child(node.inner))?;
                if !is_ident {
                    write!(f, ")")?;
                }
                write!(f, "(")?;
                let mut first = true;
                for arg in &*node.args {
                    if !first {
                        write!(f, ", ");
                    } else {
                        first = false;
                    }
                    write!(f, "{}", self.child(*arg))?;
                }
                write!(f, ")")?;
            }
            NodeData::Identifier(node) => {
                let (_node_id, node) = self.context().get(*node);
                self.print_identifier(f, *node)?;
            }
            NodeData::Literal(node) => {
                let (_node_id, node) = self.context().get(*node);
                let s = self
                    .context()
                    .string_interner
                    .get_str_by_loc(location.start);
                if let Some(s) = s {
                    write!(f, "{s}")?;
                } else {
                    write!(f, "Missing {node:?} Literal at {location:?}")?;
                }
            }
            NodeData::Definition(node) => {
                let (_node_id, node) = self.context().get(*node);
                let Definition {
                    mode,
                    name,
                    arguments,
                    implementation,
                } = node;
                self.print_definition_head(
                    f,
                    *mode,
                    *name,
                    arguments.as_ref().map(|bs| bs.iter()),
                    &mut ty,
                )?;
                if let Some(implementation) = implementation {
                    write!(f, "={}", self.child(*implementation));
                }
            }
            NodeData::Op(node) => {
                let (_node_id, node) = self.context().get(*node);
                let mut first = true;
                for arg in node.args.iter() {
                    // TODO: Postfix and infix ops?
                    if !first || node.args.len() == 1 {
                        write!(f, "{}", node.op)?;
                    } else {
                        first = false;
                    }
                    // TODO: Cancel out brackets.
                    let needs_parens = {
                        let node = self.context().get(*arg);
                        matches!(node.id, NodeData::Op(_) | NodeData::Definition(_))
                    };
                    if needs_parens {
                        write!(f, "(")?;
                    }
                    write!(f, "{}", self.child(*arg))?;
                    if needs_parens {
                        write!(f, ")")?;
                    }
                }
            }
        }
        self.print_ty(f, &mut ty)?;
        write!(f, "")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::TError;
    use crate::parser::parse;
    use crate::parser::lexer::lex;
    use std::path::PathBuf;

    fn test_file1() -> PathBuf {
        "test.tk".into()
    }

    fn setup(s: &str) -> Result<String, TError> {
        crate::ensure_initialized();
        let tokens = lex(s)?;
        let ast = parse(&test_file1(), s, &tokens)?;
        assert_eq!(
            ast.roots.len(),
            1,
            "Expect one root, found {}",
            ast.roots.len()
        );
        Ok(format!("{}", ast.pretty()))
    }

    #[test]
    fn round_trip_lit() -> Result<(), TError> {
        let out = setup("123")?;
        assert_eq!(out, "123");
        Ok(())
    }

    #[test]
    fn round_trip_op_ident_and_lit() -> Result<(), TError> {
        let out = setup("x+1")?;
        assert_eq!(out, "x+1");
        Ok(())
    }

    #[test]
    fn round_trip_mul_add() -> Result<(), TError> {
        let out = setup("1*2+3")?;
        assert_eq!(out, "(1*2)+3");
        Ok(())
    }

    #[test]
    fn round_trip_add_add() -> Result<(), TError> {
        let out = setup("1+2+3")?;
        assert_eq!(out, "(1+2)+3");
        Ok(())
    }

    #[test]
    fn round_trip_add_mul() -> Result<(), TError> {
        let out = setup("1+2*3")?;
        assert_eq!(out, "1+(2*3)");
        Ok(())
    }

    #[test]
    fn round_trip_div_div() -> Result<(), TError> {
        let out = setup("1/2/3")?;
        assert_eq!(out, "(1/2)/3");
        Ok(())
    }

    #[test]
    fn round_trip_exp_exp() -> Result<(), TError> {
        let out = setup("1**2**3")?;
        assert_eq!(out, "1**(2**3)");
        Ok(())
    }

    #[test]
    fn round_trip_exp_mul() -> Result<(), TError> {
        let out = setup("1**2*3")?;
        assert_eq!(out, "(1**2)*3");
        Ok(())
    }

    #[test]
    fn round_trip_assignment_with_type() -> Result<(), TError> {
        let out = setup("x:Int=1")?;
        assert_eq!(out, "x: Int=1");
        Ok(())
    }

    #[test]
    fn round_trip_assignment_with_type_and_bindings() -> Result<(), TError> {
        let out = setup("x(y: Int): Int=1")?;
        assert_eq!(out, "x(y: Int): Int=1");
        Ok(())
    }

    #[test]
    fn round_trip_implicit_bindings() -> Result<(), TError> {
        let out = setup("x(forall T: Int, y: T): T=1")?;
        assert_eq!(out, "x(forall T: Int, y: T): T=1");
        Ok(())
    }
    #[test]
    fn round_trip_implicit_bindings_id_fn() -> Result<(), TError> {
        let out = setup("id(forall T: Type, y: T): T=y")?;
        assert_eq!(out, "id(forall T: Type, y: T): T=y");
        Ok(())
    }

    #[test]
    fn round_trip_values_in_fn_args() -> Result<(), TError> {
        let out = setup("signum(x)=if(x<0, -1, 1)")?;
        assert_eq!(out, "signum(x)=if(x<0, -1, 1)");
        Ok(())
    }

    #[test]
    fn round_trip_lambda_with_args() -> Result<(), TError> {
        let out = setup("(x->x)(x=2*3)")?;
        assert_eq!(out, "(x->x)(x=2*3)");
        Ok(())
    }

    #[test]
    fn round_trip_lambda_value_with_args() -> Result<(), TError> {
        let out = setup("x->x(x=2*3)")?;
        assert_eq!(out, "x->x(x=2*3)");
        Ok(())
    }
}
