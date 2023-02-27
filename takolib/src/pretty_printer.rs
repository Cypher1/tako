#![allow(unused)]
use crate::{
    ast::{Ast, Binding, Contains, Node, NodeData, NodeId},
    string_interner::Identifier,
};
use std::fmt;

/*
// TODO: Consider a generic form of this.
struct InContext<'a, T> {
    value: T,
    ast: &'a Ast,
}
*/

#[derive(Debug)]
struct PrintNode<'ast> {
    ast: &'ast Ast,
    node: NodeId,
}

pub fn pretty(ast: &Ast, node: NodeId) -> impl fmt::Display + fmt::Debug + '_ {
    PrintNode::in_context(ast, node)
}

impl<'ast> PrintNode<'ast> {
    pub fn from_context(ast: &'ast Ast) -> Vec<Self> {
        let mut nodes = vec![];
        for node in &ast.roots {
            nodes.push(Self::in_context(ast, *node))
        }
        nodes
    }
    pub fn in_context(ast: &'ast Ast, node: NodeId) -> Self {
        Self { ast, node }
    }
    pub fn child(&self, node: NodeId) -> Self {
        Self {
            ast: self.ast,
            node,
        }
    }

    fn print_ty(&self, f: &mut fmt::Formatter<'_>, ty: &mut Option<NodeId>) -> fmt::Result {
        if let Some(ty) = ty.take() {
            write!(f, ": {}", self.child(ty))?;
        }
        Ok(())
    }

    fn print_binding(&self, f: &mut fmt::Formatter<'_>, binding: &Binding) -> fmt::Result {
        let Binding { mode, name, ty } = &binding;
        write!(
            f,
            "{mode}{}",
            self.ast
                .string_interner
                .get_str(*name)
                .expect("Unknown name!?")
        )?;
        if let Some(ty) = ty {
            write!(f, ": {}", self.child(*ty))?;
        };
        Ok(())
    }

    fn print_definition_head(
        &self,
        f: &mut fmt::Formatter<'_>,
        name: Identifier,
        bindings: &Option<Vec<Binding>>,
        ty: &mut Option<NodeId>,
    ) -> fmt::Result {
        self.print_identifier(f, name)?;
        if let Some(bindings) = &bindings {
            write!(f, "(")?;
            let mut first = true;
            for binding in bindings {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                self.print_binding(f, binding)?;
            }
            write!(f, ")")?;
        }
        self.print_ty(f, ty)
    }

    fn print_identifier(&self, f: &mut fmt::Formatter<'_>, ident: Identifier) -> fmt::Result {
        let s = self.ast.string_interner.get_str(ident);
        if let Some(s) = s {
            write!(f, "{s}")
        } else {
            write!(f, "/*Missing Identifier {ident:?}*/")
        }
    }
}

impl<'ast> fmt::Display for PrintNode<'ast> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Node {
            id: node,
            ty,
            location,
        } = self.ast.get(self.node);
        let mut ty = *ty;
        use NodeData::*;
        match node {
            Warning(node) => {
                let (_node_id, node) = self.ast.get(*node);
                write!(f, "Warning {node:?}")?;
            }
            Binding(node) => {
                let (_node_id, node) = self.ast.get(*node);
                write!(f, "Binding {node:?}")?;
            }
            Atom(node) => {
                let (_node_id, node) = self.ast.get(*node);
                self.print_identifier(f, node.name)?;
            }
            Call(node) => {
                let (_node_id, node) = self.ast.get(*node);
                write!(f, "{node:?}")?;
            }
            Identifier(node) => {
                let (_node_id, node) = self.ast.get(*node);
                self.print_identifier(f, *node)?;
            }
            Literal(node) => {
                let (_node_id, node) = self.ast.get(*node);
                let s = self.ast.string_interner.get_str_by_loc(location.start);
                if let Some(s) = s {
                    write!(f, "{s}")?;
                } else {
                    write!(f, "Missing {node:?} Literal at {location:?}")?;
                }
            }
            NodeRef(node) => {
                write!(f, "{}", self.child(*node))?;
            }
            Definition(node) => {
                let (_node_id, node) = self.ast.get(*node);
                self.print_definition_head(f, node.name, &node.bindings, &mut ty)?;
                return write!(f, "={}", self.child(node.implementation));
            }
            Op(node) => {
                let (_node_id, node) = self.ast.get(*node);
                let mut first = true;
                for arg in &node.args {
                    // TODO: Postfix and infix ops?
                    if !first || node.args.len() == 1 {
                        write!(f, "{}", node.op)?;
                    } else {
                        first = false;
                    }
                    // TODO: Cancel out brackets.
                    let needs_parens = {
                        let node = self.ast.get(*arg);
                        matches!(node.id, NodeData::Op(_))
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
    use crate::parser::tokens::lex;
    use std::path::PathBuf;

    fn test_file1() -> PathBuf {
        "test.tk".into()
    }

    fn setup(s: &str) -> Result<String, TError> {
        crate::ensure_initialized();
        let tokens = lex(s)?;
        let ast = parse(&test_file1(), s, &tokens)?;
        let nodes = PrintNode::from_context(&ast);
        assert_eq!(nodes.len(), 1, "Expect one root, found {}", nodes.len());
        Ok(format!("{}", nodes[0]))
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
}
