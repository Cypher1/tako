mod nodes;
use entity_component_slab::Contains;
pub use nodes::*;
pub mod location;
mod pretty_printer;
pub mod string_interner;

use crate::parser::semantics::Literal;
use crate::parser::tokens::Symbol;
use entity_component_slab::{make_component, make_world};
use location::Location;
use pretty_printer::{pretty, pretty_node};
use short_typed_index::TypedIndex;
use smallvec::smallvec;
use std::path::PathBuf;
use std::sync::Arc;
use string_interner::{Identifier, StringInterner};

// Note: Arc is used here to allow passes to make cheap copies of the AST.
// Each AST get's its own Arcs when mutating, but uses the Arcs of the previous
// pass otherwise.
type Container<T> = Arc<Vec<T>>;
type BackRefContainer<T> = Container<(NodeId, T)>;

#[derive(Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct Ast {
    // TODO(usability): Add a range tree for mapping from locations to nodes.
    // Abstract syntax tree... forest
    pub filepath: PathBuf,
    pub roots: Container<NodeId>,
    pub nodes: Container<Node>,

    // Partials:
    pub warnings: BackRefContainer<Warning>,

    // Syntactic constructs:
    pub calls: BackRefContainer<Call>, // Convert to this from definition head.
    pub identifiers: BackRefContainer<Identifier>, // Convert to this from definition head.
    pub ops: BackRefContainer<Op>,
    pub definitions: BackRefContainer<Definition>,
    pub literals: BackRefContainer<Literal>,
    pub atoms: BackRefContainer<Atom>,

    pub string_interner: StringInterner,
}

impl Ast {
    #[must_use]
    pub fn new(filepath: PathBuf) -> Self {
        Self {
            filepath,
            ..Self::default()
        }
    }

    #[must_use]
    pub fn pretty(&self) -> impl std::fmt::Display + std::fmt::Debug + '_ {
        pretty(self)
    }
    #[must_use]
    pub fn pretty_node(&self, node: NodeId) -> impl std::fmt::Display + std::fmt::Debug + '_ {
        pretty_node(self, node)
    }
}

impl Ast {
    pub fn add_equivalent(&mut self, mut node_id: NodeId, eq: NodeId) -> NodeId {
        while node_id != eq {
            let eqs = &mut self.get_mut(node_id).equivalents;
            node_id = *eqs.get_or_insert(eq);
        }
        eq
    }
    pub fn add_annotation(&mut self, node_id: NodeId, mut ty: NodeId) -> NodeId {
        let old_ty: Option<NodeId> = self.get(node_id).ty;
        if let Some(old_ty) = old_ty {
            // In the case where two annotations were added:
            // e.g. 12: Int: Int
            // It is probably a mistake, but we can safely handle it.
            let location = self.get(node_id).location;
            self.add_warning(
                Warning::DoubleAnnotation {
                    node_id,
                    old_ty,
                    ty,
                },
                location,
            );
            ty = self.add_op(Op::new(Symbol::And, smallvec![old_ty, ty]), location);
        }
        let node: &mut Node = self.get_mut(node_id);
        node.ty = Some(ty);
        node_id
    }
    pub fn set_root(&mut self, new_root: NodeId) {
        Arc::make_mut(&mut self.roots).push(new_root);
    }
}

make_world!(
    nodes,
    Node,
    NodeRef,
    unsafe_add_node,
    NodeData,
    Location,
    Ast,
    |archetype, location| Node {
        id: archetype,
        equivalents: None,
        lowered_to: None,
        ty: None,
        location,
    }
);
make_component!(identifiers, Identifier, Ast);
make_component!(literals, Literal, Ast);
make_component!(warnings, Warning, Ast);
make_component!(atoms, Atom, Ast);
make_component!(calls, Call, Ast);
make_component!(ops, Op, Ast);
make_component!(definitions, Definition, Ast);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::semantics::BindingMode;

    #[test]
    fn can_add_nodes_to_ast() {
        let mut ast = Ast::default();
        let a = ast.string_interner.register_str("a");
        let b = Literal::Numeric; // ("123456789");
        let a = ast.add_identifier(a, Location::dummy_for_test());
        let b = ast.add_literal(b, Location::dummy_for_test());
        let op = ast.add_op(
            Op {
                op: Symbol::Add,
                args: smallvec![a, b],
            },
            Location::dummy_for_test(),
        );
        let a_prime = ast.string_interner.register_str("a_prime");
        let definition = Definition {
            mode: BindingMode::Given,
            name: a_prime,
            arguments: None,
            implementation: Some(op),
        };
        let definition = ast.add_definition(definition, Location::dummy_for_test());
        ast.set_root(definition);
        dbg!(&ast);

        assert_eq!(ast.nodes.len(), 4);
        assert_eq!(ast.identifiers.len(), 1);
        assert_eq!(ast.literals.len(), 1);
        assert_eq!(ast.ops.len(), 1);
        assert_eq!(ast.calls.len(), 0);
        assert_eq!(ast.definitions.len(), 1);
        assert_eq!(ast.roots, vec![definition].into());
    }
}
