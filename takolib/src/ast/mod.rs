#[macro_use]
mod contains;
pub use contains::*;
mod nodes;
pub use nodes::*;
pub mod location;
mod pretty_printer;
pub mod string_interner;

use crate::parser::semantics::Literal;
use crate::parser::tokens::Symbol;
use crate::primitives::typed_index::TypedIndex;
use location::Location;
use pretty_printer::{pretty, pretty_node};
use smallvec::smallvec;
use std::path::PathBuf;
use std::sync::Arc;
use string_interner::{Identifier, StringInterner};

type Container<T> = Arc<Vec<T>>;

#[derive(Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct Ast {
    // TODO(usability): Add a range tree for mapping from locations to nodes.
    // Abstract syntax tree... forest
    pub filepath: PathBuf,
    pub roots: Container<NodeId>,
    pub nodes: Container<Node>,

    // Partials:
    pub warnings: Container<(NodeId, Warning)>,

    // Syntactic constructs:
    pub calls: Container<(NodeId, Call)>, // Convert to this from definition head.
    pub identifiers: Container<(NodeId, Identifier)>, // Convert to this from definition head.
    pub ops: Container<(NodeId, Op)>,
    pub definitions: Container<(NodeId, Definition)>,
    pub literals: Container<(NodeId, Literal)>,
    pub atoms: Container<(NodeId, Atom)>,

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
    pub fn make_node<T>(&mut self, value: T, location: Location) -> NodeId
    where
        Self: Contains<(NodeId, T)>,
    {
        self.make_node_with_id(|_node_id| value, location)
    }

    pub fn make_node_with_id<T, F: FnOnce(NodeId) -> T>(
        &mut self,
        value: F,
        location: Location,
    ) -> NodeId
    where
        Self: Contains<(NodeId, T)>,
    {
        let node_id =
            TypedIndex::next(&self.nodes).expect("Should never have that many AstNodes..."); // Reserve it...
        let value_id = self.alloc((node_id, value(node_id)));
        let node = Node {
            id: Self::to_node(value_id),
            equivalents: None,
            ty: None,
            location,
            lowered_to: None,
        };
        let new_node_id = TypedIndex::new(Arc::make_mut(&mut self.nodes), node)
            .expect("Should never have that many AstNodes...");
        assert_eq!(node_id, new_node_id);
        new_node_id
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::semantics::BindingMode;
    use string_interner::StringInterner;

    #[test]
    fn can_add_nodes_to_ast() {
        let mut lits = StringInterner::new();
        let mut ast = Ast::default();
        let a = lits.register_str("a");
        let b = Literal::Numeric; // ("123456789");
        let a = ast.make_node(a, Location::dummy_for_test());
        let b = ast.make_node(b, Location::dummy_for_test());
        let call = Op {
            op: Symbol::Add,
            args: smallvec![a, b],
        };
        let call = ast.make_node(call, Location::dummy_for_test());
        let a_prime = lits.register_str("a_prime");
        let definition = Definition {
            mode: BindingMode::Lambda,
            name: a_prime,
            bindings: None,
            implementation: Some(call),
        };
        let definition = ast.make_node(definition, Location::dummy_for_test());
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
