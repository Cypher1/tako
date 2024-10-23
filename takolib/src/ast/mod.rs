mod nodes;
use entity_component_slab::{ChildSlab, Slab};
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
use smallvec::{smallvec, SmallVec};
use std::path::PathBuf;
use string_interner::{Identifier, StringInterner};
use tree_sitter::Language;

type TsNodeId = u16;

#[derive(Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct Ast {
    // TODO(usability): Add a range tree for mapping from locations to nodes.
    // Abstract syntax tree... forest
    pub filepath: PathBuf,
    pub roots: SmallVec<NodeId, 10>,
    pub nodes: Slab<Node>,

    // Partials:
    pub warnings: ChildSlab<Warning, NodeId>,

    // Syntactic constructs:
    pub calls: ChildSlab<Call, NodeId>, // Convert to this from definition head.
    pub identifiers: ChildSlab<Identifier, NodeId>, // Convert to this from definition head.
    pub ops: ChildSlab<Op, NodeId>,
    pub definitions: ChildSlab<Definition, NodeId>,
    pub literals: ChildSlab<Literal, NodeId>,
    pub atoms: ChildSlab<Atom, NodeId>,

    pub string_interner: StringInterner,

    // TS Node Ids
    // TODO: generated from
    // ../../../tree_sitter_tako/src/node-types.json
    pub add_node_id: TsNodeId,
    pub and_node_id: TsNodeId,
    pub assign_node_id: TsNodeId,
    pub binding_node_id: TsNodeId,
    pub bit_and_node_id: TsNodeId,
    pub bit_not_node_id: TsNodeId,
    pub bit_or_node_id: TsNodeId,
    pub bit_xor_node_id: TsNodeId,
    pub block_node_id: TsNodeId,
    pub call_node_id: TsNodeId,
    pub color_node_id: TsNodeId,
    pub container_node_id: TsNodeId,
    pub div_node_id: TsNodeId,
    pub equals_node_id: TsNodeId,
    pub exp_node_id: TsNodeId,
    pub field_node_id: TsNodeId,
    pub format_expression_node_id: TsNodeId,
    pub greater_than_node_id: TsNodeId,
    pub greater_than_equals_node_id: TsNodeId,
    pub has_type_node_id: TsNodeId,
    pub hex_literal_node_id: TsNodeId,
    pub index_node_id: TsNodeId,
    pub left_shift_node_id: TsNodeId,
    pub less_than_node_id: TsNodeId,
    pub less_than_equals_node_id: TsNodeId,
    pub mod_node_id: TsNodeId,
    pub mul_node_id: TsNodeId,
    pub neg_node_id: TsNodeId,
    pub nesting_comment_node_id: TsNodeId,
    pub not_node_id: TsNodeId,
    pub not_equals_node_id: TsNodeId,
    pub or_node_id: TsNodeId,
    pub parens_node_id: TsNodeId,
    pub range_node_id: TsNodeId,
    pub right_shift_node_id: TsNodeId,
    pub sequence_node_id: TsNodeId,
    pub set_node_id: TsNodeId,
    pub shebang_node_id: TsNodeId,
    pub single_line_comment_node_id: TsNodeId,
    pub source_file_node_id: TsNodeId,
    pub spread_node_id: TsNodeId,
    pub string_literal_node_id: TsNodeId,
    pub sub_node_id: TsNodeId,
    pub try_node_id: TsNodeId,
    pub escape_sequence_node_id: TsNodeId,
    pub exists_node_id: TsNodeId,
    pub float_literal_node_id: TsNodeId,
    pub forall_node_id: TsNodeId,
    pub given_node_id: TsNodeId,
    pub heading_node_id: TsNodeId,
    pub ident_node_id: TsNodeId,
    pub int_literal_node_id: TsNodeId,
}

impl Ast {
    #[must_use]
    pub fn new(filepath: PathBuf) -> Self {
        let tako_lang: &Language = &tree_sitter_tako::LANGUAGE.into();
        let int_literal_node_id = tako_lang.id_for_node_kind("int_literal", /*named*/ true);
        Self {
            filepath,
            int_literal_node_id,
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
            let eqs = &mut self[node_id].equivalents;
            node_id = *eqs.get_or_insert(eq);
        }
        eq
    }
    pub fn add_annotation(&mut self, node_id: NodeId, mut ty: NodeId) -> NodeId {
        let old_ty: Option<NodeId> = self[node_id].ty;
        if let Some(old_ty) = old_ty {
            // In the case where two annotations were added:
            // e.g. 12: Int: Int
            // It is probably a mistake, but we can safely handle it.
            let location = self[node_id].location;
            self.add_warning(
                Warning::DoubleAnnotation {
                    node_id,
                    old_ty,
                    ty,
                },
                location,
            );
            ty = self.add_op(
                Op {
                    op: Symbol::And,
                    args: smallvec![old_ty, ty],
                },
                location,
            );
        }
        let node: &mut Node = &mut self[node_id];
        node.ty = Some(ty);
        node_id
    }
    pub fn set_root(&mut self, new_root: NodeId) {
        self.roots.push(new_root);
    }
}

make_world!(
    Ast,
    nodes,
    Node,
    NodeData,
    Location,
    |archetype, location| Node {
        id: archetype,
        equivalents: None,
        lowered_to: None,
        ty: None,
        location,
    }
);
make_component!(Ast, identifiers, Identifier);
make_component!(Ast, literals, Literal);
make_component!(Ast, warnings, Warning);
make_component!(Ast, atoms, Atom);
make_component!(Ast, calls, Call);
make_component!(Ast, ops, Op);
make_component!(Ast, definitions, Definition);

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
        assert_eq!(ast.roots[..], [definition]);
    }
}
