pub use paste::paste;
pub mod nodes;
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
    pub node_types: NodeTypes,
}

macro_rules! construct_lang {
    ($start_value: expr, $( $name: ident ),*) => {
        {
            let _lang: &Language = &tree_sitter_tako::LANGUAGE.into();

            paste!{
                Ast {
                    node_types: NodeTypes {
                        $(
                            [< _ $name>]: _lang.id_for_node_kind(stringify!($name), /*named*/ true),
                        )*
                    },
                    ..$start_value
                }
            }
        }
    }
}

impl Ast {
    #[must_use]
    pub fn new(filepath: PathBuf) -> Self {
        construct_lang!(
            Self {
                filepath,
                ..Self::default()
            },
            // TODO: generate from
            // ./tree_sitter_tako/src/node-types.json | jq "map(.type)"
            add,
            and,
            assign,
            binding,
            bit_and,
            bit_not,
            bit_or,
            bit_xor,
            block,
            call,
            color,
            container,
            div,
            equals,
            exp,
            field,
            format_expression,
            greater_than,
            greater_than_equals,
            has_type,
            hex_literal,
            index,
            left_shift,
            less_than,
            less_than_equals,
            mod,
            mul,
            neg,
            nesting_comment,
            not,
            not_equals,
            or,
            parens,
            range,
            right_shift,
            sequence,
            set,
            shebang,
            single_line_comment,
            source_file,
            spread,
            string_literal,
            sub,
            try,
            escape_sequence,
            exists,
            float_literal,
            forall,
            given,
            heading,
            ident,
            int_literal
        )
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
