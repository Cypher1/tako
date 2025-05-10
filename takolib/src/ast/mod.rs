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

// TODO: Use https://binrw.rs/ for saving binaries.
// const MAGIC: [u8; 2] = ['T', 'K'];
// const AST_VERSION: [u8; 2] = [0, 0];
// #[derive(BinRead, BinWrite)]
// #[brw(little, magic = MAGIC)]
#[derive(Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct Ast {
    // TODO(usability): Add a range tree for mapping from locations to nodes.
    // Abstract syntax tree... forest
    pub contents: String,
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
    pub string_interner: StringInterner,
}

impl Ast {
    #[must_use]
    pub fn new(filepath: PathBuf, contents: String) -> Self {
        Self {
            contents,
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
            let eqs = &mut self[node_id].equivalents;
            node_id = *eqs.get_or_insert(eq);
        }
        eq
    }
    pub fn add_args(&mut self, node_id: NodeId, args: SmallVec<NodeId, CALL_ARGS_STANDARD_ITEM_NUM>) -> NodeId {
        let location = self[node_id].location;
        let id = self[node_id].id.clone();
        match id {
            NodeData::Identifier(_) | NodeData::Call(_) => {
                let call = Call {
                    inner: node_id,
                    args,
                };
                self.add_call(call, location)
            }
            node => todo!("Assignment to non definition head support: {node:?}"),
        }
    }
    pub fn add_implementation(&mut self, node_id: NodeId, imp: NodeId) -> NodeId {
        use crate::parser::semantics::BindingMode;
        let location = self[node_id].location;
        let id = self[node_id].id.clone();
        let mut ty = self[node_id].ty;
        let d = match id {
            NodeData::Definition(def_id) => {
                let def = &mut self[def_id].1;
                if let Some(_old_imp) = def.implementation {
                    // e.g. x = y = 3
                    todo!("Double assignment support");
                }
                def.implementation = Some(imp);
                ty = None;
                node_id
            }
            NodeData::Identifier(id_id) => {
                let name = self[id_id].1;
                let def = Definition {
                    mode: BindingMode::Given,
                    name,
                    arguments: None,
                    implementation: Some(imp),
                };
                self.add_definition(def, location)
            }
            NodeData::Call(call_id) => {
                let call = &self[call_id].1;
                let name_id = self[call.inner].id.clone();
                let name = match name_id {
                    NodeData::Identifier(name_id) => self[name_id].1,
                    d => todo!("assignment to {d:?}"),
                };
                let def = Definition {
                    mode: BindingMode::Given,
                    name,
                    arguments: Some(call.args.clone()),
                    implementation: Some(imp),
                };
                self.add_definition(def, location)
            }
            node => todo!("Assignment to non definition head support: {node:?}"),
        };
        if let Some(ty) = ty {
            self.add_annotation(d, ty)
        } else {
            d
        }
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
        // In theory these could be struct of array'ed.
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
