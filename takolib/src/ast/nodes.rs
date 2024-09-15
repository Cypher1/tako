use super::location::Location;
use super::Ast;
use crate::ast::string_interner::Identifier;
use crate::parser::{
    semantics::{BindingMode, Literal},
    tokens::Symbol,
};
use entity_component_slab::{make_component, make_world};
use short_typed_index::TypedIndex;
use smallvec::SmallVec;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Node {
    pub id: NodeData,
    // This could be an expression, function or not specified.

    // TODO(perf): These should be stored with struct of arrays.
    pub ty: Option<NodeId>,
    pub equivalents: Option<NodeId>,
    pub lowered_to: Option<usize>,
    pub location: Location,
}
make_world!(
    nodes,
    Node,
    NodeRef,
    unsafe_add_node,
    NodeId,
    NodeData,
    Ast
);

// TODO(clarity): Use macro for defining and registering each of these.
#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum NodeData {
    // TODO(clarity): consider how to split this up.
    // Use a Array of Enums Structs to Struct of Arrays (i.e. AoES2SoA).

    // Variable:
    Identifier(IdentifierId),
    Atom(AtomId),

    // Apply & Abstract:
    Call(CallId),
    Op(OpId),

    // Value:
    Literal(LiteralId),

    // Sugar:
    Definition(DefinitionId),
    NodeRef(NodeId), // Represents an indirection (i.e. when two things have been found to be
    Warning(WarningId), // Represents a warning.
}

make_component!(
    identifiers,
    Identifier,
    IdentifierId,
    add_identifier,
    Ast
);
make_component!(
    literals,
    Literal,
    LiteralId,
    add_literal,
    Ast
);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Warning {
    DoubleAnnotation {
        node_id: NodeId,
        old_ty: NodeId,
        ty: NodeId,
    },
}
make_component!(
    warnings,
    Warning,
    WarningId,
    add_warning,
    Ast
);

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Atom {
    pub name: Identifier,
}
make_component!(
    atoms,
    Atom,
    AtomId,
    add_atom,
    Ast
);

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Call {
    pub inner: NodeId,
    pub args: SmallVec<NodeId, 2>,
}
make_component!(
    calls,
    Call,
    CallId,
    add_call,
    Ast
);

impl Call {
    #[cfg(test)]
    #[must_use]
    pub fn from_slice(inner: NodeId, args: &[NodeId]) -> Self {
        Self {
            inner,
            args: args.into(),
        }
    }
    #[must_use]
    pub fn new(inner: NodeId, args: SmallVec<NodeId, 2>) -> Self {
        Self { inner, args }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Op {
    pub op: Symbol,
    // TODO(perf): Use left: Option<NodeId>, right: Option<NodeId>
    pub args: SmallVec<NodeId, 2>,
}
make_component!(ops, Op, OpId, add_op, Ast);

impl Op {
    #[must_use]
    pub fn new(op: Symbol, args: SmallVec<NodeId, 2>) -> Self {
        Self { op, args }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Definition {
    pub mode: BindingMode,
    pub name: Identifier,
    pub arguments: Option<SmallVec<NodeId, 2>>,
    pub implementation: Option<NodeId>,
}
make_component!(
    definitions,
    Definition,
    DefinitionId,
    add_definition,
    Ast
);
