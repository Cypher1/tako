use super::contains::Contains;
use super::location::Location;
use super::Ast;
use crate::ast::string_interner::Identifier;
use crate::parser::{
    semantics::{BindingMode, Literal},
    tokens::Symbol,
};
use crate::primitives::typed_index::TypedIndex;
use smallvec::SmallVec;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Node {
    pub id: NodeData,
    // This could be an expression, function or not specified.
    pub ty: Option<NodeId>,
    pub equivalents: Option<NodeId>,
    pub location: Location,
}
make_contains!(nodes, Node, NodeRef, NodeId, unsafe_add_node);

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

make_contains!(
    identifiers,
    (NodeId, Identifier),
    Identifier,
    IdentifierId,
    add_identifier
);
make_contains!(literals, (NodeId, Literal), Literal, LiteralId, add_literal);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Warning {
    DoubleAnnotation {
        node_id: NodeId,
        old_ty: NodeId,
        ty: NodeId,
    },
}
make_contains!(warnings, (NodeId, Warning), Warning, WarningId, add_warning);

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Atom {
    pub name: Identifier,
}
make_contains!(atoms, (NodeId, Atom), Atom, AtomId, add_atom);

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Call {
    pub inner: NodeId,
    pub args: SmallVec<[NodeId; 2]>,
}
make_contains!(calls, (NodeId, Call), Call, CallId, add_call);

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
    pub fn new(inner: NodeId, args: SmallVec<[NodeId; 2]>) -> Self {
        Self { inner, args }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Op {
    pub op: Symbol,
    pub args: SmallVec<[NodeId; 2]>,
}
make_contains!(ops, (NodeId, Op), Op, OpId, add_op);

impl Op {
    #[must_use]
    pub fn new(op: Symbol, args: SmallVec<[NodeId; 2]>) -> Self {
        Self { op, args }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Definition {
    pub mode: BindingMode,
    pub name: Identifier,
    pub bindings: Option<SmallVec<[NodeId; 2]>>,
    pub implementation: Option<NodeId>,
}
make_contains!(
    definitions,
    (NodeId, Definition),
    Definition,
    DefinitionId,
    add_definition
);
