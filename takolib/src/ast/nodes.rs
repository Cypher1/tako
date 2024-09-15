use super::location::Location;
use super::*;
use crate::ast::string_interner::Identifier;
use crate::parser::{semantics::BindingMode, tokens::Symbol};
use smallvec::SmallVec;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Node {
    pub id: NodeData,
    // This could be an expression, function or not specified.

    // TODO(perf): The following should be stored with struct of arrays.
    pub ty: Option<NodeId>,
    // TODO(perf, clarity): Should not be a linked list.
    pub equivalents: Option<NodeId>,
    // TODO(perf, clarity): Should not be a usize.
    pub lowered_to: Option<usize>,
    pub location: Location,
}

// TODO(clarity): Use macro for defining and registering each of these.
// TODO(perf): Consider a tag and index pair as they should all be the same size and shape.
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
    NodeRef(NodeId), // Represents an indirection (i.e. when two things have been found to be the same)
    Warning(WarningId), // Represents a warning.
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Warning {
    DoubleAnnotation {
        node_id: NodeId,
        old_ty: NodeId,
        ty: NodeId,
    },
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Atom {
    pub name: Identifier,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Call {
    pub inner: NodeId,
    pub args: SmallVec<NodeId, 2>,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Op {
    pub op: Symbol,
    // TODO(perf): Use left: Option<NodeId>, right: Option<NodeId>
    pub args: SmallVec<NodeId, 2>,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Definition {
    pub mode: BindingMode,
    pub name: Identifier,
    pub arguments: Option<SmallVec<NodeId, 2>>,
    pub implementation: Option<NodeId>,
}
