use super::location::Location;
use super::*;
use crate::ast::string_interner::Identifier;
use crate::parser::semantics::BindingMode;
use crate::parser::tokens::Symbol;
use smallvec::SmallVec;

pub const OP_ARGS_STANDARD_ITEM_NUM: usize = 2;
pub const CALL_ARGS_STANDARD_ITEM_NUM: usize = 5;
pub const FMT_STR_STANDARD_ITEM_NUM: usize = 2; // TODO: Convert to a different store

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

    // Apply & Abstract:
    Call(CallId),
    Op(OpId),

    // Value:
    Literal(LiteralId),

    // Sugar:
    Definition(DefinitionId),
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
pub struct Call {
    pub inner: NodeId,
    pub args: SmallVec<NodeId, CALL_ARGS_STANDARD_ITEM_NUM>,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Op {
    pub op: Symbol,
    // TODO(perf): Use left: Option<NodeId>, right: Option<NodeId>
    pub args: SmallVec<NodeId, OP_ARGS_STANDARD_ITEM_NUM>,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Definition {
    pub mode: BindingMode,
    pub name: IdentifierId,
    pub arguments: Option<SmallVec<NodeId, CALL_ARGS_STANDARD_ITEM_NUM>>,
    pub implementation: Option<NodeId>,
}
