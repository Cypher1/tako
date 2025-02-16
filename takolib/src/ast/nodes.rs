use super::location::Location;
use super::*;
use crate::ast::string_interner::Identifier;
use crate::parser::semantics::BindingMode;
use smallvec::SmallVec;

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct NodeTypes {
    // TS Node Ids
    // TODO: generate from
    // ./tree_sitter_tako/src/node-types.json | jq "map(.type)"
    pub _add: TsNodeId,
    pub _and: TsNodeId,
    pub _arrow: TsNodeId,
    pub _assign: TsNodeId,
    pub _binding: TsNodeId,
    pub _bit_and: TsNodeId,
    pub _bit_not: TsNodeId,
    pub _bit_or: TsNodeId,
    pub _bit_xor: TsNodeId,
    pub _block: TsNodeId,
    pub _call: TsNodeId,
    pub _color: TsNodeId,
    pub _container: TsNodeId,
    pub _div: TsNodeId,
    pub _equals: TsNodeId,
    pub _exp: TsNodeId,
    pub _field: TsNodeId,
    pub _format_expression: TsNodeId,
    pub _greater_than: TsNodeId,
    pub _greater_than_equals: TsNodeId,
    pub _has_type: TsNodeId,
    pub _hex_literal: TsNodeId,
    pub _index: TsNodeId,
    pub _left_shift: TsNodeId,
    pub _less_than: TsNodeId,
    pub _less_than_equals: TsNodeId,
    pub _mod: TsNodeId,
    pub _mul: TsNodeId,
    pub _neg: TsNodeId,
    pub _nesting_comment: TsNodeId,
    pub _not: TsNodeId,
    pub _not_equals: TsNodeId,
    pub _or: TsNodeId,
    pub _parens: TsNodeId,
    pub _range: TsNodeId,
    pub _right_shift: TsNodeId,
    pub _sequence: TsNodeId,
    pub _set: TsNodeId,
    pub _shebang: TsNodeId,
    pub _single_line_comment: TsNodeId,
    pub _source_file: TsNodeId,
    pub _spread: TsNodeId,
    pub _string_literal: TsNodeId,
    pub _sub: TsNodeId,
    pub _try: TsNodeId,
    pub _escape_sequence: TsNodeId,
    pub _exists: TsNodeId,
    pub _float_literal: TsNodeId,
    pub _forall: TsNodeId,
    pub _given: TsNodeId,
    pub _heading: TsNodeId,
    pub _ident: TsNodeId,
    pub _int_literal: TsNodeId,
}


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
