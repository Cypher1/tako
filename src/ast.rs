use crate::concepts::FileId;
use crate::free_standing::typed_index::TypedIndex;
use crate::location::Location;
use crate::string_interner::StrId;
use soa_derive::StructOfArray;

// TODO: Replace strings where ideal...
// TODO: Use macro for defining and registering each of these.
// TODO: A lambda calculus impl.

#[derive(StructOfArray, Eq, PartialEq)]
#[soa_attr(Vec, derive(Debug))]
#[soa_attr(Vec, derive(Default))]
pub struct Symbol {
    node: NodeId,
    name: TypedIndex<StrId>, // index into the file
    file_id: FileId,
}
pub type SymbolId = TypedIndex<Symbol>;

#[derive(StructOfArray, Debug, Eq, PartialEq)]
#[soa_attr(Vec, derive(Debug))]
#[soa_attr(Vec, derive(Default))]
pub struct Call {
    node: NodeId,
    pub inner: NodeId,
    pub args: Vec<NodeId>, // TODO: Short vec
}
pub type CallId = TypedIndex<Call>;

impl Call {
    #[cfg(test)]
    pub fn new(node: NodeId, inner: NodeId, args: &[NodeId]) -> Self {
        Self {
            node,
            inner,
            args: args.to_vec(),
        }
    }
}

#[derive(StructOfArray, Debug, Eq, PartialEq)]
#[soa_attr(Vec, derive(Debug))]
#[soa_attr(Vec, derive(Default))]
pub struct Definition {
    node: NodeId,
    pub name: TypedIndex<StrId>,
    pub implementation: NodeId,
}
pub type DefinitionId = TypedIndex<Definition>;

#[derive(StructOfArray, Debug, Eq, PartialEq)]
#[soa_attr(Vec, derive(Debug))]
#[soa_attr(Vec, derive(Default))]
pub struct Primitive {
    node: NodeId,
    pub value: (), // TODO: ???
}
pub type PrimitiveId = TypedIndex<Primitive>;

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum NodeData {
    // TODO: consider how to split this up.
    // Use a Array of Enums Structs to Struct of Arrays (i.e. AoES2SoA).
    Symbol(SymbolId),
    Call(CallId),
    Definition(DefinitionId),
    Primitive(PrimitiveId),
}

#[derive(StructOfArray, Debug, Eq, PartialEq)]
#[soa_attr(Vec, derive(Debug))]
#[soa_attr(Vec, derive(Default))]
pub struct Node {
    pub name: TypedIndex<Symbol>,
    pub id: NodeData,
    pub location: Location,
}
pub type NodeId = TypedIndex<Node>;

#[derive(Default, Debug)]
pub struct Ast {
    // Abstract syntax tree... forest
    pub roots: Vec<NodeId>,
    pub nodes: NodeVec,
    pub calls: CallVec,
    pub symbols: SymbolVec,
    pub definitions: DefinitionVec,
    pub primitives: PrimitiveVec,
}
