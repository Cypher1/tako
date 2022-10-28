use crate::location::Location;
use crate::free_standing::typed_index::TypedIndex;
use soa_derive::StructOfArray;

// TODO: Replace strings where ideal...
// TODO: Use macro for defining and registering each of these.
// TODO: A lambda calculus impl.

#[derive(StructOfArray, Eq, PartialEq)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Symbol {
    name: TypedIndex<Identifier>, // index into the file
    file_id: FileId,
}

#[derive(StructOfArray, Debug, Eq, PartialEq)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Call {
    pub inner: NodeId,
    pub args: Vec<Entity>, // TODO: Short vec
}

impl Call {
    #[cfg(test)]
    pub fn new(inner: Entity, args: &[Entity]) -> Self {
        Self {
            inner,
            args: args.to_vec(),
        }
    }
}

#[derive(StructOfArray, Debug, Eq, PartialEq)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Definition {
    pub name: TypedIndex<Symbol>,
    pub implementation: Entity,
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum NodeData {
    // TODO: consider how to split this up.
    // Use a Array of Enums Structs to Struct of Arrays (i.e. AoES2SoA).
    Symbol(SymbolId),
    Call(CallId),
    Definition(DefinitionId),
}

#[derive(StructOfArray, Debug, Eq, PartialEq)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Node {
    pub name: TypedIndex<Symbol>,
    pub id: NodeData,
}
pub type NodeId = NodeId<Node>;
