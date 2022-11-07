use crate::free_standing::typed_index::TypedIndex;
use crate::location::Location;

// TODO: String interner?
// TODO: Replace strings where ideal...
// TODO: Use macro for defining and registering each of these.
// TODO: A lambda calculus impl.

trait Contains<T> {
    fn get_all(&self) -> &Vec<T>;
    fn get_all_mut(&mut self) -> &mut Vec<T>;
    fn add(&mut self, value: T) -> TypedIndex<T> {
        TypedIndex::new(self.get_all_mut(), value).expect("Should never have that many AstNodes...")
    }
    fn get(&self, id: TypedIndex<T>) -> &T {
        id.get(self.get_all())
    }
    fn get_mut(&mut self, id: TypedIndex<T>) -> &mut T {
        id.get_mut(self.get_all_mut())
    }
}

macro_rules! make_contains(
    { $field:ident, $type:ty, $id_type: ident } => {
        impl Contains<$type> for Ast {
            fn get_all(&self) -> &Vec<$type> {
                &self.$field
            }
            fn get_all_mut(&mut self) -> &mut Vec<$type> {
                &mut self.$field
            }
        }
        type $id_type = TypedIndex<$type>;
     };
);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Node {
    pub name: TypedIndex<Symbol>,
    pub id: NodeData,
    pub location: Location,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum NodeData {
    // TODO: consider how to split this up.
    // Use a Array of Enums Structs to Struct of Arrays (i.e. AoES2SoA).
    Symbol(SymbolId),
    Call(CallId),
    Definition(DefinitionId),
    Primitive(PrimitiveId),
}

#[derive(Clone, Default, Debug)]
pub struct Ast {
    // Abstract syntax tree... forest
    pub roots: Vec<NodeId>,
    pub nodes: Vec<Node>,
    pub calls: Vec<Call>,
    pub symbols: Vec<Symbol>,
    pub definitions: Vec<Definition>,
    pub primitives: Vec<Primitive>,
}

make_contains!(nodes, Node, NodeId);
make_contains!(calls, Call, CallId);
make_contains!(symbols, Symbol, SymbolId);
make_contains!(definitions, Definition, DefinitionId);
make_contains!(primitives, Primitive, PrimitiveId);

type StrId = String; // TODO: replace with an interned string id.

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Symbol {
    node: NodeId,
    name: TypedIndex<StrId>, // index into the file
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Call {
    node: NodeId,
    pub inner: NodeId,
    pub args: Vec<NodeId>, // TODO: Short vec
}

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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Definition {
    node: NodeId,
    pub name: TypedIndex<StrId>,
    pub implementation: NodeId,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Primitive {
    node: NodeId,
    pub value: (), // TODO: ???
}
