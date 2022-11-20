use crate::location::Location;
use crate::literal_values::{NamedSymbol, LiteralValues};
use crate::tokens::Symbol;
use crate::utils::typed_index::TypedIndex;
use std::collections::BTreeMap;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;

#[derive(Debug)]
struct InContext<'a, T> {
    value: T,
    ast: &'a Ast,
}

// TODO: String interner?
// TODO: Replace strings where ideal...
// TODO: Use macro for defining and registering each of these.
// TODO: A lambda calculus impl.

pub trait Contains<T> {
    fn get_all(&self) -> &Vec<T>;
    fn get_all_mut(&mut self) -> &mut Vec<T>;
    fn alloc(&mut self, value: T) -> TypedIndex<T> {
        TypedIndex::new(self.get_all_mut(), value)
            .expect("Should never have that many AstNodes of a particular type...")
    }
    fn get(&self, id: TypedIndex<T>) -> &T {
        id.get(self.get_all())
    }
    fn get_mut(&mut self, id: TypedIndex<T>) -> &mut T {
        id.get_mut(self.get_all_mut())
    }
    fn to_node(index: TypedIndex<T>) -> NodeData;
}

macro_rules! make_contains(
    { $field:ident, $type:ty, $kind: ident, $id_type: ident, $alloc_fn_name: ident } => {
        impl Contains<$type> for Ast {
            fn get_all(&self) -> &Vec<$type> {
                &self.$field
            }
            fn get_all_mut(&mut self) -> &mut Vec<$type> {
                &mut self.$field
            }
            fn to_node(index: TypedIndex<$type>) -> NodeData {
                NodeData::$kind(index)
            }
        }

        impl Ast {
            pub fn $alloc_fn_name<T>(&mut self, item: T, location: Location) -> NodeId where (NodeId, T): Into<$type> {
                let node = TypedIndex::next(&self.nodes)
                    .expect("Should always be able to allocate a new Ast Node");
                let id = TypedIndex::new(&mut self.$field, (node, item).into())
                    .expect("Should always be able to allocate a new Ast Node");
                let node: TypedIndex<Node> = TypedIndex::new(&mut self.nodes, Node {
                    id: NodeData::$kind(id),
                    location,
                })
                .expect("Should always be able to allocate a new Ast Node");
                node
            }
        }

        pub type $id_type = TypedIndex<$type>;
     };
);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Node {
    pub id: NodeData,
    pub location: Location,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum NodeData {
    // TODO: consider how to split this up.
    // Use a Array of Enums Structs to Struct of Arrays (i.e. AoES2SoA).
    NodeRef(NodeId), // Hopefully don't need this...???
    NamedSymbol(NamedSymbolId),
    Call(CallId),
    Op(OpId),
    Definition(DefinitionId),
    Literal(LiteralId),
}

#[derive(Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct Ast {
    // Abstract syntax tree... forest
    pub filepath: PathBuf,
    pub roots: Vec<NodeId>,
    pub nodes: Vec<Node>,
    pub calls: Vec<(NodeId, Call)>,
    pub ops: Vec<(NodeId, Op)>,
    pub named_symbols: Vec<(NodeId, NamedSymbol)>,
    pub definitions: Vec<(NodeId, Definition)>,
    pub literals: Vec<(NodeId, Literal)>,
}

impl Ast {
    pub fn new(filepath: PathBuf) -> Self {
        Self {
            filepath,
            ..Self::default()
        }
    }
}

make_contains!(nodes, Node, NodeRef, NodeId, unsafe_add_node);
make_contains!(calls, (NodeId, Call), Call, CallId, add_call);
make_contains!(ops, (NodeId, Op), Op, OpId, add_op);
make_contains!(
    named_symbols,
    (NodeId, NamedSymbol),
    NamedSymbol,
    NamedSymbolId,
    add_named_symbol
);
make_contains!(
    definitions,
    (NodeId, Definition),
    Definition,
    DefinitionId,
    add_definition
);
make_contains!(literals, (NodeId, Literal), Literal, LiteralId, add_literal);

impl Ast {
    pub fn make_node<T>(&mut self, value: T, location: Location) -> NodeId
    where
        Self: Contains<(NodeId, T)>,
    {
        self.make_node_with_id(|_node_id| value, location)
    }

    pub fn make_node_with_id<T, F: FnOnce(NodeId) -> T>(
        &mut self,
        value: F,
        location: Location,
    ) -> NodeId
    where
        Self: Contains<(NodeId, T)>,
    {
        let node_id =
            TypedIndex::next(&self.nodes).expect("Should never have that many AstNodes..."); // Reserve it...
        let value_id = self.alloc((node_id, value(node_id)));
        let node = Node {
            id: Self::to_node(value_id),
            location,
        };
        let new_node_id = TypedIndex::new(&mut self.nodes, node)
            .expect("Should never have that many AstNodes...");
        assert_eq!(node_id, new_node_id);
        new_node_id
    }
    pub fn set_root(&mut self, new_root: NodeId) {
        self.roots.push(new_root);
    }
}

impl<'a> std::fmt::Display for InContext<'a, NamedSymbol> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(s) = self.ast.get_str(self.value) {
            write!(f, "{s}")
        } else {
            write!(f, "<unknown symbol: {self:?}>")
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Call {
    pub inner: NodeId,
    pub args: Vec<NodeId>, // TODO: Short vec
}

impl Call {
    #[cfg(test)]
    pub fn from_slice(inner: NodeId, args: &[NodeId]) -> Self {
        Self {
            inner,
            args: args.to_vec(),
        }
    }
    pub fn new(inner: NodeId, args: Vec<NodeId>) -> Self {
        Self { inner, args }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Op {
    pub op: Symbol,
    pub args: [Option<NodeId>; 2],
}

impl Op {
    pub fn new(op: Symbol, args: [Option<NodeId>; 2]) -> Self {
        Self { op, args }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Definition {
    pub name: NamedSymbol,
    pub implementation: NodeId,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum Literal {
    Bool(bool),    // a boolean of arbitrary size :P (true/false)
    Numeric(String), // an Integer or Float of arbitrary size
    Text(String),    // a character or strings of arbitrary size (e.g. UTF-8 or Unicode)
             // TODO: Add more complex literals like:
             // Rational, e.g. 12
             // Color,
             // URL,
             // JSON,
             // JSON,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_add_nodes_to_ast() {
        let mut lits = LiteralValues::default();
        let mut ast = Ast::default();
        let a = lits.register_str("a".to_string());
        let b = Literal::Numeric("123456789".to_string());
        let a = ast.make_node(a, Location::dummy_for_test());
        let b = ast.make_node(b, Location::dummy_for_test());
        let call = Op {
            op: Symbol::Add,
            args: [Some(a), Some(b)],
        };
        let call = ast.make_node(call, Location::dummy_for_test());
        let a_prime = lits.register_str("a_prime".to_string());
        let definition = Definition {
            name: a_prime,
            implementation: call,
        };
        let definition = ast.make_node(definition, Location::dummy_for_test());
        ast.set_root(definition);
        dbg!(&ast);

        assert_eq!(ast.nodes.len(), 5);
        assert_eq!(ast.named_symbols.len(), 1);
        assert_eq!(ast.literals.len(), 1);
        assert_eq!(ast.calls.len(), 1);
        assert_eq!(ast.definitions.len(), 1);
        assert_eq!(ast.roots, vec![definition]);
    }
}
