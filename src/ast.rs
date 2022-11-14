use crate::location::Location;
use crate::tokens::Symbol;
use crate::utils::typed_index::TypedIndex;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use static_assertions::*;
assert_eq_size!(NamedSymbol, [u8; 8]);
assert_eq_size!([NamedSymbol; 2], [u8; 16]);

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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Node {
    pub id: NodeData,
    pub location: Location,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum NodeData {
    // TODO: consider how to split this up.
    // Use a Array of Enums Structs to Struct of Arrays (i.e. AoES2SoA).
    NodeRef(NodeId), // Hopefully don't need this...???
    Symbol(SymbolId),
    NamedSymbol(NamedSymbolId),
    Call(CallId),
    Definition(DefinitionId),
    Literal(LiteralId),
}

type StringHash = u64;

#[derive(Clone, Default, Debug)]
pub struct Ast {
    // Abstract syntax tree... forest
    pub file: String,
    pub roots: Vec<NodeId>,
    pub nodes: Vec<Node>,
    pub calls: Vec<(NodeId, Call)>,
    pub symbols: Vec<(NodeId, Symbol)>,
    pub named_symbols: Vec<(NodeId, NamedSymbol)>,
    pub definitions: Vec<(NodeId, Definition)>,
    pub literals: Vec<(NodeId, Literal)>,
    // This ensures we can look up the string from the hash.
    // BUT: We can also merge the hashes without losing any information.
    pub strings: HashMap<StringHash, String>,
}

impl Ast {
    pub fn new(file: &str) -> Self {
        Self {
            file: file.to_string(),
            ..Self::default()
        }
    }
}

make_contains!(nodes, Node, NodeRef, NodeId, unsafe_add_node);
make_contains!(calls, (NodeId, Call), Call, CallId, add_call);
make_contains!(symbols, (NodeId, Symbol), Symbol, SymbolId, add_symbol);
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
    pub fn register_str(&mut self, name: String) -> StrId {
        let mut hasher = fxhash::FxHasher::default();
        name.hash(&mut hasher);
        let str_hash = hasher.finish();
        self.strings.entry(str_hash).or_insert(name);
        TypedIndex::from_raw(str_hash)
    }
    pub fn get_str(&self, s: StrId) -> Option<&str> {
        self.strings
            .get(&s.raw_index())
            .map(|ref_string| &**ref_string)
    }
}

type StrId = TypedIndex<String, StringHash>;
// Ensures that str ids are unique per string but also stable across different files etc.

pub type NamedSymbol = StrId;

impl<'a> std::fmt::Display for InContext<'a, NamedSymbol> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(s) = self.ast.get_str(self.value) {
            write!(f, "{}", s)
        } else {
            write!(f, "<unknown symbol: {:?}>", self)
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Call {
    pub inner: NodeId,
    pub args: Vec<NodeId>, // TODO: Short vec
}

impl Call {
    #[cfg(test)]
    pub fn new(inner: NodeId, args: &[NodeId]) -> Self {
        Self {
            inner,
            args: args.to_vec(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Definition {
    pub name: StrId,
    pub implementation: NodeId,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Literal {
    Bool,    // a boolean of arbitrary size :P (true/false)
    Numeric, // an Integer or Float of arbitrary size
    Text,    // a character or strings of arbitrary size (e.g. UTF-8 or Unicode)
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
        let mut ast = Ast::default();
        let a = ast.register_str("a".to_string());
        let plus = Symbol::Add;
        let b = Literal::Numeric; // 123456789
        let plus = ast.make_node(plus, Location::dummy_for_test());
        let a = ast.make_node(a, Location::dummy_for_test());
        let b = ast.make_node(b, Location::dummy_for_test());
        let call = Call {
            inner: plus,
            args: vec![a, b],
        };
        let call = ast.make_node(call, Location::dummy_for_test());
        let a_prime = ast.register_str("a_prime".to_string());
        let definition = Definition {
            name: a_prime,
            implementation: call,
        };
        let definition = ast.make_node(definition, Location::dummy_for_test());
        ast.set_root(definition);
        dbg!(&ast);

        assert_eq!(ast.nodes.len(), 5);
        assert_eq!(ast.symbols.len(), 2);
        assert_eq!(ast.literals.len(), 1);
        assert_eq!(ast.calls.len(), 1);
        assert_eq!(ast.definitions.len(), 1);
        assert_eq!(ast.roots, vec![definition]);
    }
}
