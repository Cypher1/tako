use super::nodes::NodeData;
use crate::primitives::typed_index::TypedIndex;
// TODO(clarity): Use macro for defining and registering each of these.

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
                std::sync::Arc::make_mut(&mut self.$field)
            }
            fn to_node(index: TypedIndex<$type>) -> NodeData {
                NodeData::$kind(index)
            }
        }

        // TODO: Make generic over types.
        impl Ast {
            pub fn $alloc_fn_name<T>(&mut self, item: T, location: Location) -> NodeId where (NodeId, T): Into<$type> {
                let node = TypedIndex::next(&self.nodes)
                    .expect("Should always be able to allocate a new Ast Node");
                let id = TypedIndex::new(std::sync::Arc::make_mut(&mut self.$field), (node, item).into())
                    .expect("Should always be able to allocate a new Ast Node");
                let node: TypedIndex<Node> = TypedIndex::new(std::sync::Arc::make_mut(&mut self.nodes), Node {
                    id: NodeData::$kind(id),
                    equivalents: None,
                    ty: None,
                    location,
                })
                .expect("Should always be able to allocate a new Ast Node");
                node
            }
        }

        pub type $id_type = TypedIndex<$type>;
     };
);
