use short_typed_index::TypedIndex;
// TODO(clarity): Use macro for defining and registering each of these.

pub trait World<Archetypes> {
    // type ...
}

pub trait Contains<T, Archetypes> {
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
    fn to_node(index: TypedIndex<T>) -> Archetypes;
}

#[macro_export]
macro_rules! make_contains(
    { $field:ident, $type:ty, $kind: ident, $id_type: ident, $alloc_fn_name: ident, $root_id: ident, $archetypes: ident, $world: ident } => {
        impl $crate::Contains<$type, $archetypes> for $world {
            fn get_all(&self) -> &Vec<$type> {
                &self.$field
            }
            fn get_all_mut(&mut self) -> &mut Vec<$type> {
                std::sync::Arc::make_mut(&mut self.$field)
            }
            fn to_node(index: TypedIndex<$type>) -> $archetypes {
                $archetypes::$kind(index)
            }
        }

        impl $world {
            pub fn $alloc_fn_name<T>(&mut self, item: T, location: Location) -> $root_id where ($root_id, T): Into<$type> {
                let node = TypedIndex::next(&self.nodes)
                    .expect("Should always be able to allocate a new Ast Node");
                let id = TypedIndex::new(std::sync::Arc::make_mut(&mut self.$field), (node, item).into())
                    .expect("Should always be able to allocate a new Ast Node");
                let node: TypedIndex<Node> = TypedIndex::new(std::sync::Arc::make_mut(&mut self.nodes), Node {
                    id: $archetypes::$kind(id),
                    equivalents: None,
                    lowered_to: None,
                    ty: None,
                    location,
                })
                .expect("Should always be able to allocate a new Ast Node");
                node
            }
        }

        pub type $id_type = TypedIndex<$type>;

        impl std::ops::Index<$id_type> for $world {
            type Output = $type;

            fn index(&self, id: $id_type) -> &Self::Output {
                use $crate::Contains;
                id.get(self.get_all())
            }
        }

        impl std::ops::IndexMut<$id_type> for $world {
            fn index_mut(&mut self, id: $id_type) -> &mut Self::Output {
                use $crate::Contains;
                id.get_mut(self.get_all_mut())
            }
        }
     };
);

#[macro_export]
macro_rules! make_world(
    { $field:ident, $type:ty, $kind: ident, $id_type: ident, $alloc_fn_name: ident, $root_id: ident, $archetypes: ident, $world: ident } => {

    $crate::make_contains!($field, $type, $kind, $id_type, $alloc_fn_name, $root_id, $archetypes, $world);
                                                                                                                                  }
);

#[macro_export]
macro_rules! make_component(
    { $field:ident, $kind: ident, $id_type: ident, $alloc_fn_name: ident, $root_id: ident, $archetypes: ident, $world: ident } => {

    $crate::make_contains!($field, ($root_id, $kind), $kind, $id_type, $alloc_fn_name, $root_id, $archetypes, $world);
                                                                                                                                            }
);
