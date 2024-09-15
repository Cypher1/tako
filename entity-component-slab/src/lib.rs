pub use paste::paste;
use short_typed_index::TypedIndex;
// TODO(clarity): Use macro for defining and registering each of these.

pub trait World {
    type EntityType;
    type EntityId;
    type Archetypes;
    type EntityMeta;

    fn new_entity(
        &mut self,
        archetype: Self::Archetypes,
        meta: Self::EntityMeta,
    ) -> Self::EntityType;
}

pub trait Contains<T, Archetypes>: World {
    fn get_all(&self) -> &Vec<T>;
    fn get_all_mut(&mut self) -> &mut Vec<T>;
    fn alloc_internal(&mut self, value: T) -> TypedIndex<T> {
        TypedIndex::new(self.get_all_mut(), value)
            .expect("Should never have that many entities of a particular type...")
    }
    fn get(&self, id: TypedIndex<T>) -> &T {
        id.get(self.get_all())
    }
    fn get_mut(&mut self, id: TypedIndex<T>) -> &mut T {
        id.get_mut(self.get_all_mut())
    }
    fn to_entity(index: TypedIndex<T>) -> Archetypes;
}

#[macro_export]
macro_rules! make_contains(
    { $field:ident, $type:ty, $kind: ident, $id_type: ident, $alloc_fn_name: ident, $world: ident} => {
        impl $crate::Contains<$type, <$world as $crate::World>::Archetypes> for $world {
            fn get_all(&self) -> &Vec<$type> {
                &self.$field
            }
            fn get_all_mut(&mut self) -> &mut Vec<$type> {
                std::sync::Arc::make_mut(&mut self.$field)
            }
            fn to_entity(index: TypedIndex<$type>) -> <$world as $crate::World>::Archetypes {
                <$world as $crate::World>::Archetypes::$kind(index)
            }
        }

        impl $world {
            pub fn $alloc_fn_name<T>(&mut self, item: T, meta: <$world as $crate::World>::EntityMeta) -> <$world as $crate::World>::EntityId where (<$world as $crate::World>::EntityId, T): Into<$type> {
                use $crate::{Contains, World};
                let next_entity: TypedIndex<<$world as $crate::World>::EntityType> = TypedIndex::next(&self.get_all())
                    .expect("Should always be able to allocate a new entity");
                let id = TypedIndex::new(&mut self.get_all_mut(), (next_entity, item).into())
                    .expect("Should always be able to allocate a new entity");
                let entity_data = self.new_entity(<$world as $crate::World>::Archetypes::$kind(id), meta);
                let entity: TypedIndex<<$world as $crate::World>::EntityType> = TypedIndex::new(
                    &mut self.get_all_mut(),
                    entity_data
                ).expect("Should always be able to allocate a new entity");
                entity
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
{ $field:ident, $type:ty, $kind: ident, $archetypes: ident, $meta: ty, $world: ident, $alloc_lambda: expr } => {
$crate::paste!{
    impl $crate::World for $world {
        type EntityType = $type;
        type EntityId = [<$type Id>];
        type Archetypes = $archetypes;
        type EntityMeta = $meta;

        fn new_entity(&mut self, archetype: Self::Archetypes, meta: Self::EntityMeta) -> Self::EntityType {
            $alloc_lambda(archetype, meta)
        }
    }
    $crate::make_contains!($field, $type, $kind, [<$type Id>], [<add_ $kind:lower>], $world);
}});

#[macro_export]
macro_rules! make_component(
{ $field:ident, $kind: ident, $world: ident } => {
$crate::paste!{
    $crate::make_contains!($field, (<$world as $crate::World>::EntityId, $kind), $kind, [<$kind Id>], [<add_ $kind:lower>], $world);
}});
