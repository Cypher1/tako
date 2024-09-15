pub use paste::paste;
use short_typed_index::TypedIndex;
use std::sync::Arc;

// Note: Arc is used here to allow passes to make cheap copies of the 'slab'.
// Each system get's its own Arc when mutating, but uses the Arcs of the previous
// system otherwise.
// TODO(clarity): Consider newtype-ing...
pub type Slab<T> = Arc<Vec<T>>;
pub type ChildSlab<T, EntityId> = Slab<(EntityId, T)>;

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

pub trait ContainsSlab<T, Archetypes>: World {
    fn get_all(&self) -> &Vec<T>;
    fn get_all_mut(&mut self) -> &mut Vec<T>;
    fn next_internal(&mut self) -> TypedIndex<T> {
        TypedIndex::next(self.get_all())
            .expect("Should always be able to allocate a new entity")
    }
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
}

#[macro_export]
macro_rules! make_contains_slab(
    { $world: ident, $field:ident, $type:ty, $id_type: ident} => {
        impl $crate::ContainsSlab<$type, <$world as $crate::World>::Archetypes> for $world {
            fn get_all(&self) -> &Vec<$type> {
                &self.$field
            }
            fn get_all_mut(&mut self) -> &mut Vec<$type> {
                // TODO(perf): This may be slow as it must check that the Arc is 'safe' to mutate
                // or must copy it.
                // We could do this statically...
                std::sync::Arc::make_mut(&mut self.$field)
            }
        }

        pub type $id_type = TypedIndex<$type>;

        impl std::ops::Index<$id_type> for $world {
            type Output = $type;

            fn index(&self, id: $id_type) -> &Self::Output {
                use $crate::ContainsSlab;
                id.get(self.get_all())
            }
        }

        impl std::ops::IndexMut<$id_type> for $world {
            fn index_mut(&mut self, id: $id_type) -> &mut Self::Output {
                // TODO(perf): Add a clippy warning that this isn't 'fast'.
                // Instead get_all_mut should be called once and operated on.
                use $crate::ContainsSlab;
                id.get_mut(self.get_all_mut())
            }
        }
     };
);

#[macro_export]
macro_rules! make_world(
{ $world: ident, $field:ident, $type:ty, $archetypes: ident, $meta: ty, $alloc_lambda: expr } => {
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
    $crate::make_contains_slab!($world, $field, $type, [<$type Id>]);
}});

#[macro_export]
macro_rules! make_component_helper(
    { $world: ident, $field:ident, $type:ty, $kind: ident, $id_type: ident, $alloc_fn_name: ident} => {

    impl $world {
        pub fn $alloc_fn_name<T>(&mut self, item: T, meta: <$world as $crate::World>::EntityMeta) -> <$world as $crate::World>::EntityId where (<$world as $crate::World>::EntityId, T): Into<$type> {
            use $crate::{ContainsSlab, World};
            let next_entity = self.next_internal();
            let id = self.alloc_internal((next_entity, item).into());
            let entity_data = self.new_entity(<$world as $crate::World>::Archetypes::$kind(id), meta);
            let entity = self.alloc_internal(entity_data);
            debug_assert_eq!(entity, next_entity, "The new entity should be the next entity");
            entity
        }
    }

    $crate::make_contains_slab!($world, $field, $type, $id_type);
                                                                                                      }
);
#[macro_export]
macro_rules! make_component(
{ $world: ident, $field:ident, $kind: ident } => {
$crate::paste!{
    $crate::make_component_helper!($world, $field, (<$world as $crate::World>::EntityId, $kind), $kind, [<$kind Id>], [<add_ $kind:lower>]);
}});
