use crate::errors::TError;
use crate::location::Loc;
use crate::primitives::Val;
use specs::prelude::*;
use specs::Component;
use std::collections::BTreeSet;

// TODO: Use macro for defining and registering each of these.
#[derive(Component, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct File {
    path: String, // TODO: Use something 'right'
    root: Entity,
    contents: String,
}

#[derive(Component, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct Symbol {
    name: TypedIndex<Identifier>, // index into the file
    file_id: TypedIndex<File>,
}

#[derive(Component, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct Location {
    location: usize, // index into the file
    file_id: TypedIndex<File>,
}

#[derive(Component, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct Call {
    pub inner: Entity,
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

#[derive(Component, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct Definition {
    pub name: TypedIndex<Symbol>,
    pub implementation: Entity,
}
