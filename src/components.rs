use crate::ast::Path;
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
    location: u32, // index into the file
    length: u8, // length of the symbol
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

#[derive(Component, Clone, Default, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct SymbolRef {
    pub name: Path,
    pub context: Path,
    pub definition: Option<Entity>,
}

#[derive(Component, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct Definition {
    pub name: TypedIndex<Symbol>,
    pub implementation: Entity,
}
