use crate::location::Loc;
use crate::typed_index::TypedIndex;

// TODO: Replace strings where ideal...
// TODO: Use macro for defining and registering each of these.

#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Symbol {
    name: TypedIndex<Identifier>, // index into the file
    file_id: FileId,
}

#[derive(StructOfArray, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
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

#[derive(StructOfArray, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Definition {
    pub name: TypedIndex<Symbol>,
    pub implementation: Entity,
}
