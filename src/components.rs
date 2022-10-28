use crate::errors::TError;
use crate::location::Loc;
use crate::typed_index::TypedIndex;

// TODO: Replace strings where ideal...
// TODO: Use macro for defining and registering each of these.

#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct File {
    path: String, // TODO: Use something 'right'
    root: Entity,
    contents: String,
}
pub type FileId = TypedIndex<File>;

#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct EntryPoint {
    function_name: String,
    file: FileId,
}
pub type EntryPointId = TypedIndex<EntryPoint>;

#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Module {
    file: FileId,
    entry_points: Vec<EntryPointId>,
    root: Entity,
}
pub type ModuleId = TypedIndex<Module>;

#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Error {
    file: FileId,
    module: ModuleId,
    entry_points: Vec<EntryPointId>,
    ast: Entity,
    error: TError,
}
pub type ErrorId = TypedIndex<Error>;

#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Symbol {
    name: TypedIndex<Identifier>, // index into the file
    file_id: TypedIndex<File>,
}

#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Location {
    location: usize, // index into the file
    file_id: TypedIndex<File>,
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
