use crate::error::TError;
use crate::location::Location;
use crate::typed_index::TypedIndex;
use crate::tokens::Token;

// TODO: Replace strings where ideal...
#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct File {
    path: String, // TODO: Use something 'right'
    root: Entity,
    contents: String,
    lexed: Option<Vec<Token>>,
}
pub type FileId = TypedIndex<File>;

#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Module {
    file: FileId,
    root: Entity,
}
pub type ModuleId = TypedIndex<Module>;

#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Error {
    file: FileId,
    module: ModuleId,
    location: LocationId,
    error: TError,
}
pub type ErrorId = TypedIndex<Error>;
