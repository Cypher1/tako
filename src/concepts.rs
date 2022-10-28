use crate::error::TError;
use crate::location::Location;
use crate::free_standing::typed_index::TypedIndex;
use crate::tokens::Token;
use crate::ast::*;
use soa_derive::StructOfArray;

// TODO: Replace strings where ideal...
#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct File {
    path: String, // TODO: Use something 'right'
    root: ModuleId,
    contents: String,
    lexed: Option<Vec<Token>>,
    ast: Ast,
}
pub type FileId = TypedIndex<File>;

#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Module {
    file: FileId,
    parent: Option<ModuleId>,
    children: Vec<ModuleId>,
    root: NodeId,
}
pub type ModuleId = TypedIndex<Module>;

#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Error {
    file: FileId,
    module: ModuleId,
    location: Location,
    error: TError,
}
pub type ErrorId = TypedIndex<Error>;
