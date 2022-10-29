use crate::error::TError;
use crate::location::Location;
use crate::free_standing::typed_index::TypedIndex;
use crate::tokens::Token;
use crate::ast::*;
use crate::string_interner::StrInterner;
use soa_derive::StructOfArray;

// TODO: Replace strings where ideal...
#[derive(StructOfArray)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct File {
    pub path: String, // TODO: Use something 'right'
    pub root: ModuleId,
    pub contents: String,
    string_interner: StrInterner,
    lexed: Option<Vec<Token>>,
    ast: Ast,
}
pub type FileId = TypedIndex<File>;

#[derive(StructOfArray)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Module {
    pub file: FileId,
    pub parent: Option<ModuleId>,
    pub children: Vec<ModuleId>,
    pub root: NodeId,
}
pub type ModuleId = TypedIndex<Module>;

#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Error {
    pub file: FileId,
    pub module: ModuleId,
    pub location: Location,
    pub error: TError,
}
pub type ErrorId = TypedIndex<Error>;
