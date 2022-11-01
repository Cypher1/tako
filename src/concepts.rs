use crate::error::TError;
use crate::location::Location;
use crate::free_standing::typed_index::TypedIndex;
use crate::tokens::Token;
use crate::ast::*;
use crate::string_interner::StrInterner;
use soa_derive::StructOfArray;

// TODO: Replace strings where ideal...
#[derive(StructOfArray, Debug)]
#[soa_attr(Vec, derive(Debug))]
#[soa_attr(Vec, derive(Default))]
pub struct File {
    pub path: String, // TODO: Use something 'right'
    pub root: ModuleId,
    pub contents: String,
    pub string_interner: StrInterner,
    pub lexed: Option<Vec<Token>>,
    pub ast: Ast,
}
pub type FileId = TypedIndex<File>;

impl File {
    #[cfg(test)]
    pub fn dummy_for_test(contents: &str) -> Self {
        Self {
            path: "test.tk".to_string(),
            root: ModuleId::new(0),
            contents: contents.to_string(),
            string_interner: crate::string_interner::get_new_interner(),
            lexed: None,
            ast: Ast::default(),
        }
    }
}

#[derive(StructOfArray, Debug)]
#[soa_attr(Vec, derive(Debug))]
#[soa_attr(Vec, derive(Default))]
pub struct Module {
    pub file: FileId,
    pub parent: Option<ModuleId>,
    pub children: Vec<ModuleId>,
    pub root: NodeId,
}
pub type ModuleId = TypedIndex<Module>;

#[derive(StructOfArray, Debug)]
#[soa_attr(Vec, derive(Debug))]
#[soa_attr(Vec, derive(Default))]
pub struct Error {
    pub file: FileId,
    pub module: ModuleId,
    pub location: Location,
    pub error: TError,
}
pub type ErrorId = TypedIndex<Error>;
