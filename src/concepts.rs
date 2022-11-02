use crate::ast::*;
use crate::error::TError;
use crate::free_standing::typed_index::TypedIndex;
use crate::location::Location;
use crate::string_interner::StrInterner;
use crate::tokens::Token;

// TODO: Replace strings where ideal...
#[derive(Debug)]
pub struct File {
    pub path: String, // TODO: Use something 'right'
    pub string_interner: StrInterner,
    pub ast: Ast,
    pub root: Option<ModuleId>,
    pub contents: Option<String>,
    pub lexed: Option<Vec<Token>>,
}
pub type FileId = TypedIndex<File>;

impl File {
    #[cfg(test)]
    pub fn dummy_for_test(contents: &str) -> Self {
        Self {
            path: "test.tk".to_string(),
            root: None,
            contents: contents.to_string(),
            string_interner: crate::string_interner::get_new_interner(),
            lexed: None,
            ast: Ast::default(),
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub file: FileId,
    pub parent: Option<ModuleId>,
    pub children: Vec<ModuleId>,
    pub root: NodeId,
}
pub type ModuleId = TypedIndex<Module>;

#[derive(Debug)]
pub struct Error {
    pub file: FileId,
    pub module: ModuleId,
    pub location: Location,
    pub error: TError,
}
pub type ErrorId = TypedIndex<Error>;
