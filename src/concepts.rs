use crate::ast::*;
use crate::free_standing::typed_index::TypedIndex;
use crate::tokens::Token;
use crate::string_interner::{StrInterner, get_new_interner};

// TODO: Replace strings where ideal...
#[derive(Debug)]
pub struct File {
    pub path: String, // TODO: Use something 'right'
    pub string_interner: StrInterner,
    pub ast: Option<Ast>,
    pub root: Option<ModuleId>,
    pub contents: Option<String>,
    pub tokens: Option<Vec<Token>>,
}
pub type FileId = TypedIndex<File>;

impl File {
    #[cfg(test)]
    pub fn dummy_for_test(contents: &str) -> Self {
        Self {
            path: "test.tk".to_string(),
            contents: Some(contents.to_string()),
            string_interner: get_new_interner(),
            root: None,
            tokens: None,
            ast: None,
        }
    }

    pub fn from_path(path: &str) -> Self {
        File {
            path: path.to_string(),
            string_interner: get_new_interner(),
            root: None,
            contents: None,
            tokens: None,
            ast: None,
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
