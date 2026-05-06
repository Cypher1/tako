use std::path::PathBuf;

use enum_kinds::EnumKind;
use qbice::{Decode, Encode, Identifiable, Query, StableHash};

use crate::{
    ast::{string_interner::Name, Ast},
    parser::tokens::Token,
    primitives::Prim,
};

#[derive(EnumKind)]
#[enum_kind(QueryKind, derive(Hash, Ord, PartialOrd))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnyQuery {
    LoadFileQuery(LoadFile),
    LexFileQuery(LexFile),
    ParseFileQuery(ParseFile),
    DesugarFileQuery(DesugarFile),
    LowerFileQuery(LowerFile),
    CodegenQuery(Codegen),
    EvalQuery(Eval),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub enum FileRef {
    File(PathBuf),
    InMemory(PathBuf, String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct LoadFile {
    file: PathBuf,
}

impl Query for LoadFile {
    type Value = String;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct LexFile {
    file: FileRef,
}

impl Query for LexFile {
    type Value = Vec<Token>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct ParseFile {
    file: FileRef,
}

impl Query for ParseFile {
    type Value = Ast;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct DesugarFile {
    file: FileRef,
}

impl Query for DesugarFile {
    type Value = Ast;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct LowerFile {
    file: FileRef,
}

impl Query for LowerFile {
    type Value = Ast;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Codegen {
    entry: FileRef,
    entry_name: Option<Name>,
}

impl Query for Codegen {
    type Value = Ast; // Should include binaries...
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Eval {
    entry: FileRef,
    entry_name: Option<Name>,
    // TODO: Add context variables.
}

impl Query for Eval {
    type Value = (Ast, Prim); // Should include binaries...
}
