use enum_kinds::EnumKind;
use qbice::{Decode, Encode, Identifiable, Query, StableHash};
use std::collections::BTreeMap;
use std::path::PathBuf;

#[cfg(feature = "codegen")]
use crate::codegen::{BinaryDescription, BinaryInfo};
use crate::{
    ast::{Ast, NodeId, location::Location, string_interner::Name},
    error::{Error, TError},
    parser::tokens::Token,
    primitives::Prim,
};

#[derive(EnumKind)]
#[enum_kind(QueryKind, derive(Hash, Ord, PartialOrd))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnyQuery {
    // Loading
    LoadQuery(Load),                                 // name, version? -> string
    LoadLocalFileQuery(LoadLocalFile),               // path -> string with IO
    DownloadDependenciesQuery(DownloadDependencies), // name, version -> string with IO
    // Parsing
    LexQuery(Lex),                           // name -> token[]
    ParseFrontMatterQuery(ParseFrontMatter), // name -> [partial] ast, token[]
    EvalFrontMatterQuery(EvalFrontMatter),   // name -> operator[], macro[], [partial] ast, token[]
    ParseQuery(Parse),                       // file/name -> [partial]ast, [src]node
    HandleImportQuery(HandleImport),         // name -> [partial]ast
    MacroExpandQuery(MacroExpand),           // name -> [partial]ast, [gen]node
    FindNodeQuery(FindNode),                 // src_pos -> [src]node
    FindDefinitionQuery(FindDefinition),     // name -> [src]node
    GetLocationQuery(GetLocation),           // [src]node -> src_pos
    // Semantic Analysis
    TypeAtQuery(TypeAt),           // src_pos -> TypeInfo
    TypeCheckQuery(TypeCheck),     // name -> [typed]ast
    GetTypeQuery(GetType),         // [src]node -> TypeInfo
    CheckProofsQuery(CheckProofs), // [gen]node -> [typed]ast, errors[]
    // Error Reporting
    ErrorsQuery(Errors),               // name -> (src_pos, error)[]
    ErrorsAtQuery(ErrorsAt),           // src_pos -> error[]
    ErrorsForNodeQuery(ErrorsForNode), // [src]node -> error[]
    // DevTools
    PrettyPrintQuery(PrettyPrint), // ast, node -> string
    InterpretQuery(Interpret),     // name -> IO
    EvalQuery(Eval),               // string -> value with IO
    EvalNodeQuery(EvalNode),       // [src]node -> value with IO
    // CodeGen
    OptimizeQuery(Optimize), // [src]node ->  [optimized,lowered,typed]ast, [gen]node
    LowerQuery(Lower),       // [src]node -> [lowered,typed]ast, [gen]node
    #[cfg(feature = "codegen")]
    CodeGenAllQuery(CodeGenAll), // name -> IO
    #[cfg(feature = "codegen")]
    WriteCodeGenAllQuery(WriteCodeGenAll), // name -> (name, binary_info)[]
    #[cfg(feature = "codegen")]
    EnnumerateBinariesQuery(EnnumerateBinaries), // name -> binary_info[]
    #[cfg(feature = "codegen")]
    WriteCodeGenQuery(WriteCodeGen), // name -> IO
    #[cfg(feature = "codegen")]
    CodeGenQuery(CodeGen), // name -> binary_info
    #[cfg(feature = "codegen")]
    SourceMapGenQuery(SourceMapGen), // src -> IO
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub enum FileRef {
    File(PathBuf),
    Dependency {
        name: String,
        version: String,
        internal_path: PathBuf,
    },
    InMemory(PathBuf, String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Desugar {
    file: FileRef,
}

impl Query for Desugar {
    type Value = Ast;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Lower {
    file: FileRef,
}

impl Query for Lower {
    type Value = Ast;
}

#[cfg(feature = "codegen")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct CodeGenAll {
    root: FileRef,
}

#[cfg(feature = "codegen")]
impl Query for CodeGenAll {
    type Value = BTreeMap<Name, BinaryInfo>;
}

#[cfg(feature = "codegen")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct EnnumerateBinaries {
    root: FileRef,
}

#[cfg(feature = "codegen")]
impl Query for EnnumerateBinaries {
    type Value = BTreeMap<Name, BinaryDescription>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct WriteCodeGenAll {
    root: FileRef,
}

impl Query for WriteCodeGenAll {
    type Value = ();
}

#[cfg(feature = "codegen")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct CodeGen {
    entry: FileRef,
    entry_name: Option<Name>,
}

#[cfg(feature = "codegen")]
impl Query for CodeGen {
    type Value = BinaryInfo;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct WriteCodeGen {
    entry: FileRef,
    entry_name: Option<Name>,
}

impl Query for WriteCodeGen {
    type Value = ();
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Eval {
    entry: FileRef,
    entry_name: Option<Name>,
    // TODO: Add context variables.
}

impl Query for Eval {
    // A set of context values would be useful here...
    type Value = Prim;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Optimize {
    root: NodeId,
    ast: Ast,
}

impl Query for Optimize {
    type Value = (Ast, NodeId);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct TypeAt {
    entry: FileRef,
    location: Location,
}

impl Query for TypeAt {
    type Value = (Ast, NodeId); // NodeId should point to the TypeInfo to pretty print.
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct TypeCheck {
    entry: FileRef,
    ast: Ast,
}

impl Query for TypeCheck {
    type Value = (Ast, NodeId); // With the type info added.
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct CheckProofs {
    entry: FileRef,
    ast: Ast,
}

impl Query for CheckProofs {
    type Value = (Ast, NodeId, Vec<TError>);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct GetType {
    node: NodeId,
    ast: Ast,
}

impl Query for GetType {
    type Value = (Ast, NodeId); // With the type info added.
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct FindNode {
    entry: FileRef,
    location: Location,
}

impl Query for FindNode {
    type Value = (Ast, NodeId);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct GetLocation {
    context: Ast,
    node: NodeId,
}

impl Query for GetLocation {
    type Value = (FileRef, Location);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Parse {
    entry: FileRef,
}

impl Query for Parse {
    type Value = Ast;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct FindDefinition {
    entry: FileRef,
    name: Name,
}

impl Query for FindDefinition {
    type Value = (Ast, NodeId);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct EvalFrontMatter {
    entry: FileRef,
}

impl Query for EvalFrontMatter {
    type Value = (Ast, usize); // Number of tokens to skip
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct ParseFrontMatter {
    entry: FileRef,
}

impl Query for ParseFrontMatter {
    type Value = Ast;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct MacroExpand {
    entry: FileRef,
}

impl Query for MacroExpand {
    type Value = Ast;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct HandleImport {
    entry: FileRef,
}

impl Query for HandleImport {
    type Value = Ast;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Lex {
    entry: FileRef,
}

impl Query for Lex {
    type Value = Vec<Token>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Load {
    file: FileRef,
}

impl Query for Load {
    type Value = String;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct LoadLocalFile {
    file: PathBuf,
}

impl Query for LoadLocalFile {
    type Value = String;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct DownloadDependencies {
    file: PathBuf,
    version: String,
}

impl Query for DownloadDependencies {
    type Value = String;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct PrettyPrint {
    ast: Ast,
    entry: NodeId,
}

impl Query for PrettyPrint {
    type Value = String;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Interpret {
    file: FileRef,
    entry: Option<Name>,
}

impl Query for Interpret {
    type Value = ();
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct EvalNode {
    ast: Ast,
    entry: NodeId,
}

impl Query for EvalNode {
    type Value = Prim;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Errors {
    file: FileRef,
}

impl Query for Errors {
    type Value = BTreeMap<Location, Vec<Error>>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct ErrorsAt {
    file: FileRef,
    location: Location,
}

impl Query for ErrorsAt {
    type Value = Vec<Error>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct ErrorsForNode {
    ast: Ast,
    node: NodeId,
}

impl Query for ErrorsForNode {
    type Value = Vec<TError>;
}
