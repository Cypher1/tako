pub(crate) mod executors;

use enum_kinds::EnumKind;
use qbice::{Decode, Encode, Identifiable, Query, StableHash};
use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::path::PathBuf;

#[cfg(feature = "codegen")]
use crate::codegen::{BinaryDescription, BinaryInfo};
use crate::{
    ast::{
        location::{Location, UserFacingLocation},
        string_interner::Name,
        Ast, NodeId,
    },
    error::{Error, TError},
    parser::tokens::Token,
    primitives::Prim,
};

#[derive(EnumKind)]
#[enum_kind(QueryKind, derive(Hash, Ord, PartialOrd))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnyQuery {
    // Loading
    LoadQuery(Load), // name, version? -> string
    // Parsing
    LexQuery(Lex),                           // name -> token[]
    ParseFrontMatterQuery(ParseFrontMatter), // name -> [partial] ast, token[]
    ParseQuery(Parse),                       // file/name -> [partial]ast, [src]node
    HandleImportQuery(HandleImport),         // name[] -> [partial]ast
    ResolveAstQuery(ResolveAst),             // file -> [partial, resolved]ast
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
    ErrorsQuery(Errors),     // name -> (src_pos, error)[]
    ErrorsAtQuery(ErrorsAt), // src_pos -> error[]
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
    EnumerateBinariesQuery(EnumerateBinaries), // name -> binary_info[]
    #[cfg(feature = "codegen")]
    WriteCodeGenQuery(WriteCodeGen), // name -> IO
    #[cfg(feature = "codegen")]
    CodeGenQuery(CodeGen), // name -> binary_info
    #[cfg(feature = "codegen")]
    SourceMapGenQuery(SourceMapGen), // src & binary -> IO
    #[cfg(feature = "codegen")]
    SourceMapGenAllQuery(SourceMapGenAll), // src -> IO
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, StableHash, Identifiable, Encode, Decode,
)]
pub enum FileRef {
    File(PathBuf),
    InMemory(PathBuf, String),
    Dependency {
        name: String,
        version: String,
        internal_path: PathBuf,
    },
}

impl Default for FileRef {
    fn default() -> Self {
        Self::InMemory(PathBuf::new(), String::new())
    }
}

impl std::fmt::Display for FileRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::File(pathbuf) => write!(f, "{}", pathbuf.display()),
            Self::InMemory(pathbuf, _) => write!(f, "mem://{}", pathbuf.display()),
            Self::Dependency {
                name,
                version,
                internal_path,
            } => write!(f, "{name}@{version}: {}", internal_path.display()),
        }
    }
}

impl FileRef {
    pub fn to_path_buf(&self) -> (Option<PathBuf>, PathBuf) {
        match self {
            Self::File(pathbuf) => (None, pathbuf.to_owned()),
            Self::InMemory(pathbuf, _) => (None, pathbuf.to_owned()),
            Self::Dependency {
                name,
                version,
                internal_path,
            } => {
                // TODO: Make into a query kind of thing?
                let project_dirs = directories::ProjectDirs::from("dev", "takolang", "tako")
                    .expect("Should produce a valid path");
                // let config_dir = project_dirs
                //      .config_dir(); // "Roaming" / synced config dir
                let cache_dir = project_dirs.cache_dir();
                // TODO: Get file from bzip2.
                // TODO: Return as a enum of PathBuf or BzipPath + Subpath
                (
                    Some(
                        cache_dir
                            .join(PathBuf::from("packages"))
                            .join(PathBuf::from(name))
                            .join(PathBuf::from(version)),
                    ),
                    internal_path.to_path_buf(),
                )
            }
        }
    }
}

#[derive(Default, Debug, Clone, Copy)]
pub struct TaskStats {
    num_requests: u32,
    total_num_results: u32,
    num_already_running: u32,
    num_cached: u32,
    num_failed: u32,
    num_succeeded: u32,
}

type TaskHash = u64;
pub type TaskId = TaskHash;

#[derive(Debug, Clone)]
pub struct StatusReport {
    pub kind: QueryKind,
    pub stats: TaskStats,
    pub errors: HashMap<TaskId, BTreeMap<UserFacingLocation, Vec<Error>>>,
}

impl StatusReport {
    #[must_use]
    pub fn new(kind: QueryKind) -> Self {
        Self {
            kind,
            stats: TaskStats::default(),
            errors: HashMap::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct ErrorsAt {
pub file: FileRef,
pub location: UserFacingLocation,
}

impl Query for ErrorsAt {
type Value = Vec<Error>;
}



#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Desugar {
    pub entry: FileRef,
}

impl Query for Desugar {
    type Value = Result<Ast, TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Lower {
    pub entry: FileRef,
}

impl Query for Lower {
    type Value = Result<Ast, TError>;
}

#[cfg(feature = "codegen")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct CodeGenAll {
    pub entry: FileRef,
}

#[cfg(feature = "codegen")]
impl Query for CodeGenAll {
    type Value = Result<BTreeMap<Name, BinaryInfo>, TError>;
}

#[cfg(feature = "codegen")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct EnumerateBinaries {
    pub entry: FileRef,
}

#[cfg(feature = "codegen")]
impl Query for EnumerateBinaries {
    type Value = Result<BTreeMap<Name, BinaryDescription>, TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct WriteCodeGenAll {
    pub entry: FileRef,
}

impl Query for WriteCodeGenAll {
    type Value = Vec<TError>;
}

#[cfg(feature = "codegen")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct CodeGen {
    pub entry: FileRef,
    pub entry_name: Option<Name>,
}

#[cfg(feature = "codegen")]
impl Query for CodeGen {
    type Value = Result<BinaryInfo, TError>;
}

#[cfg(feature = "codegen")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct WriteCodeGen {
    pub entry: FileRef,
    pub entry_name: Name,
    pub target: BinaryDescription,
}

#[cfg(feature = "codegen")]
impl Query for WriteCodeGen {
    type Value = Result<(), TError>;
}

#[cfg(feature = "codegen")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct SourceMapGen {
    pub entry: FileRef,
    pub entry_name: Name,
}

#[cfg(feature = "codegen")]
impl Query for SourceMapGen {
    type Value = Result<(), TError>;
}

#[cfg(feature = "codegen")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct SourceMapGenAll {
    pub entry: FileRef,
}

#[cfg(feature = "codegen")]
impl Query for SourceMapGenAll {
    type Value = Result<(), TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Eval {
    pub entry: FileRef,
    pub entry_name: Option<Name>,
    // TODO: Add context variables.
}

impl Query for Eval {
    // A set of context values would be useful here...
    type Value = Result<Prim, TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Optimize {
    pub entry: FileRef,
    pub root: NodeId,
}

impl Query for Optimize {
    type Value = Result<(Ast, NodeId), TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct TypeAt {
    pub entry: FileRef,
    pub location: Location,
}

impl Query for TypeAt {
    type Value = Result<(Ast, NodeId), TError>; // NodeId should point to the TypeInfo to pretty print.
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct TypeCheck {
    pub entry: FileRef,
    // pub ast: Ast,
}

impl Query for TypeCheck {
    type Value = Result<(Ast, NodeId), TError>; // With the type info added.
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct CheckProofs {
    pub entry: FileRef,
}

impl Query for CheckProofs {
    type Value = Result<(Ast, NodeId, Vec<TError>), TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct GetType {
    pub entry: FileRef,
    pub node: NodeId,
}

impl Query for GetType {
    type Value = Result<(Ast, NodeId), TError>; // With the type info added.
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct FindNode {
    pub entry: FileRef,
    pub location: Location,
}

impl Query for FindNode {
    type Value = Result<(Ast, NodeId), TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct GetLocation {
    pub entry: FileRef,
    pub node: NodeId,
}

impl Query for GetLocation {
    type Value = Result<(FileRef, Location), TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Parse {
    pub entry: FileRef,
}

impl Query for Parse {
    type Value = Result<Ast, TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct FindDefinition {
    pub entry: FileRef,
    pub name: Name,
}

impl Query for FindDefinition {
    type Value = Result<(Ast, NodeId), TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct ParseFrontMatter {
    pub entry: FileRef,
}

impl Query for ParseFrontMatter {
    type Value = Result<Ast, TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct MacroExpand {
    pub entry: FileRef,
}

impl Query for MacroExpand {
    type Value = Result<Ast, TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct HandleImport {
    pub entry: FileRef,
    pub import: FileRef,
}

impl Query for HandleImport {
    type Value = Result<Ast, TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct ResolveAst {
    pub entry: FileRef,
}

impl Query for ResolveAst {
    type Value = Result<Ast, TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Lex {
    pub entry: FileRef,
}

impl Query for Lex {
    type Value = Result<Vec<Token>, TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Load {
    pub file: FileRef,
}

impl Query for Load {
    type Value = Result<String, TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct PrettyPrint {
    pub ast: Ast,
    pub root: NodeId,
}

impl Query for PrettyPrint {
    type Value = String;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Interpret {
    pub entry: FileRef,
    pub start: Option<Name>,
}

impl Query for Interpret {
    type Value = Result<Prim, TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct EvalNode {
    pub ast: Ast,
    pub entry: FileRef,
    pub start: NodeId,
}

impl Query for EvalNode {
    type Value = Result<Prim, TError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct Errors {
    pub file: FileRef,
}

impl Query for Errors {
    type Value = BTreeMap<UserFacingLocation, Vec<Error>>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct ErrorsAt {
    pub file: FileRef,
    pub location: UserFacingLocation,
}

impl Query for ErrorsAt {
    type Value = Vec<Error>;
}
