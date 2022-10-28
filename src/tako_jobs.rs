use crate::location::Loc;
use crate::typed_index::TypedIndex;

#[derive(Debug)]
pub enum TakoJob {
    Lex(FileId),
    Parse(FileId),
    TypeCheck(ModuleId),
    GlobalTypeCheck,
    Optimise(ModuleId),
    GlobalOptimise,
    CodeGen(EntryPointId),
}

// TODO: Replace strings where ideal...
// TODO: Use macro for defining and registering each of these.

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
