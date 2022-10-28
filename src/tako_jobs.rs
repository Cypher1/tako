use crate::components::*;

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
