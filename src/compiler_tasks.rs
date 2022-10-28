use crate::concepts::*;

#[derive(Debug, Copy, Clone)]
pub enum TakoJob {
    Lex(FileId),
    Parse(FileId),
    TypeCheck(ModuleId),
    GlobalTypeCheck,
    Optimise(ModuleId),
    GlobalOptimise,
    CodeGen(EntryPointId),
}
