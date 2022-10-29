use crate::concepts::*;

#[derive(Debug, Copy, Clone)]
pub enum JobTypes {
    Lex(FileId),
    Parse(FileId),
    TypeCheck(ModuleId),
    GlobalTypeCheck,
    Optimise(ModuleId),
    GlobalOptimise,
    CodeGen(ModuleId),
}