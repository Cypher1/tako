use crate::concepts::*;

#[derive(Debug, Copy, Clone)]
pub enum JobTypes {
    Load(String, FileId),
    Lex(FileId),
    Parse(FileId),
    TypeCheck(ModuleId),
    GlobalTypeCheck,
    Optimise(ModuleId),
    GlobalOptimise,
    CodeGen(ModuleId),
}
