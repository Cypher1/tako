use crate::concepts::*;

#[derive(Debug, Copy, Clone)]
pub enum JobTypes {
    Load(FileId),
    Lex(FileId),
    Parse(FileId),
    AllFilesParsed,
    TypeCheck(ModuleId),
    GlobalTypeCheck,
    Optimise(ModuleId),
    GlobalOptimise,
    CodeGen(ModuleId),
}
