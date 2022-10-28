type FileId = ();
type ChunkId = ();
type ModuleId = ();
type EntryPointId = ();

#[derive(Debug)]
pub enum TakoJob {
    Lex(FileId),
    Parse(FileId, ChunkId),
    TypeCheck(ModuleId),
    GlobalTypeCheck,
    Optimise(ModuleId),
    GlobalOptimise,
    CodeGen(EntryPointId),
}
