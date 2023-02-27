#[cfg(feature = "llvm_backend")]
mod llvm;

trait Backend {
    fn add_binary(&mut self);
    fn add_library(&mut self);
    fn add_function(&mut self);
}
