use crate::error::TError;
use std::path::Path;

#[cfg(feature = "llvm_backend")]
mod llvm;

#[cfg(feature = "llvm_backend")]
pub fn backend<'ctx>(cfg: BackendConfig, context: &'ctx <llvm::Llvm<'ctx> as Backend<'ctx>>::Context) -> llvm::Llvm<'ctx> {
    llvm::Llvm::new(cfg, context)
}

#[cfg(feature = "llvm_backend")]
pub fn create_context<'ctx>() -> <llvm::Llvm<'ctx> as Backend<'ctx>>::Context {
    llvm::Llvm::create_context()
}

#[derive(Default, Debug)]
pub struct BackendConfig {}

pub trait BackendStateTrait {
    type IntType;
    type IntValue;
    type PointerType;
    type PointerValue;
    type Value;
    type ValueType;
    type ReturnValue;
    type FunctionType;
    type FunctionValue;
    type VectorType;
    type VectorValue;

    fn add_main(&mut self) -> (Self::FunctionValue, Self::Value, Self::Value);
    fn add_function(&mut self, name: &str, ty: Self::FunctionType) -> Self::FunctionValue;

    fn access_into_array(&mut self, ty: Self::ValueType, array: Self::PointerValue) -> Self::Value;

    fn array_of_strings_type(&mut self) -> Self::PointerType;
    fn string_type(&mut self) -> Self::PointerType {
        self.char_ptr_type()
    }
    fn char_ptr_type(&mut self) -> Self::PointerType;
    fn global_string(&mut self, value: &str) -> Self::PointerValue;
    fn printf(&mut self, fmt: &str, args: &[Self::Value]);
    fn build_return(&mut self, value: Self::ReturnValue);

    fn const_int<T: Into<u64>>(&mut self, ty: Self::IntType, value: T) -> Self::Value;

    fn i8_type(&mut self) -> Self::IntType;
    fn i16_type(&mut self) -> Self::IntType;
    fn i32_type(&mut self) -> Self::IntType;
    fn i64_type(&mut self) -> Self::IntType;

    fn create_binary(&self, bin_path: &Path) -> Result<(), TError>;
}

pub trait Backend<'ctx> {
    type Context;
    type BackendState: BackendStateTrait;

    fn create_context() -> Self::Context;
    fn new(_config: BackendConfig, context: &'ctx Self::Context) -> Self;
    fn add_module(&'ctx mut self, name: &str) -> Result<Self::BackendState, Box<dyn std::error::Error>>;
}
