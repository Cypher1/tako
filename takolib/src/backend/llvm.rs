use std::collections::HashMap;
use super::{Backend, BackendConfig, BackendStateTrait};
use inkwell::{targets::{
    CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
}, OptimizationLevel, values::{PointerValue, FunctionValue, BasicMetadataValueEnum, IntValue, VectorValue}, module::{Module, Linkage}, context::Context, builder::Builder, execution_engine::ExecutionEngine, AddressSpace, types::{PointerType, FunctionType, IntType, BasicTypeEnum, VectorType}};

#[derive(Debug)]
struct Llvm<'ctx> {
    reloc: RelocMode,
    model: CodeModel,
    opt: OptimizationLevel,
    target_triple: TargetTriple,
    target_machine: Option<TargetMachine>,
    context: &'ctx Context,
}

impl<'ctx> Llvm<'ctx> {}

impl<'ctx> Backend<'ctx> for Llvm<'ctx> {
    type Context = Context;
    type BackendState = LlvmState<'ctx>;

    fn create_context() -> Self::Context {
        Context::create()
    }

    fn new(_config: BackendConfig, context: &'ctx Self::Context) -> Self {
        Llvm {
            reloc: RelocMode::Default,
            model: CodeModel::Default,
            opt: OptimizationLevel::Default,
            target_triple: TargetMachine::get_default_triple(),
            target_machine: None,
            context,
        }
    }
    fn setup(&mut self) {
        let target = Target::from_triple(&self.target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &self.target_triple,
                "generic",
                "",
                self.opt,
                self.reloc,
                self.model,
            )
            .unwrap();
        self.target_machine = Some(target_machine);
        Target::initialize_all(&InitializationConfig::default());
    }
    fn add_module(&mut self, name: &str) -> Result<Self::BackendState, Box<dyn std::error::Error>> {
        let context = self.context;
        let module = context.create_module(name);
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
        let builder = context.create_builder();
        let state = LlvmState {
            context,
            module,
            builder,
            execution_engine,
            functions: HashMap::new(),
            strings: HashMap::new(),
        };
        Ok(state)
    }
}

#[derive(Debug)]
struct LlvmState<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    functions: HashMap<String, PointerValue<'ctx>>,
    strings: HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> LlvmState<'ctx> {
    fn get_builtin(&mut self, name: &str, ty: FunctionType<'ctx>) -> FunctionValue<'ctx> {
        self.module.get_function(name).unwrap_or_else(|| {
            self.module.add_function(name, ty, Some(Linkage::External))
        })
    }

    fn get_printf(&mut self) -> FunctionValue<'ctx> {
        let printf_type = self.context.i32_type().fn_type(&[self.char_ptr_type().into()], true);
        self.get_builtin("printf", printf_type)
    }
}

impl<'ctx> BackendStateTrait<'ctx> for LlvmState<'ctx> {
    type IntType = IntType<'ctx>;
    type IntValue = IntValue<'ctx>;
    type PointerType = PointerType<'ctx>;
    type PointerValue = PointerValue<'ctx>;
    type Value = BasicMetadataValueEnum<'ctx>;
    type ValueType = BasicTypeEnum<'ctx>;
    type FunctionType = FunctionType<'ctx>;
    type FunctionValue = FunctionValue<'ctx>;
    type VectorType = VectorType<'ctx>;
    type VectorValue = VectorValue<'ctx>;

    fn add_function(&mut self, name: &str, ty: FunctionType<'ctx>) -> Self::FunctionValue {
        let fun = self.module.add_function(name, ty, None);

        let basic_block = self.context.append_basic_block(fun, "entry");
        self.builder.position_at_end(basic_block);

        fun
    }

    fn add_main(&mut self) -> (Self::FunctionValue, Self::Value, Self::Value) {
        let main_fn_type = self.i8_type().fn_type(&[self.i64_type().into(), self.array_of_strings_type().into()], false);
        let main = self.add_function("main", main_fn_type);
        let argc = main.get_nth_param(0).expect("Expect to be able to use argc in 'main'").into_int_value();
        let argv = main.get_nth_param(1).expect("Expect to be able to use argv in 'main'");

        (main, argc.into(), argv.into())
    }

    fn access_into_array(&mut self, ty: Self::ValueType, array: Self::PointerValue) -> Self::Value {
        self.builder.build_load(ty, array, "load_array").into()
    }

    fn array_of_strings_type(&mut self) -> Self::PointerType {
        let char_star_type = self.char_ptr_type();
        char_star_type.ptr_type(AddressSpace::default())
    }
    fn const_int<T: Into<u64>>(&mut self, ty: Self::IntType, value: T) -> Self::Value {
        // TODO: Check bounds.
        ty.const_int(value.into(), false).into()
    }

    fn i8_type(&mut self) -> Self::IntType {
        self.context.i8_type()
    }
    fn i16_type(&mut self) -> Self::IntType {
        self.context.i16_type()
    }
    fn i32_type(&mut self) -> Self::IntType {
        self.context.i32_type()
    }
    fn i64_type(&mut self) -> Self::IntType {
        self.context.i64_type()
    }

    fn char_ptr_type(&mut self) -> Self::PointerType {
        self.context.i8_type().ptr_type(AddressSpace::default())
    }

    fn global_string(&mut self, value: &str) -> Self::PointerValue {
        self.strings.get(value).copied().unwrap_or_else(|| {
            let ptr_value = self.builder.build_global_string_ptr(value, "global_string");
            let ptr = ptr_value.as_pointer_value();
            self.strings.insert(value.to_string(), ptr);
            ptr
        })
    }

    fn printf(&mut self, fmt: &str, args: &[Self::Value]) {
        let printf = self.get_printf();
        let fmt_str = self.global_string(fmt);
        let mut arg_array: Vec<BasicMetadataValueEnum<'ctx>> = vec![fmt_str.into()];
        arg_array.extend_from_slice(args);
        self.builder.build_call(printf, &arg_array[..], "_call_printf");
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn llvm_is_included() {
        print!("LLVM feature on");
    }

    #[test]
    fn can_print_hello_world() {
        let config = BackendConfig {

        };
        let ref context = Llvm::create_context();
        let mut llvm_backend = Llvm::new(config, context);
        let mut llvm = llvm_backend.add_module("hello_world").expect("Could construct module");

        let (_main, argc, argv) = llvm.add_main();
        let char_star_type = llvm.string_type();
        //let i32_type = llvm.i32_type();
        // let zero = llvm.const_int(i32_type, 0);
        let argv_0 = llvm.access_into_array(char_star_type.into(), argv.into_pointer_value());
        llvm.printf("ARGC: %d, ARGV: %s\n", &[argc.into(), argv_0.into()]);
        llvm.builder.build_return(Some(&argc.into_int_value()));

        dbg!(llvm);
    }
}
