use inkwell::context::Context;
use inkwell::OptimizationLevel;
use crate::vm::module::function::Function;
use crate::vm::module::vm_module::Module;

pub unsafe fn compile_function(module: *mut Module, function: *mut Function, optimization_level: OptimizationLevel) -> usize {
    let module = &mut *module;
    let function = &mut *function;

    let context = Context::create();
    let llvm_module = context.create_module(module.name.as_str());

    return 0;
}