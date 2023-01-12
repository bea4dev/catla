use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use crate::llvm::compiler::{CompileError, LLVMValues};
use crate::vm::module::function::Function;
use crate::vm::module::vm_module::Module;
use crate::vm::tortie::ModuleLoadError;
use crate::{LLVMModuleHolder, VMThread};

pub trait Order {

    fn eval(&self, vm_thread: *mut VMThread, module: *mut Module, registers: &mut Vec<u64>, variables: &mut Vec<u64>, arguments: &Vec<u64>);

    fn link(&mut self, module: *mut Module, function: *mut Function) -> Result<(), ModuleLoadError>;

    fn compile<'a>(&self, module: &mut Module, function: &mut Function, llvm_module_holder: &LLVMModuleHolder<'a>,
               llvm_values: &mut LLVMValues<'a>) -> Result<(), CompileError>;

}