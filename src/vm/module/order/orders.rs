use inkwell::builder::Builder;
use inkwell::context::Context;
use crate::llvm::compiler::{CompileError, LLVMValues};
use crate::vm::module::function::Function;
use crate::vm::module::vm_module::Module;
use crate::VMThread;

pub trait Order {

    fn eval(&self, vm_thread: *mut VMThread, module: *mut Module, registers: &mut Vec<u64>, variables: &mut Vec<u64>, arguments: &Vec<u64>);

    fn link(&mut self, module: *mut Module, function: *mut Function);

    fn compile<'a>(self, module: &mut Module, function: &mut Function, context: &'a mut Context,
               builder: &'a mut Builder<'a>, llvm_module: &'a mut inkwell::module::Module<'a>,
               llvm_values: &'a mut LLVMValues<'a>) -> Result<(), CompileError>;

}