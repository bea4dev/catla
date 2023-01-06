use inkwell::builder::Builder;
use inkwell::context::Context;
use crate::llvm::compiler::{CompileError, LLVMValues};
use crate::vm::module::function::Function;
use crate::vm::module::order::orders::Order;
use crate::vm::module::vm_module::Module;
use crate::vm::tortie::ModuleLoadError;
use crate::VMThread;

pub struct SetVariableOrder {
    target_variable_index: usize,
    get_register_index: usize
}

impl SetVariableOrder {
    pub fn new(target_argument_index: usize, get_register_index: usize) -> Self {
        Self {
            target_variable_index: target_argument_index,
            get_register_index
        }
    }
}

impl Order for SetVariableOrder {
    fn eval(&self, _: *mut VMThread, _: *mut Module, registers: &mut Vec<u64>, variables: &mut Vec<u64>, _: &Vec<u64>) {
        variables[self.target_variable_index] = registers[self.get_register_index];
    }

    fn link(&mut self, _: *mut Module, _: *mut Function) -> Result<(), ModuleLoadError> {
        return Ok(());
    }

    fn compile<'a>(&self, module: &mut Module, function: &mut Function, context: &'a Context, builder: &Builder<'a>, llvm_module: &inkwell::module::Module<'a>, llvm_values: &mut LLVMValues<'a>) -> Result<(), CompileError> {
        return Ok(());
    }
}