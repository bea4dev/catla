use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::values::{BasicValue, FunctionValue};
use crate::llvm::compiler::{CompileError, InstantNameProvider, LLVMValues};
use crate::vm::module::function::Function;
use crate::vm::module::order::orders::Order;
use crate::vm::module::vm_module::Module;
use crate::vm::tortie::ModuleLoadError;
use crate::{LLVMModuleHolder, VMThread};

pub struct ReturnOrder {
    get_register_index: usize,
    is_void: bool
}

impl ReturnOrder {
    pub fn new(get_register_index: usize, is_void: bool) -> Self {
        Self {
            get_register_index,
            is_void
        }
    }
}

impl Order for ReturnOrder {
    fn eval(&self, vm_thread: *mut VMThread, _: *mut Module, registers: &mut Vec<u64>, _: &mut Vec<u64>, _: &Vec<u64>) {
        unsafe {
            let function = (*vm_thread).current_function;
            registers[(*function).register_length] = registers[self.get_register_index];
            (*vm_thread).is_return_function = true;
        }
    }

    fn link(&mut self, _: *mut Module, _: *mut Function) -> Result<(), ModuleLoadError> {
        return Ok(());
    }

    fn compile<'a>(&self, module: &mut Module, function: &mut Function, llvm_module_holder: &LLVMModuleHolder<'a>, llvm_function: &FunctionValue<'a>, name_provider: &mut InstantNameProvider, llvm_values: &mut LLVMValues<'a>) -> Result<(), CompileError> {
        let builder = &llvm_module_holder.builder;

        return if self.is_void {
            builder.build_return(None);
            Ok(())
        } else {
            match llvm_values.get_value(self.get_register_index) {
                Ok(value) => builder.build_return(Some(&value)),
                Err(err) => { return Err(err); }
            };
            Ok(())
        }
    }
}