use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::values::BasicValue;
use crate::llvm::compiler::{CompileError, LLVMValues};
use crate::vm::module::function::Function;
use crate::vm::module::order::orders::Order;
use crate::vm::module::vm_module::Module;
use crate::VMThread;

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

    fn link(&mut self, _: *mut Module, _: *mut Function) {/*None*/}

    fn compile<'a>(&self, module: &mut Module, function: &mut Function, context: &'a Context, builder: &'a Builder<'a>, llvm_module: &inkwell::module::Module<'a>, llvm_values: &mut LLVMValues<'a>) -> Result<(), CompileError> {
        return if self.is_void {
            builder.build_return(None);
            Ok(())
        } else {
            match llvm_values.int_values.get(&self.get_register_index) {
                Some(value) => builder.build_return(Some(value)),
                _ => {
                    match llvm_values.float_values.get(&self.get_register_index) {
                        Some(value) => builder.build_return(Some(value)),
                        _ => {
                            match llvm_values.pointer_values.get(&self.get_register_index) {
                                Some(value) => builder.build_return(Some(value)),
                                _ => { return Err(CompileError::RegisterNotFoundError(self.get_register_index)); }
                            }
                        }
                    }
                }
            };
            Ok(())
        }
    }
}