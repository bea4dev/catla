use inkwell::builder::Builder;
use inkwell::context::Context;
use crate::llvm::compiler::{CompileError, LLVMValues};
use crate::vm::module::function::Function;
use crate::vm::module::order::orders::Order;
use crate::vm::module::vm_module::Module;
use crate::VMThread;

pub struct ReturnOrder {
    get_register_index: usize
}

impl ReturnOrder {
    pub fn new(get_register_index: usize) -> Self {
        Self {
            get_register_index
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

    fn compile<'a>(self, module: &mut Module, function: &mut Function, context: &'a mut Context, builder: &'a mut Builder<'a>, llvm_module: &'a mut inkwell::module::Module<'a>, llvm_values: &'a mut LLVMValues<'a>) -> Result<(), CompileError> {
        return Ok(());
    }
}