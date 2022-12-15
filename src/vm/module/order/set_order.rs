use crate::vm::module::function::Function;
use crate::vm::module::order::orders::Order;
use crate::vm::module::vm_module::Module;
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
    fn eval(&self, vm_thread: *mut VMThread, module: *mut Module, registers: &mut Vec<u64>, variables: &mut Vec<u64>, arguments: &mut Vec<u64>) {
        arguments[self.target_variable_index] = registers[self.get_register_index];
    }

    fn link(&mut self, module: *mut Module, function: *mut Function) {/*None*/}
}