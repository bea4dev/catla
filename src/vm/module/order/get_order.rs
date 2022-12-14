use crate::vm::module::const_value::ConstValue;
use crate::vm::module::function::Function;
use crate::vm::module::object_type::Type;
use crate::vm::module::order::orders::Order;
use crate::vm::module::vm_module::Module;
use crate::VMThread;

pub struct GetArgumentOrder {
    target_index: usize,
    argument_index: usize
}

impl GetArgumentOrder {
    pub fn new(target_index: usize, argument_index: usize) -> Self {
        Self {
            target_index,
            argument_index
        }
    }
}

impl Order for GetArgumentOrder {
    fn eval(&self, vm_thread: *mut VMThread, module: *mut Module, registers: &mut Vec<u64>, variables: &mut Vec<u64>, arguments: &mut Vec<u64>) {
        registers[self.target_index] = arguments[self.argument_index];
    }

    fn link(&mut self, module: *mut Module, function: *mut Function) {
        //None
    }
}



pub struct GetConstValueOrder {
    target_index: usize,
    const_value_index: usize,
    get_type: Type
}

impl GetConstValueOrder {
    pub fn new(target_index: usize, const_value_index: usize, get_type: Type) -> Self {
        Self {
            target_index,
            const_value_index,
            get_type
        }
    }
}

impl Order for GetConstValueOrder {
    fn eval(&self, vm_thread: *mut VMThread, module: *mut Module, registers: &mut Vec<u64>, variables: &mut Vec<u64>, arguments: &mut Vec<u64>) {
        let const_value_ref = unsafe { &(*module).const_value_list[self.const_value_index] };

    }

    fn link(&mut self, module: *mut Module, function: *mut Function) {
        todo!()
    }
}