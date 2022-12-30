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
    fn eval(&self, _: *mut VMThread, _: *mut Module, registers: &mut Vec<u64>, _: &mut Vec<u64>, arguments: &Vec<u64>) {
        registers[self.target_index] = arguments[self.argument_index];
    }

    fn link(&mut self, _: *mut Module, _: *mut Function) {
        //None
    }
}



pub struct GetConstValueOrder {
    target_index: usize,
    value_type: Type,
    const_value_bits: u64
}

impl GetConstValueOrder {
    pub fn new(target_index: usize, value_type: Type, const_value_bits: u64) -> Self {
        Self {
            target_index,
            value_type,
            const_value_bits
        }
    }
}

impl Order for GetConstValueOrder {
    fn eval(&self, _: *mut VMThread, _: *mut Module, registers: &mut Vec<u64>, _: &mut Vec<u64>, _: &Vec<u64>) {
        registers[self.target_index] = self.const_value_bits;
    }

    fn link(&mut self, _: *mut Module, _: *mut Function) {/*None*/}
}