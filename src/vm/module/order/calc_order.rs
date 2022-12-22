use crate::vm::module::function::Function;
use crate::vm::module::object_type::Type;
use crate::vm::module::order::orders::Order;
use crate::vm::module::vm_module::Module;
use crate::VMThread;

pub struct AddIntegerOrder {
    target_index: usize,
    argument_register_left: usize,
    argument_register_right: usize,
    integer_type: Type
}

impl AddIntegerOrder {
    pub fn new(target_index: usize, argument_register_left: usize, argument_register_right: usize, integer_type: Type) -> Self {
        Self {
            target_index,
            argument_register_left,
            argument_register_right,
            integer_type
        }
    }
}

impl Order for AddIntegerOrder {
    fn eval(&self, vm_thread: *mut VMThread, module: *mut Module, registers: &mut Vec<u64>, variables: &mut Vec<u64>, arguments: &Vec<u64>) {
        let value: u64 = match &self.integer_type {
            Type::I8 => (registers[self.argument_register_left] as i8 + registers[self.argument_register_right] as i8) as u64,
            Type::I16 => (registers[self.argument_register_left] as i16 + registers[self.argument_register_right] as i16) as u64,
            Type::I32 => (registers[self.argument_register_left] as i32 + registers[self.argument_register_right] as i32) as u64,
            Type::I64 => (registers[self.argument_register_left] as i64 + registers[self.argument_register_right] as i64) as u64,
            Type::U8 => (registers[self.argument_register_left] as u8 + registers[self.argument_register_right] as u8) as u64,
            Type::U16 => (registers[self.argument_register_left] as u16 + registers[self.argument_register_right] as u16) as u64,
            Type::U32 => (registers[self.argument_register_left] as u32 + registers[self.argument_register_right] as u32) as u64,
            Type::U64 => registers[self.argument_register_left] as u64 + registers[self.argument_register_right] as u64,
            _ => 0
        };
        registers[self.target_index] = value;
    }

    fn link(&mut self, module: *mut Module, function: *mut Function) {/**/}
}