use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::values::{FunctionValue, IntValue};
use crate::llvm::compiler::{CompileError, InstantNameProvider, LLVMValues};
use crate::vm::module::function::Function;
use crate::vm::module::object_type::Type;
use crate::vm::module::order::orders::Order;
use crate::vm::module::vm_module::Module;
use crate::vm::tortie::ModuleLoadError;
use crate::{LLVMModuleHolder, VMThread};

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
    fn eval(&self, _: *mut VMThread, _: *mut Module, registers: &mut Vec<u64>, _: &mut Vec<u64>, _: &Vec<u64>) {
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

    fn link(&mut self, _: *mut Module, _: *mut Function) -> Result<(), ModuleLoadError> {
        return Ok(());
    }

    fn compile<'a>(&self, module: &mut Module, function: &mut Function, llvm_module_holder: &LLVMModuleHolder<'a>, llvm_function: &FunctionValue<'a>, name_provider: &mut InstantNameProvider, llvm_values: &mut LLVMValues<'a>) -> Result<(), CompileError> {
        let left: IntValue = match llvm_values.get_value(self.argument_register_left) {
            Ok(value) => match value.try_into() {
                Ok(value) => value,
                Err(_) => { return Err(CompileError::TypeMismatchError("integer".to_string(), "other".to_string())); }
            },
            Err(err) => { return Err(err); }
        };
        let right: IntValue = match llvm_values.get_value(self.argument_register_right) {
            Ok(value) => match value.try_into() {
                Ok(value) => value,
                Err(_) => { return Err(CompileError::TypeMismatchError("integer".to_string(), "other".to_string())); }
            },
            Err(err) => { return Err(err); }
        };

        let value = llvm_module_holder.builder.build_int_add(left, right, format!("reg{}", self.target_index).as_str());
        llvm_values.insert_value(self.target_index, value.into());

        return Ok(());
    }
}