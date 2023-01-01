use inkwell::builder::Builder;
use inkwell::context::Context;
use crate::llvm::compiler::{CompileError, LLVMValues};
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

    fn compile<'a>(&self, module: &mut Module, function: &mut Function, context: &'a Context, builder: &'a Builder<'a>, llvm_module: &inkwell::module::Module<'a>, llvm_values: &mut LLVMValues<'a>) -> Result<(), CompileError> {
        return Ok(());
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

    fn compile<'a>(&self, module: &mut Module, function: &mut Function, context: &'a Context, builder: &'a Builder<'a>, llvm_module: &inkwell::module::Module<'a>, llvm_values: &mut LLVMValues<'a>) -> Result<(), CompileError> {
        let value = match &self.value_type {
            Type::I8 => context.i8_type().const_int(self.const_value_bits, true),
            Type::I16 => context.i16_type().const_int(self.const_value_bits, true),
            Type::I32 => context.i32_type().const_int(self.const_value_bits, true),
            Type::I64 => context.i64_type().const_int(self.const_value_bits, true),
            Type::U8 => context.i8_type().const_int(self.const_value_bits, false),
            Type::U16 => context.i16_type().const_int(self.const_value_bits, false),
            Type::U32 => context.i32_type().const_int(self.const_value_bits, false),
            Type::U64 => context.i64_type().const_int(self.const_value_bits, false),
            _ => { return Err(CompileError::TypeMismatchError("integer".to_string(), "other".to_string())); }
        };

        llvm_values.insert_int_value(self.target_index, value);

        return Ok(());
    }
}