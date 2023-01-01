use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use inkwell::context::Context;
use inkwell::OptimizationLevel;
use inkwell::types::{FloatType, IntMathType, IntType};
use inkwell::values::{AnyValue, FloatMathValue, FloatValue, IntMathValue, IntValue, PointerMathValue, PointerValue};
use crate::vm::module::function::Function;
use crate::vm::module::vm_module::Module;

pub unsafe fn compile_function(module: *mut Module, function: *mut Function, optimization_level: OptimizationLevel) -> usize {
    let module = &mut *module;
    let function = &mut *function;

    let mut context = Context::create();
    let mut llvm_module = context.create_module(module.name.as_str());

    return 0;
}


pub struct LLVMValues<'a> {
    int_values: HashMap<usize, IntValue<'a>>,
    float_values: HashMap<usize, FloatValue<'a>>,
    pointer_values: HashMap<usize, PointerValue<'a>>
}

impl<'a> LLVMValues<'a> {

    pub fn get_int_value(&mut self, index: usize) -> Result<IntValue, CompileError> {
        return match self.int_values.get(&index) {
            Some(value) => Ok(*value),
            _ => Err(CompileError::RegisterNotFoundError(index))
        }
    }


    pub fn insert_int_value(&mut self, index: usize, value: IntValue<'a>) {
        self.int_values.insert(index, value);
    }

}


#[derive(Debug)]
pub enum CompileError {
    RegisterNotFoundError(usize)
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::RegisterNotFoundError(num) => write!(f, "Register '{}' is not found.", num)
        }
    }
}