use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::mem;
use std::mem::transmute_copy;
use inkwell::context::Context;
use inkwell::{AddressSpace, OptimizationLevel};
use inkwell::builder::Builder;
use inkwell::execution_engine::{ExecutionEngine, FunctionLookupError};
use inkwell::support::LLVMString;
use inkwell::types::{BasicMetadataTypeEnum, FloatType, IntMathType, IntType, PointerType, VoidType};
use inkwell::values::{AnyValue, FloatMathValue, FloatValue, IntMathValue, IntValue, PointerMathValue, PointerValue};
use crate::vm::module::function::Function;
use crate::vm::module::object_type::Type;
use crate::vm::module::vm_module::Module;


pub struct LLVMModuleHolder<'ctx> {
    pub context: &'ctx Context,
    pub llvm_module: inkwell::module::Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
}


impl<'ctx> LLVMModuleHolder<'ctx> {

    pub unsafe fn compile_function(&self, llvm_values: &mut LLVMValues<'ctx>, module: *mut Module, function: *mut Function) -> Result<(), CompileError> {
        //Create llvm function
        let mut argument_types: Vec<BasicMetadataTypeEnum> = Vec::new();
        argument_types.push(self.context.i8_type().ptr_type(AddressSpace::default()).into());

        for argument_type in (*function).argument_type_list.iter() {
            match convert_type_to_llvm(argument_type, &self.context) {
                LLVMTypeTemp::Int(int_type) => argument_types.push(int_type.into()),
                LLVMTypeTemp::Float(float_type) => argument_types.push(float_type.into()),
                LLVMTypeTemp::Pointer(pointer_type) => argument_types.push(pointer_type.into()),
                LLVMTypeTemp::Void(_) => {}
            }
        }

        let function_type = match convert_type_to_llvm(&(*function).return_type, &self.context) {
            LLVMTypeTemp::Int(int_type) => int_type.fn_type(argument_types.as_slice(), false),
            LLVMTypeTemp::Float(float_type) => float_type.fn_type(argument_types.as_slice(), false),
            LLVMTypeTemp::Pointer(pointer_type) => pointer_type.fn_type(argument_types.as_slice(), false),
            LLVMTypeTemp::Void(void_type) => void_type.fn_type(argument_types.as_slice(), false),
        };

        let llvm_function = self.llvm_module.add_function((*function).name.as_str(), function_type, None);

        let builder_ref = &self.builder;
        for label_block in (*function).label_block_list.iter() {
            let llvm_block = self.context.append_basic_block(llvm_function, label_block.name.as_str());
            builder_ref.position_at_end(llvm_block);

            for order in label_block.order_list.iter() {
                match order.compile(&mut *module, &mut *function, self.context, builder_ref, &self.llvm_module, llvm_values) {
                    Ok(_) => {},
                    Err(err) => { return Err(err); }
                }
            }
        }

        return Ok(());
    }

}



pub enum LLVMTypeTemp<'a> {
    Int(IntType<'a>),
    Float(FloatType<'a>),
    Pointer(PointerType<'a>),
    Void(VoidType<'a>)
}

pub fn convert_type_to_llvm<'a>(type_info: &Type, context: &'a Context) -> LLVMTypeTemp<'a> {
    return match type_info {
        Type::I8 => LLVMTypeTemp::Int(context.i8_type()),
        Type::I16 => LLVMTypeTemp::Int(context.i16_type()),
        Type::I32 => LLVMTypeTemp::Int(context.i32_type()),
        Type::I64 => LLVMTypeTemp::Int(context.i64_type()),
        Type::U8 => LLVMTypeTemp::Int(context.i8_type()),
        Type::U16 => LLVMTypeTemp::Int(context.i16_type()),
        Type::U32 => LLVMTypeTemp::Int(context.i32_type()),
        Type::U64 => LLVMTypeTemp::Int(context.i64_type()),
        Type::F32 => LLVMTypeTemp::Float(context.f32_type()),
        Type::F64 => LLVMTypeTemp::Float(context.f64_type()),
        Type::ObjectReference(_, _) => LLVMTypeTemp::Pointer(context.i8_type().ptr_type(AddressSpace::default())),
        Type::Void => LLVMTypeTemp::Void(context.void_type())
    }
}



pub struct LLVMValues<'a> {
    pub int_values: HashMap<usize, IntValue<'a>>,
    pub float_values: HashMap<usize, FloatValue<'a>>,
    pub pointer_values: HashMap<usize, PointerValue<'a>>
}

impl<'a> LLVMValues<'a> {

    pub fn new() -> LLVMValues<'a> {
        return Self {
            int_values: HashMap::new(),
            float_values: HashMap::new(),
            pointer_values: HashMap::new()
        };
    }

    pub fn get_int_value(&mut self, index: usize) -> Result<IntValue<'a>, CompileError> {
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
    RegisterNotFoundError(usize),
    TypeMismatchError(String, String),
    LLVMCreateJITCompileEngineError(LLVMString),
    LLVMFunctionLookupError(FunctionLookupError)
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::RegisterNotFoundError(num) => write!(f, "Register '{}' is not found.", num),
            CompileError::TypeMismatchError(type_name1, type_name2) => write!(f, "Type mismatch. Expected '{}', but found '{}'.", type_name1, type_name2),
            CompileError::LLVMCreateJITCompileEngineError(llvm_error_message) => write!(f, "Failed to create jit compile engine. Caused by => {}", llvm_error_message.to_string()),
            CompileError::LLVMFunctionLookupError(err) => write!(f, "Not found function. => {}", err)
        }
    }
}