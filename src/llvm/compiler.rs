use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::mem;
use std::mem::transmute_copy;
use inkwell::context::Context;
use inkwell::{AddressSpace, OptimizationLevel};
use inkwell::execution_engine::FunctionLookupError;
use inkwell::support::LLVMString;
use inkwell::types::{BasicMetadataTypeEnum, FloatType, IntMathType, IntType, PointerType, VoidType};
use inkwell::values::{AnyValue, FloatMathValue, FloatValue, IntMathValue, IntValue, PointerMathValue, PointerValue};
use crate::vm::module::function::Function;
use crate::vm::module::object_type::Type;
use crate::vm::module::vm_module::Module;

pub unsafe fn compile_function(module: *mut Module, function: *mut Function, optimization_level: OptimizationLevel) -> Result<usize, CompileError> {
    let module = &mut *module;

    let context = Context::create();
    let builder = context.create_builder();
    let llvm_module = context.create_module(module.name.as_str());
    let mut llvm_values = LLVMValues::new();
    let llvm_values_ref = &mut llvm_values;

    //Create llvm function
    let mut argument_types: Vec<BasicMetadataTypeEnum> = Vec::new();
    for argument_type in (*function).argument_type_list.iter() {
        match convert_type_to_llvm(argument_type, &context) {
            LLVMTypeTemp::Int(int_type) => argument_types.push(int_type.into()),
            LLVMTypeTemp::Float(float_type) => argument_types.push(float_type.into()),
            LLVMTypeTemp::Pointer(pointer_type) => argument_types.push(pointer_type.into()),
            LLVMTypeTemp::Void(_) => {}
        }
    }

    let function_type = match convert_type_to_llvm(&(*function).return_type, &context) {
        LLVMTypeTemp::Int(int_type) => int_type.fn_type(argument_types.as_slice(), false),
        LLVMTypeTemp::Float(float_type) => float_type.fn_type(argument_types.as_slice(), false),
        LLVMTypeTemp::Pointer(pointer_type) => pointer_type.fn_type(argument_types.as_slice(), false),
        LLVMTypeTemp::Void(void_type) => void_type.fn_type(argument_types.as_slice(), false),
    };

    let llvm_function = llvm_module.add_function((*function).name.as_str(), function_type, Option::None);

    for label_block in (*function).label_block_list.iter() {
        let llvm_block = context.append_basic_block(llvm_function, label_block.name.as_str());
        builder.position_at_end(llvm_block);

        for order in label_block.order_list.iter() {
            match order.compile(module, &mut *function, &context, &builder, &llvm_module, llvm_values_ref) {
                Ok(_) => {},
                Err(err) => { return Err(err); }
            }
        }
    }

    let jit_engine = match llvm_module.create_jit_execution_engine(optimization_level) {
        Ok(engine) => engine,
        Err(err) => { return Err(CompileError::LLVMCreateJITCompileEngineError(err)); }
    };

    let function_address = match jit_engine.get_function_address((*function).name.as_str()) {
        Ok(address) => address,
        Err(err) => { return Err(CompileError::LLVMFunctionLookupError(err)); }
    };

    let jit_function = transmute_copy::<usize, unsafe extern "C" fn() -> i64>(&function_address);
    let result = jit_function();

    println!("JIT result = {}", result);

    println!("{}", llvm_module.print_to_string().to_string());

    return Ok(function_address);
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