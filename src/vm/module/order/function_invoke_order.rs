use std::ptr::null_mut;
use either::Either;
use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::targets::TargetData;
use inkwell::types::{BasicMetadataTypeEnum, FunctionType};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, CallableValue, FunctionValue};
use crate::vm::module::function::Function;
use crate::vm::module::order::orders::Order;
use crate::vm::module::vm_module::Module;
use crate::{LLVMModuleHolder, LLVMValues, VMThread};
use crate::llvm::compiler::{CompileError, convert_type_to_llvm, InstantNameProvider, LLVMTypeTemp};
use crate::vm::module::object_type::Type;
use crate::vm::module::parser::TypeInfo;
use crate::vm::tortie::{get_type, ModuleLoadError, run_function};

pub struct FunctionTypeInfo {
    register_index: usize,
    type_info: TypeInfo,
    value_type: Type
}

pub struct InvokeFunction {
    target_index: usize,
    import_module_index: usize,
    function_name: String,
    return_type: FunctionTypeInfo,
    argument_list: Vec<FunctionTypeInfo>,
    function: *mut Function
}

impl InvokeFunction {
    pub fn new(target_index: usize, import_module_index: usize, function_name: String, return_type: FunctionTypeInfo, argument_list: Vec<FunctionTypeInfo>) -> Self {
        return Self {
            target_index,
            import_module_index,
            function_name,
            return_type,
            argument_list,
            function: null_mut()
        }
    }
}

impl Order for InvokeFunction {
    fn eval(&self, vm_thread: *mut VMThread, module: *mut Module, registers: &mut Vec<u64>, variables: &mut Vec<u64>, arguments: &Vec<u64>) {
        let arguments = unsafe { (*(*vm_thread).arguments).push(self.argument_list.len(), 0) };
        for argument_info in self.argument_list.iter() {
            arguments.push(registers[argument_info.register_index]);
        }
        let result = unsafe { run_function(vm_thread, module, self.function, arguments) };
        registers[self.target_index] = result;
    }

    fn link(&mut self, module: *mut Module, function: *mut Function) -> Result<(), ModuleLoadError> {
        let import_module = match unsafe { (*module).import_module_list.get(self.import_module_index) } {
            Some(module) => *module,
            _ => { return Err(ModuleLoadError::ModuleNotFoundError(format!("Index : {}", self.import_module_index))); }
        };
        let target_function = match unsafe { (*import_module).get_function_ptr(&self.function_name) } {
            Some(function) => function,
            _ => { return Err(ModuleLoadError::FunctionNotFoundError(self.function_name.clone())); }
        };
        self.function = target_function;

        for argument_info in self.argument_list.iter_mut() {
            argument_info.value_type = match unsafe { get_type(module, &argument_info.type_info) } {
                Ok(value_type) => value_type,
                Err(err) => { return Err(err); }
            };
        }

        return Ok(());
    }

    fn compile<'a>(&self, module: &mut Module, function: &mut Function, llvm_module_holder: &LLVMModuleHolder<'a>, llvm_function: &FunctionValue<'a>, name_provider: &mut InstantNameProvider, llvm_values: &mut LLVMValues<'a>) -> Result<(), CompileError> {
        let context = llvm_module_holder.context;
        let builder = &llvm_module_holder.builder;

        let address_type = context.ptr_sized_int_type(llvm_module_holder.execution_engine.get_target_data(), None);
        let vm_function_address = address_type.const_int(self.function as u64, false);
        let load_function_address = builder.build_load(vm_function_address.const_to_pointer(address_type.ptr_type(AddressSpace::default())), format!("instant#{}#load_func_jit", self.target_index).as_str());


        let if_has_jit_block = context.append_basic_block(llvm_function.clone(), format!("instant#{}#has_jit_block", self.target_index).as_str());
        builder.position_at_end(if_has_jit_block);

        let mut argument_type_list: Vec<BasicMetadataTypeEnum<'a>> = Vec::new();
        for argument_info in self.argument_list.iter() {
            match convert_type_to_llvm(&argument_info.value_type, context) {
                LLVMTypeTemp::Int(int_type) => argument_type_list.push(int_type.into()),
                LLVMTypeTemp::Float(float_type) => argument_type_list.push(float_type.into()),
                LLVMTypeTemp::Pointer(pointer_type) => argument_type_list.push(pointer_type.into()),
                LLVMTypeTemp::Void(_) => {}
            }
        }

        let function_type = match convert_type_to_llvm(&self.return_type.value_type, context) {
            LLVMTypeTemp::Int(int_type) => int_type.fn_type(argument_type_list.as_slice(), false),
            LLVMTypeTemp::Float(float_type) => float_type.fn_type(argument_type_list.as_slice(), false),
            LLVMTypeTemp::Pointer(pointer_type) => pointer_type.fn_type(argument_type_list.as_slice(), false),
            LLVMTypeTemp::Void(void_type) => void_type.fn_type(argument_type_list.as_slice(), false),
        };

        let mut arguments: Vec<BasicMetadataValueEnum<'a>> = Vec::new();
        for argument_info in self.argument_list.iter() {
            let value_enum: BasicMetadataValueEnum<'a> = match argument_info.value_type {
                Type::I8 => match llvm_values.get_value(argument_info.register_index) { Ok(value) => value.into(), Err(err) => { return Err(err); } },
                Type::I16 => match llvm_values.get_value(argument_info.register_index) { Ok(value) => value.into(), Err(err) => { return Err(err); } },
                Type::I32 => match llvm_values.get_value(argument_info.register_index) { Ok(value) => value.into(), Err(err) => { return Err(err); } },
                Type::I64 => match llvm_values.get_value(argument_info.register_index) { Ok(value) => value.into(), Err(err) => { return Err(err); } },
                Type::U8 => match llvm_values.get_value(argument_info.register_index) { Ok(value) => value.into(), Err(err) => { return Err(err); } },
                Type::U16 => match llvm_values.get_value(argument_info.register_index) { Ok(value) => value.into(), Err(err) => { return Err(err); } },
                Type::U32 => match llvm_values.get_value(argument_info.register_index) { Ok(value) => value.into(), Err(err) => { return Err(err); } },
                Type::U64 => match llvm_values.get_value(argument_info.register_index) { Ok(value) => value.into(), Err(err) => { return Err(err); } },
                _ => { return Err(CompileError::TypeMismatchError("integer".to_string(), "other".to_string())) }
            };

            arguments.push(value_enum);
        }

        let alloc_function_pointer = builder.build_alloca(function_type.ptr_type(AddressSpace::default()), format!("instant#{}#alloc_jit_func", self.target_index).as_str());
        builder.build_store(alloc_function_pointer, load_function_address);
        let load_function_pointer = builder.build_load(alloc_function_pointer, format!("instant#{}#load_jit_func", self.target_index).as_str());

        let callable_ptr = CallableValue::try_from(load_function_pointer.into_pointer_value()).unwrap();
        let jit_function_call = builder.build_call(callable_ptr, arguments.as_slice(), format!("instant#{}#call_jit_func", self.target_index).as_str());
        let value = jit_function_call.try_as_basic_value();
        match value.left() {
            Some(value) => llvm_values.insert_value(self.target_index, value),
            _ => {}
        }


        let else_block = context.append_basic_block(llvm_function.clone(), format!("instant#{}#else_block", self.target_index).as_str());
        builder.position_at_end(else_block);


        return Ok(());
    }
}