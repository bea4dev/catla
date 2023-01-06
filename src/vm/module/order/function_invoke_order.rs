use std::ptr::null_mut;
use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::targets::TargetData;
use crate::vm::module::function::Function;
use crate::vm::module::order::orders::Order;
use crate::vm::module::vm_module::Module;
use crate::{LLVMValues, VMThread};
use crate::llvm::compiler::CompileError;
use crate::vm::tortie::{ModuleLoadError, run_function};

pub struct InvokeFunction {
    target_index: usize,
    import_module_index: usize,
    function_name: String,
    argument_register_list: Vec<usize>,
    function: *mut Function
}

impl InvokeFunction {
    pub fn new(target_index: usize, import_module_index: usize, function_name: String, argument_register_list: Vec<usize>) -> Self {
        return Self {
            target_index,
            import_module_index,
            function_name,
            argument_register_list,
            function: null_mut()
        }
    }
}

impl Order for InvokeFunction {
    fn eval(&self, vm_thread: *mut VMThread, module: *mut Module, registers: &mut Vec<u64>, variables: &mut Vec<u64>, arguments: &Vec<u64>) {
        let arguments = unsafe { (*(*vm_thread).arguments).push(self.argument_register_list.len(), 0) };
        for register_index in self.argument_register_list.iter() {
            arguments.push(registers[*register_index]);
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
        return Ok(());
    }

    fn compile<'a>(&self, module: &mut Module, function: &mut Function, context: &'a Context, builder: &Builder<'a>, llvm_module: &inkwell::module::Module<'a>, llvm_values: &mut LLVMValues<'a>) -> Result<(), CompileError> {
        let vm_function_address = context.ptr_sized_int_type(, );

        return Ok(());
    }
}