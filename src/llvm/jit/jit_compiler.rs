use std::collections::VecDeque;
use std::sync::{Mutex, RwLock};
use inkwell::context::Context;
use threadpool::ThreadPool;
use crate::{LLVMModuleHolder, LLVMValues, SpinLock};
use crate::llvm::compiler::CompileError;
use crate::vm::module::function::Function;
use crate::vm::module::vm_module::Module;

pub struct JITCompiler {
    compiler_thread: ThreadPool,
    holder: LLVMModuleHolder<'static>,
    llvm_values: LLVMValues<'static>
}

impl JITCompiler {

    pub unsafe fn new() -> Self {
        let compiler_thread = ThreadPool::new(1);

        let context = Box::into_raw(Box::new(Context::create())).as_ref::<'static>().unwrap();
        let llvm_module = context.create_module(format!("jit_compiler").as_str());
        let builder = context.create_builder();
        let execution_engine = llvm_module.create_execution_engine().unwrap();

        let holder = LLVMModuleHolder {
            context,
            llvm_module,
            builder,
            execution_engine
        };

        let llvm_values = LLVMValues::new();

        return Self {
            compiler_thread,
            holder,
            llvm_values
        };
    }

    pub fn schedule(&mut self, module: *mut Module, function: *mut Function) {
        let this_ptr = unsafe { self as *mut JITCompiler as u64 };
        let module = module as u64;
        let function = function as u64;

        self.compiler_thread.execute(move || {
            let jit_compiler = unsafe { &mut *(this_ptr as *mut JITCompiler) };
            let holder = &jit_compiler.holder;
            let llvm_values = &mut jit_compiler.llvm_values;

            let module = module as *mut Module;
            let function = function as *mut Function;

            let result = unsafe { holder.compile_function(llvm_values, module, function) };
            match result {
                Ok(_) => {},
                Err(err) => {
                    unsafe {
                        panic!("Failed to jit compile. Module: {}, Function: {}\n{}", (*module).name, (*function).name, err);
                    }
                }
            }

            unsafe {
                let function_address = match holder.execution_engine.get_function_address((*function).name.as_str()) {
                    Ok(address) => address,
                    Err(err) => {
                        panic!("Failed to jit compile. Module: {}, Function: {}\n{}", (*module).name, (*function).name, CompileError::LLVMFunctionLookupError(err));
                    }
                };

                (*function).jit_function_address = function_address;
            }
        });
    }

    pub fn join(&self) {
        self.compiler_thread.join();
    }

}

