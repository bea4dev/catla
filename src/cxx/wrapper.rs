use std::ffi::c_void;
use crate::VMThread;
use crate::vm::module::function::Function;
use crate::vm::module::vm_module::Module;

extern {
    pub fn create_jump_buffer_wrapped() -> *mut c_void;
    pub fn run_function_for_set_jump_branch(vm_thread: *mut VMThread, module: *mut Module,
                                            function1: *mut Function, argument1: *const Vec<u64>,
                                            function2: *mut Function, argument2: *const Vec<u64>) -> u64;
    pub fn long_jump_wrapped(buffer: *mut c_void, value: usize);
}