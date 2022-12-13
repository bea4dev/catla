use crate::vm::module::function::Function;
use crate::vm::module::vm_module::Module;
use crate::VMThread;

pub trait Order {

    fn eval(&self, vm_thread: *mut VMThread, module: *mut Module, registers: &mut Vec<u64>, variables: &mut Vec<u64>, arguments: &mut Vec<u64>);

    fn link(&mut self, module: *mut Module, function: *mut Function);

}