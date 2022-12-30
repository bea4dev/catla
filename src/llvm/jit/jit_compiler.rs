use std::sync::RwLock;
use threadpool::ThreadPool;
use crate::SpinLock;
use crate::vm::module::function::Function;
use crate::vm::module::vm_module::Module;

pub struct JITCompiler {
    compiler_threads: ThreadPool
}

impl JITCompiler {

    pub fn new(num_threads: usize) -> Self {
        return Self {
            compiler_threads: ThreadPool::new(num_threads)
        };
    }

    pub fn schedule(&self, function: *mut Function) {
        self.compiler_threads.execute(move || {

        });
    }

}

