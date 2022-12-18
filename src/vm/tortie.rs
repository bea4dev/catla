use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::ptr::null_mut;
use std::sync::RwLock;
use crate::{CycleCollector, HeapAllocator, HeapObject, parse_module, SpinLock};
use crate::vm::module::function::Function;
use crate::vm::module::parser::ByteCodeParseError;
use crate::vm::module::vm_module::Module;

#[repr(C)]
pub struct TortieVM {
    pub threads: Vec<*mut VMThread>,
    pub thread_list_lock: SpinLock,
    cycle_collector: *mut CycleCollector,
    pub module_map: RwLock<HashMap<String, *mut Module>>
}

#[repr(C)]
pub struct VMThread {
    virtual_machine: *mut TortieVM,
    stack_size: usize,
    heap_allocator: *mut HeapAllocator,
    pub suspected_cycle_objects: HashSet<*mut HeapObject>,
    pub object_set_lock: SpinLock,
    pub current_function: *mut Function,
    pub is_return_function: bool
}


impl TortieVM {

    pub unsafe fn new() -> *mut TortieVM {
        let mut virtual_machine = Self {
            threads: Vec::new(),
            thread_list_lock: SpinLock::new(),
            cycle_collector: null_mut(),
            module_map: RwLock::new(HashMap::new())
        };
        let vm_ptr = Box::into_raw(Box::new(virtual_machine));
        (&mut *vm_ptr).cycle_collector = CycleCollector::new(vm_ptr);
        return vm_ptr;
    }

    pub unsafe fn create_thread(&mut self, stack_size: usize) -> *mut VMThread {
        let vm_thread = VMThread::new(self as *mut TortieVM, stack_size);
        self.thread_list_lock.lock();
        self.threads.push(vm_thread);
        self.thread_list_lock.unlock();
        return vm_thread;
    }

    #[inline(always)]
    pub fn get_cycle_collector(&self) -> *mut CycleCollector {
        return self.cycle_collector;
    }

    pub fn pre_load_module(&mut self, name: String, code: String) -> Result<(), VMError> {
        let module = match parse_module(code.as_str()) {
            Ok(module) => module,
            Err(err) => { return Err(VMError::ModulePreLoadError(err)); }
        };
        self.module_map.write().unwrap().insert(name, Box::into_raw(Box::new(module)));
        return Ok(());
    }

    pub fn load_module(&mut self, name: String) -> Result<(), VMError> {
        unsafe {
            let mut module_map = self.module_map.write().unwrap();
            let module = match module_map.get(&name) {
                Some(module) => module,
                _ => { return Err(VMError::ModuleNotFoundError(name.clone())); }
            };

            for import_module_name in (**module).import_module_name_list.iter() {
                let import_module = match module_map.get(import_module_name) {
                    Some(module) => module,
                    _ => { return Err(VMError::ModuleHasNotBeenLoadedError(import_module_name.clone())); }
                };
                (**module).import_module_list.push(*import_module);
            }



            return Ok(());
        }
    }

}


impl VMThread {

    pub unsafe fn new(virtual_machine: *mut TortieVM, stack_size: usize) -> *mut VMThread {
        let boxed = Box::new(Self {
            virtual_machine,
            stack_size,
            heap_allocator: HeapAllocator::new(virtual_machine, 1024, 1),
            suspected_cycle_objects: HashSet::new(),
            object_set_lock: SpinLock::new(),
            current_function: null_mut(),
            is_return_function: false
        });
        return Box::into_raw(boxed);
    }

    #[inline(always)]
    pub fn get_virtual_machine(&self) -> *mut TortieVM { return self.virtual_machine; }

    #[inline(always)]
    pub fn get_stack_size(&self) -> usize { return self.stack_size; }

    #[inline(always)]
    pub fn get_heap_allocator(&self) -> *mut HeapAllocator { return self.heap_allocator; }

    #[inline(always)]
    pub fn add_suspected_object(&mut self, object: *mut HeapObject) {
        self.object_set_lock.lock();
        self.suspected_cycle_objects.insert(object);
        self.object_set_lock.unlock();
    }

}


#[derive(Debug)]
pub enum VMError {
    ModulePreLoadError(ByteCodeParseError),
    ModuleNotFoundError(String),
    ModuleHasNotBeenLoadedError(String)
}

impl Display for VMError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let error_string = match self {
            VMError::ModulePreLoadError(err) => format!("ModuleLoadError\n{}", err),
            VMError::ModuleNotFoundError(name) => format!("ModuleNotFoundError | '{}' is not found.", name),
            VMError::ModuleHasNotBeenLoadedError(name) => format!("ModuleHasNotLoadedError| '{}' has not yet been loaded.", name)
        };

        return write!(f, "VMError | {}", error_string);
    }
}