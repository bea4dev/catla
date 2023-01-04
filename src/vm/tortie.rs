use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::{env, panic};
use std::ffi::c_void;
use std::ops::{Index, IndexMut};
use std::panic::PanicInfo;
use std::ptr::{null_mut};
use std::sync::RwLock;
use crate::{CycleCollector, HeapAllocator, HeapObject, ObjectType, parse_module, SpinLock};
use crate::cxx::wrapper::{create_jump_buffer_wrapped, run_function_for_set_jump_branch};
use crate::vm::module::function::{Function, FunctionLabelBlock};
use crate::vm::module::object_type::Type;
use crate::vm::module::parser::{ArgumentTypeInfo, ByteCodeParseError, TypeInfo};
use crate::vm::module::vm_module::Module;
use crate::vm::runtime::error::RuntimeError;


#[repr(C)]
pub struct TortieVM {
    pub threads: Vec<*mut VMThread>,
    pub thread_list_lock: SpinLock,
    cycle_collector: *mut CycleCollector,
    module_map: RwLock<HashMap<String, *mut Module>>
}


#[repr(C)]
pub struct VMThread {
    virtual_machine: *mut TortieVM,
    stack_size: usize,
    heap_allocator: *mut HeapAllocator,
    pub runtime_exception: *mut RuntimeError,
    pub jump_buffer_stack: Vec<*mut c_void>,
    pub suspected_cycle_objects: HashSet<*mut HeapObject>,
    pub object_set_lock: SpinLock,
    pub current_function: *mut Function,
    pub is_return_function: bool,
    pub current_label_block: *mut FunctionLabelBlock,
    pub registers: *mut StackedVec<u64>,
    pub variables: *mut StackedVec<u64>,
    pub arguments: *mut StackedVec<u64>
}

#[no_mangle]
unsafe extern "C" fn add_jump_buffer(vm_thread: *mut VMThread) -> *mut c_void {
    let jump_buffer = create_jump_buffer_wrapped();
    (*vm_thread).jump_buffer_stack.push(jump_buffer);
    return jump_buffer;
}


pub struct StackedVec<T> {
    pub vec_stack: Vec<Vec<T>>,
}

impl<T> Index<usize> for StackedVec<T> {
    type Output = Vec<T>;
    fn index(&self, index: usize) -> &Self::Output {
        return &self.vec_stack[index];
    }
}

impl<T> IndexMut<usize> for StackedVec<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        return &mut self.vec_stack[index];
    }
}

impl<T: Copy> StackedVec<T> {
    pub fn new() -> Self {
        let instance: StackedVec<T> = Self {
            vec_stack: Vec::new()
        };
        return instance;
    }

    pub fn push(&mut self, size: usize, default: T) -> &mut Vec<T> {
        let mut inr: Vec<T> = Vec::with_capacity(size);
        for _ in 0..size {
            inr.push(default);
        }
        self.vec_stack.push(inr);

        let length = self.vec_stack.len();
        return &mut self.vec_stack[length - 1];
    }

    pub fn pop(&mut self) {
        self.vec_stack.pop();
    }
}


impl TortieVM {

    pub unsafe fn new() -> *mut TortieVM {
        let virtual_machine = Self {
            threads: Vec::new(),
            thread_list_lock: SpinLock::new(),
            cycle_collector: null_mut(),
            module_map: RwLock::new(HashMap::new())
        };
        let vm_ptr = Box::into_raw(Box::new(virtual_machine));
        (*vm_ptr).cycle_collector = CycleCollector::new(vm_ptr);

        add_custom_panic_hook();

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
    pub fn get_cycle_collector(&self) -> *mut CycleCollector { return self.cycle_collector; }

    pub fn get_module(&self, name: &String) -> Option<*mut Module> {
        let module_map = self.module_map.read().unwrap();
        return match module_map.get(name) {
            Some(module) => Some(*module),
            _ => None
        }
    }

    pub fn pre_load_module(&mut self, name: String, code: String) -> Result<(), ModuleLoadError> {
        let module = match parse_module(name.clone(), code.as_str()) {
            Ok(module) => module,
            Err(err) => { return Err(ModuleLoadError::ModulePreLoadError(err)); }
        };
        self.module_map.write().unwrap().insert(name, Box::into_raw(Box::new(module)));
        return Ok(());
    }

    pub fn load_module(&mut self, name: String) -> Result<(), ModuleLoadError> {
        unsafe {
            let mut check_modules: Vec<*mut Module> = Vec::new();
            let mut uninitialized_modules: HashSet<*mut Module> = HashSet::new();
            let module_map = self.module_map.write().unwrap();
            let mut current_module = *match module_map.get(&name) {
                Some(module) => module,
                _ => { return Err(ModuleLoadError::ModuleNotFoundError(name.clone())); }
            };

            loop {
                uninitialized_modules.insert(current_module);

                for import_module_name in (*current_module).import_module_name_list.iter() {
                    let import_module = if import_module_name.as_str() == "this" {
                        current_module
                    } else {
                        match module_map.get(import_module_name) {
                            Some(module) => *module,
                            _ => { return Err(ModuleLoadError::ModuleHasNotBeenLoadedError(import_module_name.clone())); }
                        }
                    };
                    if !(*import_module).is_initialized {
                        check_modules.push(import_module);
                        (*import_module).is_initialized = true;
                    }
                    (*current_module).import_module_list.push(import_module);
                }

                for defined_type in (*current_module).defined_type_info_list.iter() {
                    let object_type = ObjectType::new((*defined_type).clone());
                    (*current_module).defined_type_map.insert(defined_type.type_name.clone(), object_type);
                }

                current_module = match check_modules.pop() {
                    Some(module) => module,
                    _ => { break }
                };
            }

            for module in &uninitialized_modules {
                let module = *module;
                for defined_type in (*module).defined_type_map.values() {
                    let defined_type = *defined_type;
                    match (*defined_type).extends_type_info.clone() {
                        Some(info) => {
                            match info {
                                TypeInfo::DefinedType {import_module_index, type_name} => {
                                    let object_type = match get_defined_type(module, import_module_index, &type_name) {
                                        Ok(object_type) => object_type,
                                        Err(err) => { return Err(err); }
                                    };
                                    (*defined_type).extends_type = object_type;
                                },
                                TypeInfo::PrimitiveType {primitive_type: _} => {}
                            }
                        },
                        _ => {}
                    }

                    for field_info in (*defined_type).field_info_map.values() {
                        let field_name = field_info.field_name.clone();
                        let field_type = match &field_info.type_info {
                            TypeInfo::DefinedType {import_module_index, type_name} => {
                                let object_type = match get_defined_type(module, *import_module_index, type_name) {
                                    Ok(object_type) => object_type,
                                    Err(err) => { return Err(err); }
                                };
                                Type::ObjectReference(object_type, false)
                            },
                            TypeInfo::PrimitiveType {primitive_type} => {
                                (*primitive_type).clone()
                            }
                        };
                        (*defined_type).field_map.insert(field_name, field_type);
                    }
                }
            }

            for module in &uninitialized_modules {
                let module = *module;
                for using_type_info in (*module).using_type_info_list.iter() {
                    let using_type_info = using_type_info.clone();
                    match using_type_info {
                        TypeInfo::DefinedType {import_module_index, type_name} => {
                            let object_type = match get_defined_type(module, import_module_index, &type_name) {
                                Ok(object_type) => object_type,
                                Err(err) => { return Err(err); }
                            };
                            (*module).using_type_list.push(object_type);
                        },
                        TypeInfo::PrimitiveType {primitive_type: _} => {}
                    }
                }
            }

            for module in &uninitialized_modules {
                let module = *module;
                for function in (*module).function_map.values_mut() {
                    let return_type_info = &function.return_type_info;
                    let return_type = match get_type(module, return_type_info) {
                        Ok(object_type) => object_type,
                        Err(err) => { return Err(err); }
                    };
                    function.return_type = return_type;
                }

                for function in (*module).function_map.values_mut() {
                    for argument_type_info in function.argument_type_info_list.iter() {
                        let argument_type = match get_argument_type(module, argument_type_info) {
                            Ok(object_type) => object_type,
                            Err(err) => { return Err(err); }
                        };
                        function.argument_type_list.push(argument_type);
                    }
                }
            }

            for module in &uninitialized_modules {
                let module = *module;
                for function in (*module).function_map.values_mut() {
                    let function = function as *mut Function;
                    for label_block in (*function).label_block_list.iter_mut() {
                        for order in label_block.order_list.iter_mut() {
                            order.link(module, function);
                        }
                    }
                }
            }

            return Ok(());
        }
    }

    pub fn run_function(&mut self, module_name: &String, function_name: &String, arguments: &Vec<u64>) -> Result<u64, RuntimeError> {
        unsafe {
            let vm_thread = self.create_thread(1024);
            let module = self.get_module(&module_name).unwrap();
            let function = (*module).get_function_ptr(function_name).unwrap();

            let result = run_function_for_set_jump_branch(vm_thread, module, function, arguments, function, arguments);

            return Ok(result);
        }
    }

}


#[no_mangle]
pub unsafe extern "C" fn run_function(vm_thread: *mut VMThread, module: *mut Module, function: *mut Function, arguments: *const Vec<u64>) -> u64 {
    let registers: &mut Vec<u64> = (*(*vm_thread).registers).push((*function).register_length + 1, 0);
    let variables: &mut Vec<u64> = (*(*vm_thread).variables).push((*function).variable_length, 0);

    (*vm_thread).current_function = function;
    (*vm_thread).is_return_function = false;

    let first_label_block = match (*function).label_block_list.get_mut(0) {
        Some(label_block) => label_block,
        _ => { return 0; }
    };
    (*vm_thread).current_label_block = first_label_block as *mut FunctionLabelBlock;


    let order_list = &mut (*(*vm_thread).current_label_block).order_list;

    loop {
        for i in 0..order_list.len() {
            let order = &order_list[i];
            order.eval(vm_thread, module, registers, variables, &*arguments);

            if (*vm_thread).runtime_exception != null_mut() {
                break;
            }
        }

        if (*vm_thread).current_label_block == null_mut() || (*vm_thread).is_return_function {
            break;
        }
    }

    return registers[(*function).register_length];
}



#[inline(always)]
unsafe fn get_defined_type(module: *mut Module, import_module_index: usize, type_name: &String) -> Result<*mut ObjectType, ModuleLoadError> {
    let module = *match (*module).import_module_list.get(import_module_index) {
        Some(module) => module,
        _ => { return Err(ModuleLoadError::ModuleNotFoundError(import_module_index.to_string())); }
    };
    let object_type = *match (*module).defined_type_map.get(type_name) {
        Some(object_type) => object_type,
        _ => { return Err(ModuleLoadError::DefinedTypeNotFoundError((*type_name).clone())); }
    };
    return Ok(object_type);
}

#[inline(always)]
unsafe fn get_type(module: *mut Module, type_info: &TypeInfo) -> Result<Type, ModuleLoadError> {
    return match type_info {
        TypeInfo::DefinedType {import_module_index, type_name} => {
            let object_type = match get_defined_type(module, *import_module_index, type_name) {
                Ok(object_type) => object_type,
                Err(err) => { return Err(err); }
            };
            Ok(Type::ObjectReference(object_type, false))
        },
        TypeInfo::PrimitiveType {primitive_type} => Ok(primitive_type.clone())
    };
}

#[inline(always)]
unsafe fn get_argument_type(module: *mut Module, type_info: &ArgumentTypeInfo) -> Result<Type, ModuleLoadError> {
    let is_local_object = type_info.is_local_object;
    let type_info = &type_info.type_info;

    return match type_info {
        TypeInfo::DefinedType {import_module_index, type_name} => {
            let object_type = match get_defined_type(module, *import_module_index, type_name) {
                Ok(object_type) => object_type,
                Err(err) => { return Err(err); }
            };
            Ok(Type::ObjectReference(object_type, is_local_object))
        },
        TypeInfo::PrimitiveType {primitive_type} => Ok(primitive_type.clone())
    };
}


#[inline(always)]
fn add_custom_panic_hook() {
    env::set_var("RUST_BACKTRACE", "full");

    let default_hook = panic::take_hook();

    panic::set_hook(Box::new(move |panic_info: &PanicInfo| {
        let header = "!!!!!!!!!! VM Panic !!!!!!!!!!\n\
        A fatal error has occurred.\n\
        ".to_string();

        eprintln!("{}\n{}", header, panic_info);
        default_hook(panic_info);
    }));
}


impl VMThread {

    pub unsafe fn new(virtual_machine: *mut TortieVM, stack_size: usize) -> *mut VMThread {
        let boxed = Box::new(Self {
            virtual_machine,
            stack_size,
            heap_allocator: HeapAllocator::new(virtual_machine, 1024, 1),
            runtime_exception: null_mut(),
            jump_buffer_stack: Vec::new(),
            suspected_cycle_objects: HashSet::new(),
            object_set_lock: SpinLock::new(),
            current_function: null_mut(),
            is_return_function: false,
            current_label_block: null_mut(),
            registers: Box::into_raw(Box::new(StackedVec::new())),
            variables: Box::into_raw(Box::new(StackedVec::new())),
            arguments: Box::into_raw(Box::new(StackedVec::new()))
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
pub enum ModuleLoadError {
    ModulePreLoadError(ByteCodeParseError),
    ModuleNotFoundError(String),
    ModuleHasNotBeenLoadedError(String),
    DefinedTypeNotFoundError(String),
}

impl Display for ModuleLoadError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let error_string = match self {
            ModuleLoadError::ModulePreLoadError(err) => format!("ModuleLoadError\n{}", err),
            ModuleLoadError::ModuleNotFoundError(name) => format!("ModuleNotFoundError | '{}' is not found.", name),
            ModuleLoadError::ModuleHasNotBeenLoadedError(name) => format!("ModuleHasNotBeenLoadedError | '{}' has not yet been loaded.", name),
            ModuleLoadError::DefinedTypeNotFoundError(name) => format!("DefinedTypeNotFoundError | '{}' is not found.", name)
        };

        return write!(f, "ModuleLoadError | {}", error_string);
    }
}