use std::ptr::null_mut;
use std::{mem};
use std::mem::transmute_copy;
use inkwell::context::Context;
use inkwell::OptimizationLevel;
use crate::heap::allocator::{HeapAllocator, HeapObject, object_lock, object_unlock};
use crate::heap::gc::{CycleCollector, decrement_reference_count, increment_reference_count};
use vm::module::object_type::ObjectType;
use crate::llvm::compiler::{LLVMModuleHolder, LLVMValues};
use crate::util::concurrent::SpinLock;
use crate::vm::module::parser::parse_module;
use crate::vm::tortie::{TortieVM, VMThread};

mod heap;
mod vm;
mod util;
mod cxx;
mod llvm;

pub fn set_move_object_field(object: *mut HeapObject, field_index: usize, field_object: *mut HeapObject) {
    unsafe {
        let fields_ptr = (object as usize) + mem::size_of::<HeapObject>();
        let field_ptr = (fields_ptr + mem::size_of::<u64>() * field_index) as *mut *mut HeapObject;

        object_lock(object);
        let previous_field_object = *field_ptr;
        *field_ptr = field_object;
        object_unlock(object);

        if previous_field_object != null_mut() {
            decrement_reference_count(previous_field_object);
        }
    }
}

pub fn set_clone_object_field(vm_thread: *mut VMThread, object: *mut HeapObject, field_index: usize, field_object: *mut HeapObject) {
    unsafe {
        if field_object != null_mut() {
            increment_reference_count(vm_thread, field_object)
        }
        set_move_object_field(object, field_index, field_object);
    }
}

pub fn get_move_object_field(object: *mut HeapObject, field_index: usize) -> *mut HeapObject {
    unsafe {
        let fields_ptr = (object as usize) + mem::size_of::<HeapObject>();
        let field_ptr = (fields_ptr + mem::size_of::<u64>() * field_index) as *mut *mut HeapObject;

        object_lock(object);
        let field_object = *field_ptr;
        *field_ptr = null_mut();
        object_unlock(object);

        return field_object;
    }
}

pub fn get_clone_object_field(vm_thread: *mut VMThread, object: *mut HeapObject, field_index: usize) -> *mut HeapObject {
    unsafe {
        let fields_ptr = (object as usize) + mem::size_of::<HeapObject>();
        let field_ptr = (fields_ptr + mem::size_of::<u64>() * field_index) as *mut *mut HeapObject;

        object_lock(object);
        let field_object = *field_ptr;
        if field_object != null_mut() {
            increment_reference_count(vm_thread, field_object);
        }
        object_unlock(object);

        return field_object;
    }
}

#[inline(always)]
pub fn get_10(i: usize) -> usize {
    return i % 10;
}

fn main() {
    println!("Hello, world!");

    let code =
"$const
  0:4:s#nyan
  1:2:i#20
  2:3:i#100
  3:4:i#1000
  4:9:s#TestClass
  5:10:s#TestClass2
  6:1:s#a
  7:1:s#b
  8:1:s#c
  9:17:s#catla::collection
$end

$import
  0:this
$end

$typedef
  const#4:(const#6:i64,const#7:import#0:const#5):import#0:const#5
  const#5:(const#8:i64)
$end

$type
  0:import#0:const#4
  1:import#0:const#5
$end

$function
  const#0:reg:5:var:0()->i64{
    label:entry
      reg#0 = const,i64,1
      reg#1 = const,i64,2
      reg#2 = iadd,i64,reg#0,reg#1
      reg#3 = const,i64,3
      reg#4 = iadd,i64,reg#2,reg#3
      ret,reg#4
    label:end
  }
$end";

    unsafe {
        let virtual_machine = TortieVM::new();
        (*virtual_machine).pre_load_module("test".to_string(), code.to_string()).expect("Parse error!");
        let result = (*virtual_machine).load_module("test".to_string());
        match result {
            Ok(_) => {},
            Err(err) => panic!("{}", err)
        }

        let arguments: Vec<u64> = vec![];

        let result = (*virtual_machine).run_function(&"test".to_string(), &"nyan".to_string(), &arguments);

        match result {
            Ok(result) => println!("Result = {}", result),
            Err(err) => println!("Error:\n{}", err)
        }

        println!("Run JIT compile!");

        let module = (*virtual_machine).get_module(&"test".to_string()).unwrap();
        let function = (*module).get_function_ptr(&"nyan".to_string()).unwrap();
        let context = Context::create();
        let llvm_module = context.create_module((*module).name.as_str());
        let builder = context.create_builder();
        let execution_engine = llvm_module.create_execution_engine().unwrap();
        let mut llvm_values = LLVMValues::new();

        let holder = LLVMModuleHolder {
            module,
            function,
            context: &context,
            llvm_module,
            builder,
            execution_engine
        };

        holder.compile_function(&mut llvm_values).expect("Failed to compile!");

        println!("OK!");


        let jit_engine = &holder.execution_engine;
        let function_address = jit_engine.get_function_address("nyan").unwrap();
        let jit_function = transmute_copy::<usize, unsafe extern "C" fn(*mut VMThread) -> i64>(&function_address);

        let thread = (*virtual_machine).create_thread(1024);
        let result = jit_function(thread);
        println!("JIT result = {}", result);
    }


    println!("COMPLETE!");
}
