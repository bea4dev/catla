use std::ptr::null_mut;
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicUsize, fence, Ordering};
use std::{mem, thread};
use std::collections::HashSet;
use std::thread::JoinHandle;
use std::time::Instant;
use crate::heap::allocator::{HeapAllocator, HeapObject, object_lock, OBJECT_STATE_DEAD, object_unlock};
use crate::heap::gc::{CycleCollector, decrement_reference_count, increment_reference_count};
use vm::module::object_type::ObjectType;
use crate::util::concurrent::SpinLock;
use crate::vm::module::parser::parse_module;
use crate::vm::tortie::{TortieVM, VMThread};

mod heap;
mod vm;
mod util;

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
  1:const#9
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
  0:const#0:reg:5:var:0()->i64{
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
    parse_module(code);


    println!("COMPLETE!");
}
