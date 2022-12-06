use std::ptr::null_mut;
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicUsize, Ordering};
use std::{mem, thread};
use std::collections::HashSet;
use std::thread::JoinHandle;
use crate::heap::allocator::{HeapAllocator, HeapObject, object_lock, OBJECT_STATE_DEAD, object_unlock};
use crate::heap::gc::{CycleCollector, decrement_reference_count, increment_reference_count};
use crate::module::object_type::ObjectType;
use crate::util::concurrent::SpinLock;
use crate::vm::tortie::{TortieVM, VMThread};

mod heap;
mod module;
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

pub fn set_clone_object_field(cycle_collector: *mut CycleCollector, object: *mut HeapObject, field_index: usize, field_object: *mut HeapObject) {
    unsafe {
        if field_object != null_mut() {
            increment_reference_count(cycle_collector, field_object)
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

pub fn get_clone_object_field(cycle_collector: *mut CycleCollector, object: *mut HeapObject, field_index: usize) -> *mut HeapObject {
    unsafe {
        let fields_ptr = (object as usize) + mem::size_of::<HeapObject>();
        let field_ptr = (fields_ptr + mem::size_of::<u64>() * field_index) as *mut *mut HeapObject;

        object_lock(object);
        let field_object = *field_ptr;
        if field_object != null_mut() {
            increment_reference_count(cycle_collector, field_object);
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

    unsafe {
        let virtual_machine = TortieVM::new();

        let mut handles: Vec<JoinHandle<()>> = Vec::new();

        let main_thread = (&mut *virtual_machine).create_thread(1024);

        let object_type = ObjectType::new(true);

        let mut chunk_search_start_index = 0;
        let allocator = &mut *(&*main_thread).get_heap_allocator();
        let module_object = allocator.malloc(object_type, 10, &mut chunk_search_start_index);
        let obj0 = allocator.malloc(object_type, 2, &mut chunk_search_start_index);
        let obj1 = allocator.malloc(object_type, 2, &mut chunk_search_start_index);
        let obj2 = allocator.malloc(object_type, 2, &mut chunk_search_start_index);
        let obj3 = allocator.malloc(object_type, 2, &mut chunk_search_start_index);
        let obj4 = allocator.malloc(object_type, 2, &mut chunk_search_start_index);
        let obj5 = allocator.malloc(object_type, 2, &mut chunk_search_start_index);
        let obj6 = allocator.malloc(object_type, 2, &mut chunk_search_start_index);
        let obj7 = allocator.malloc(object_type, 2, &mut chunk_search_start_index);
        let obj8 = allocator.malloc(object_type, 2, &mut chunk_search_start_index);
        let obj9 = allocator.malloc(object_type, 2, &mut chunk_search_start_index);
        set_move_object_field(module_object, 0, obj0);
        set_move_object_field(module_object, 1, obj1);
        set_move_object_field(module_object, 2, obj2);
        set_move_object_field(module_object, 3, obj3);
        set_move_object_field(module_object, 4, obj4);
        set_move_object_field(module_object, 5, obj5);
        set_move_object_field(module_object, 6, obj6);
        set_move_object_field(module_object, 7, obj7);
        set_move_object_field(module_object, 8, obj8);
        set_move_object_field(module_object, 9, obj9);
        let module_object_ptr = module_object as usize;
        let mut objects: HashSet<*mut HeapObject> = HashSet::new();
        let objects_ptr = (&mut objects) as *mut HashSet<*mut HeapObject> as usize;
        let object_type_ptr = object_type as usize;

        for i in 0..3 {
            let vm_thread_ptr = (&mut *virtual_machine).create_thread(1024) as usize;
            let handle = thread::spawn(move || {
                let vm_thread = vm_thread_ptr as *mut VMThread;
                println!("START!");

                let mut chunk_search_start_index = 0;

                let cycle_collector = (&*(&*vm_thread).get_virtual_machine()).get_cycle_collector();
                let allocator = &mut *(&*vm_thread).get_heap_allocator();

                let module_object = module_object_ptr as *mut HeapObject;
                let objects = &mut *(objects_ptr as *mut HashSet<*mut HeapObject>);
                let object_type = object_type_ptr as *mut ObjectType;
                for i in i..1000000 {
                    if get_10(i) % 2 == 0 {
                        let obj1 = allocator.malloc(object_type, 2, &mut chunk_search_start_index);
                        let obj2 = allocator.malloc(object_type, 2, &mut chunk_search_start_index);
                        let obj3 = allocator.malloc(object_type, 2, &mut chunk_search_start_index);

                        object_lock(module_object);
                        objects.insert(obj1);
                        objects.insert(obj2);
                        objects.insert(obj3);
                        object_unlock(module_object);

                        set_move_object_field(module_object, get_10(i + 1), obj1);
                        set_move_object_field(module_object, get_10(i + 2), obj2);
                        set_move_object_field(module_object, get_10(i + 3), obj3);
                    } else {
                        let obj1 = get_clone_object_field(cycle_collector, module_object, get_10(i + 1));
                        let obj2 = get_clone_object_field(cycle_collector, module_object, get_10(i + 2));
                        let obj3 = get_clone_object_field(cycle_collector, module_object, get_10(i + 3));

                        if get_10(i) % 2 == 0 {
                            set_clone_object_field(cycle_collector, obj1, get_10(i + 2) % 2, obj2);
                            set_move_object_field(obj2, get_10(i + 5) % 2, obj3);
                            decrement_reference_count(obj1);
                            decrement_reference_count(obj2);
                        } else {
                            set_clone_object_field(cycle_collector, obj1, get_10(i + 1) % 2, obj2);
                            set_clone_object_field(cycle_collector, obj2, get_10(i + 2) % 2, obj3);
                            set_clone_object_field(cycle_collector, obj3, get_10(i + 3) % 2, obj1);
                            decrement_reference_count(obj1);
                            decrement_reference_count(obj2);
                            decrement_reference_count(obj3);
                        }
                    }
                }

                println!("END!");
            });
            handles.push(handle);
        }

        for handle in handles {
            handle.join().expect("Failed to wait.");
        }

        decrement_reference_count(module_object);

        (*(*virtual_machine).get_cycle_collector()).gc_collect();

        println!("-------- LIVING OBJECTS INFO --------");
        for object in objects.iter() {
            if (**object).state.load(Ordering::Acquire) != OBJECT_STATE_DEAD {
                println!("NOT DEAD! {}", *object as usize);
            } else {
                //println!("DEAD! {}", *object as usize);
            }
        }
        println!("-------------------------------------");
    }

    println!("COMPLETE!");
}
