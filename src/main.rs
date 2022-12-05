use std::ptr::null_mut;
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicUsize, Ordering};
use std::thread;
use std::thread::JoinHandle;
use crate::heap::allocator::{HeapAllocator, HeapObject, object_lock, object_unlock};
use crate::module::object_type::ObjectType;
use crate::util::concurrent::SpinLock;

mod heap;
mod module;
mod vm;
mod util;

unsafe impl Send for HeapObject {

}

fn main() {
    println!("Hello, world!");

    unsafe {
        let object_type = Box::into_raw(Box::new(ObjectType {
            is_cyclic: AtomicBool::new(true)
        }));
        let mut allocator = HeapAllocator::new(null_mut(), 1024, 1);

        let mut objects: Vec<*mut HeapObject> = Vec::new();

        let mut chunk_search_start_index = 0;
        for i in 0..1000 {
            let object = (&mut *allocator).malloc(object_type, 2, &mut chunk_search_start_index);
            if object == null_mut() {
                panic!("NULL!!!");
            }
            (*object).reference_count.fetch_add(1, Ordering::Relaxed);
            objects.push(object);
        }

        for object in objects.iter() {
            if (**object).reference_count.load(Ordering::Acquire) != 2 {
                panic!("NOT 2!!");
            }
        }
    }

    println!("COMPLETE!");
}
