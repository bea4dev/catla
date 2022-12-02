use std::ptr::null_mut;
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicUsize};
use std::thread;
use std::thread::JoinHandle;
use crate::heap::allocator::{HeapObject, object_lock, object_unlock};
use crate::util::concurrent::SpinLock;

mod heap;
mod module;
mod vm;
mod util;

unsafe impl Send for HeapObject {

}

fn main() {
    println!("Hello, world!");

    let mut object = SpinLock::new();

    unsafe {
        let raw_pointer = &mut object as *mut SpinLock;
        let mut i = 0;
        let int_ptr = &mut i as *mut i32;

        let mut joins: Vec<JoinHandle<()>> = Vec::new();
        for i in 0..20 {
            let raw_ptr = raw_pointer as usize;
            let int_raw_ptr = int_ptr as usize;
            let handle = thread::spawn(move || {
                println!("START!");
                for i in 0..100000 {
                    (*(raw_ptr as *mut SpinLock)).lock();
                    (*(int_raw_ptr as *mut i32)) += 1;
                    (*(raw_ptr as *mut SpinLock)).unlock();
                }
                println!("END!");
            });

            joins.push(handle);
        }

        for handle in joins {
            handle.join().expect("TODO: panic message");
        }

        println!("result = {}", i);
    }
}
