use std::collections::HashSet;
use std::ptr::null_mut;
use crate::{CycleCollector, HeapAllocator, HeapObject, SpinLock};

#[repr(C)]
pub struct TortieVM {
    pub threads: Vec<*mut VMThread>,
    pub thread_list_lock: SpinLock,
    cycle_collector: *mut CycleCollector
}

#[repr(C)]
pub struct VMThread {
    virtual_machine: *mut TortieVM,
    stack_size: usize,
    heap_allocator: *mut HeapAllocator,
    pub suspected_cycle_objects: HashSet<*mut HeapObject>,
    pub object_set_lock: SpinLock
}


impl TortieVM {

    pub unsafe fn new() -> *mut TortieVM {
        let mut virtual_machine = Self {
            threads: Vec::new(),
            thread_list_lock: SpinLock::new(),
            cycle_collector: null_mut()
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

}


impl VMThread {

    pub unsafe fn new(virtual_machine: *mut TortieVM, stack_size: usize) -> *mut VMThread {
        let boxed = Box::new(Self {
            virtual_machine,
            stack_size,
            heap_allocator: HeapAllocator::new(virtual_machine, 1024, 1),
            suspected_cycle_objects: HashSet::new(),
            object_set_lock: SpinLock::new()
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