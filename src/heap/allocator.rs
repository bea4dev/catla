use std::sync::atomic::{AtomicBool, AtomicU8, AtomicUsize, Ordering};
use crate::module;
use crate::module::object_type::ObjectType;
use crate::util::concurrent::SpinLock;
use crate::vm;
use crate::vm::tortie::TortieVM;

#[repr(C)]
pub struct HeapObject {
    pub reference_count: AtomicUsize,
    pub state: AtomicU8,
    pub gc_release: AtomicBool,
    pub is_cyclic_type: bool,
    pub lock_flag: AtomicBool,
    pub field_length: usize,
    pub type_info: *const ObjectType
}

#[inline(always)]
pub unsafe fn object_lock(object: *mut HeapObject) {
    loop {
        if !(*object).lock_flag.swap(true, Ordering::Acquire) {
            break
        }
        while (*object).lock_flag.load(Ordering::Relaxed) {}
    }
}

#[inline(always)]
pub unsafe fn object_unlock(object: *mut HeapObject) {
    (*object).lock_flag.store(false, Ordering::Release);
}


pub struct HeapAllocator {
    pub virtual_machine: *mut TortieVM,
    pub number_of_chunks: usize,
    pub chunks: *mut [*mut HeapChunk],
    pub lock: SpinLock,
    pub chunks_cells_size: usize
}

pub struct HeapChunk {
    pub cells_size: usize,
    pub block_info_list: *mut [BlockInfo]
}

pub struct BlockInfo {
    pub entry_position: *mut u8,
    pub empty: bool,
    pub current_location: usize
}