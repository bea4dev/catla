use std::{hint, mem};
use std::ptr::{null, null_mut};
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicUsize, Ordering};
use crate::vm::module;
use crate::vm::module::object_type::ObjectType;
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

pub const OBJECT_STATE_DEAD: u8 = 0;
pub const OBJECT_STATE_LIVE: u8 = 1;
pub const OBJECT_STATE_PROCESS_GC: u8 = 2;
pub const OBJECT_STATE_WAITING_FOR_GC: u8 = 3;



#[inline(always)]
pub unsafe fn object_lock(object: *mut HeapObject) {
    loop {
        if !(*object).lock_flag.swap(true, Ordering::Acquire) {
            break
        }
        while (*object).lock_flag.load(Ordering::Relaxed) {
            hint::spin_loop();
        }
    }
}

#[inline(always)]
pub unsafe fn object_unlock(object: *mut HeapObject) {
    (*object).lock_flag.store(false, Ordering::Release);
}


pub struct HeapAllocator {
    pub virtual_machine: *mut TortieVM,
    pub chunks: Vec<HeapChunk>,
    pub lock: SpinLock,
    pub chunk_cells_size: usize
}

pub struct HeapChunk {
    pub cells_size: usize,
    pub block_info_list: [BlockInfo; 13]
}

pub struct BlockInfo {
    pub entry_position_ptr: usize,
    pub empty: bool,
    pub current_position: usize
}


impl BlockInfo {

    fn new(entry_position_ptr: usize, empty: bool, current_position: usize) -> Self {
        return Self {
            entry_position_ptr,
            empty,
            current_position
        }
    }

}


const BLOCK_SIZE0: usize = 32;
const BLOCK_SIZE1: usize = 40;
const BLOCK_SIZE2: usize = 48;
const BLOCK_SIZE3: usize = 56;
const BLOCK_SIZE4: usize = 64;
const BLOCK_SIZE5: usize = 72;
const BLOCK_SIZE6: usize = 80;
const BLOCK_SIZE7: usize = 88;
const BLOCK_SIZE8: usize = 96;
const BLOCK_SIZE9: usize = 128;
const BLOCK_SIZE10: usize = 256;
const BLOCK_SIZE11: usize = 384;
const BLOCK_SIZE12: usize = 512;

const NUMBER_OF_BLOCKS: usize = 13;
const BLOCKS_SIZE: usize = 1856;




impl HeapAllocator {

    pub unsafe fn new(virtual_machine: *mut TortieVM,
                      chunk_cells_size: usize, number_of_chunks: usize) -> *mut HeapAllocator {
        let mut boxed = Box::new(Self {
            virtual_machine,
            chunks: Vec::new(),
            lock: SpinLock::new(),
            chunk_cells_size
        });
        for i in 0..number_of_chunks {
            boxed.create_new_chunk(chunk_cells_size);
        }
        return Box::into_raw(boxed);
    }


    pub unsafe fn malloc(&mut self, object_type: *mut ObjectType,
                  field_length: usize, chunk_search_start_index: &mut usize) -> *mut HeapObject {
        let byte_size = mem::size_of::<HeapObject>() + (field_length << 3);

        let mut index;
        let mut block_size;
        if byte_size < BLOCK_SIZE8 {
            if byte_size <= BLOCK_SIZE0 {
                index = 0;
            } else {
                let temp = byte_size - BLOCK_SIZE0;
                if (temp & 0b111) == 0 {
                    index = temp >> 3;
                } else {
                    index = (temp >> 3) + 1;
                }
            }
            block_size = BLOCK_SIZE0 + (index << 3);
        } else if byte_size <= BLOCK_SIZE12 {
            let mut index_temp;
            if byte_size <= BLOCK_SIZE9 {
                index_temp = 0;
            } else {
                let temp = byte_size - BLOCK_SIZE9;
                if (temp & 0b1111111) == 0 {
                    index_temp = temp >> 7;
                } else {
                    index_temp = (temp >> 7) + 1;
                }
            }
            block_size = BLOCK_SIZE9 + (index_temp << 7);
            index = index_temp + 9;
        } else {
            return null_mut();
        }

        let mut current_chunk_index = *chunk_search_start_index;
        let mut chunk_list_size = self.chunks.len();
        loop {
            let mut i: usize = 0;
            loop {
                if i == chunk_list_size {
                    self.create_new_chunk(self.chunk_cells_size);
                    chunk_list_size = self.chunks.len();
                    break;
                }

                let chunk = &mut self.chunks[current_chunk_index];

                let object = chunk.malloc(object_type, index, block_size, field_length);
                if object != null_mut() {
                    *chunk_search_start_index = current_chunk_index;
                    return object;
                }

                current_chunk_index += 1;
                if current_chunk_index == chunk_list_size {
                    current_chunk_index = 0;
                }

                i += 1;
            }
        }
    }


    unsafe fn create_new_chunk(&mut self, cells_size: usize) {
        let chunk = HeapChunk::new(cells_size);
        self.chunks.push(chunk);
    }

}




impl HeapChunk {

    pub unsafe fn new(cells_size: usize) -> Self {
        let position0 = libc::calloc(1, cells_size * BLOCKS_SIZE) as usize;

        let position1 = position0 + BLOCK_SIZE0 * cells_size;
        let position2 = position1 + BLOCK_SIZE1 * cells_size;
        let position3 = position2 + BLOCK_SIZE2 * cells_size;
        let position4 = position3 + BLOCK_SIZE3 * cells_size;
        let position5 = position4 + BLOCK_SIZE4 * cells_size;
        let position6 = position5 + BLOCK_SIZE5 * cells_size;
        let position7 = position6 + BLOCK_SIZE6 * cells_size;
        let position8 = position7 + BLOCK_SIZE7 * cells_size;
        let position9 = position8 + BLOCK_SIZE8 * cells_size;
        let position10 = position9 + BLOCK_SIZE9 * cells_size;
        let position11 = position10 + BLOCK_SIZE10 * cells_size;
        let position12 = position11 + BLOCK_SIZE11 * cells_size;

        let block_info_list: [BlockInfo; 13] = [
            BlockInfo::new(position0, true, 0),
            BlockInfo::new(position1, true, 0),
            BlockInfo::new(position2, true, 0),
            BlockInfo::new(position3, true, 0),
            BlockInfo::new(position4, true, 0),
            BlockInfo::new(position5, true, 0),
            BlockInfo::new(position6, true, 0),
            BlockInfo::new(position7, true, 0),
            BlockInfo::new(position8, true, 0),
            BlockInfo::new(position9, true, 0),
            BlockInfo::new(position10, true, 0),
            BlockInfo::new(position11, true, 0),
            BlockInfo::new(position12, true, 0),
        ];

        return Self {
            cells_size,
            block_info_list
        };
    }


    pub unsafe fn malloc(&mut self, object_type: *mut ObjectType,
                         mut index: usize, mut block_size: usize, field_length: usize) -> *mut HeapObject {
        let cells_size_ = self.cells_size;

        loop {
            let block_info = &mut self.block_info_list[index];

            if !block_info.empty {
                index += 1;
                if block_size < BLOCK_SIZE8 {
                    block_size += 8;
                } else if block_size == BLOCK_SIZE8 {
                    block_size = BLOCK_SIZE9;
                } else if block_size <= BLOCK_SIZE12 {
                    block_size += 128;
                } else {
                    return null_mut();
                }
                if index == 13 {
                    return null_mut();
                }
                continue;
            }

            let block_entry_ptr = block_info.entry_position_ptr;
            let mut current_entry_index = block_info.current_position;
            let mut current_entry_ptr = block_entry_ptr + current_entry_index * block_size;

            for i in 0..cells_size_ {
                let object = current_entry_ptr as *mut HeapObject;
                if (*object).state.load(Ordering::Acquire) != OBJECT_STATE_DEAD {
                    current_entry_ptr += block_size;
                    current_entry_index += 1;
                    if current_entry_index == cells_size_ {
                        current_entry_ptr = block_entry_ptr;
                        current_entry_index = 0;
                    }
                    continue;
                }

                current_entry_index += 1;
                if current_entry_index == cells_size_ {
                    current_entry_index = 0;
                }
                block_info.current_position = current_entry_index;

                let fields = (object as usize) + mem::size_of::<HeapObject>();
                for j in 0..field_length {
                    *((fields + j * mem::size_of::<u64>()) as *mut u64) = 0;
                }
                (*object).type_info = object_type;
                (*object).field_length = field_length;
                (*object).gc_release.store(false, Ordering::Relaxed);
                (*object).state.store(OBJECT_STATE_LIVE, Ordering::Relaxed);
                (*object).is_cyclic_type = (*object_type).is_cyclic.load(Ordering::Relaxed);
                (*object).reference_count.store(1, Ordering::Release);

                return object;
            }
            block_info.current_position = current_entry_index;
            block_info.empty = false;

            index += 1;
            if block_size < BLOCK_SIZE8 {
                block_size += 8;
            } else if block_size == BLOCK_SIZE8 {
                block_size = BLOCK_SIZE9;
            } else if block_size <= BLOCK_SIZE12 {
                block_size += 128;
            } else {
                return null_mut();
            }
            if index == 13 {
                return null_mut();
            }
        }
    }

}