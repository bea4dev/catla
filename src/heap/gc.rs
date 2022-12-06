use std::collections::{HashMap, HashSet};
use std::mem;
use std::ptr::{null, null_mut};
use std::sync::atomic::{fence, Ordering};
use std::sync::Mutex;
use libc::printf;
use crate::{HeapObject, object_lock, object_unlock, SpinLock};
use crate::heap::allocator::{OBJECT_STATE_DEAD, OBJECT_STATE_LIVE, OBJECT_STATE_WAITING_FOR_GC};
use crate::vm::tortie::TortieVM;

pub struct CycleCollector {
    virtual_machine: *mut TortieVM,
    suspected_object_list: HashSet<*mut HeapObject>,
    list_lock: SpinLock,
    collector_lock: Mutex<()>,
}

const OBJECT_COLOR_RED: u8 = 0;
const OBJECT_COLOR_GRAY: u8 = 1;
const OBJECT_COLOR_WHITE: u8 = 2;
const OBJECT_COLOR_BLACK: u8 = 3;

impl CycleCollector {

    pub fn new(virtual_machine: *mut TortieVM) -> *mut Self {
        let boxed = Box::new(Self {
            virtual_machine,
            suspected_object_list: HashSet::new(),
            list_lock: SpinLock::new(),
            collector_lock: Mutex::new(())
        });
        return Box::into_raw(boxed);
    }

    #[inline(always)]
    pub fn add_suspected_object(&mut self, object: *mut HeapObject) {
        self.list_lock.lock();
        self.suspected_object_list.insert(object);
        self.list_lock.unlock();
    }

    pub unsafe fn gc_collect(&mut self) {
        //Lock to limit single thread
        let _ = self.collector_lock.lock().expect("Failed to lock.");

        //Swap list
        self.list_lock.lock();
        let mut roots: HashSet<*mut HeapObject> = HashSet::new();
        mem::swap(&mut roots, &mut self.suspected_object_list);
        self.list_lock.unlock();

        let mut release_objects: HashSet<*mut HeapObject> = HashSet::new();

        for root in roots.iter() {
            let mut check_objects: Vec<*mut HeapObject> = Vec::new();
            let mut color_map: HashMap<*mut HeapObject, u8> = HashMap::new();
            let mut count_map: HashMap<*mut HeapObject, usize> = HashMap::new();
            let mut cycle_objects: Vec<*mut HeapObject> = Vec::new();
            let mut current_object = *root;
            let mut is_cyclic_root = false;

            //Mark red (Lock phase)
            color_map.insert(*root, OBJECT_COLOR_RED);
            loop {
                //Lock
                object_lock(current_object);
                cycle_objects.push(current_object);

                //Get field objects
                let fields_ptr = (current_object as usize) + mem::size_of::<HeapObject>();
                let field_length = (*current_object).field_length;
                for i in 0..field_length {
                    let field_object = *((fields_ptr + mem::size_of::<u64>() * i) as *mut *mut HeapObject);
                    if field_object != null_mut() {
                        if color_map.contains_key(&field_object) {
                            if field_object == *root {
                                is_cyclic_root = true;
                            }
                        } else {
                            color_map.insert(field_object, OBJECT_COLOR_RED);
                            check_objects.push(field_object);
                        }
                    }
                }

                //Update current object
                if check_objects.is_empty() {
                    break;
                }
                current_object = check_objects.pop().unwrap();
            }


            if is_cyclic_root {
                //Mark gray
                count_map.insert(*root, (**root).reference_count.load(Ordering::Acquire));
                color_map.insert(*root, OBJECT_COLOR_GRAY);
                current_object = *root;

                loop {
                    //Get field objects
                    let fields_ptr = (current_object as usize) + mem::size_of::<HeapObject>();
                    let field_length = (*current_object).field_length;

                    for i in 0..field_length {
                        let field_object = *((fields_ptr + mem::size_of::<u64>() * i) as *mut *mut HeapObject);
                        if field_object != null_mut() {
                            if color_map[&field_object] == OBJECT_COLOR_GRAY {
                                let previous_rc = count_map[&field_object];
                                if previous_rc > 0 {
                                    count_map.insert(field_object, previous_rc - 1);
                                }
                            } else {
                                color_map.insert(field_object, OBJECT_COLOR_GRAY);
                                check_objects.push(field_object);
                                count_map.insert(field_object, (*field_object).reference_count.load(Ordering::Acquire) - 1);
                            }
                        }
                    }

                    //Update current object
                    if check_objects.is_empty() {
                        break;
                    }
                    current_object = check_objects.pop().unwrap();
                }


                //Mark black or white
                current_object = *root;
                if count_map[root] == 0 {
                    color_map.insert(*root, OBJECT_COLOR_WHITE);
                } else {
                    color_map.insert(*root, OBJECT_COLOR_BLACK);
                }

                loop {
                    let current_object_color = color_map[&current_object];

                    //Get field objects
                    let fields_ptr = (current_object as usize) + mem::size_of::<HeapObject>();
                    let field_length = (*current_object).field_length;

                    for i in 0..field_length {
                        let field_object = *((fields_ptr + mem::size_of::<u64>() * i) as *mut *mut HeapObject);
                        if field_object != null_mut() {
                            let field_object_color = color_map[&field_object];
                            let field_object_rc = count_map[&field_object];

                            if current_object_color == OBJECT_COLOR_WHITE {
                                if field_object_rc == 0 {
                                    if field_object_color == OBJECT_COLOR_GRAY {
                                        color_map.insert(field_object, OBJECT_COLOR_WHITE);
                                        check_objects.push(field_object);
                                    }
                                } else {
                                    if field_object_color != OBJECT_COLOR_BLACK {
                                        check_objects.push(field_object);
                                        color_map.insert(field_object, OBJECT_COLOR_BLACK);
                                    }
                                }
                            } else if current_object_color == OBJECT_COLOR_BLACK {
                                count_map.insert(field_object, field_object_rc + 1);
                                if field_object_color != OBJECT_COLOR_BLACK {
                                    color_map.insert(field_object, OBJECT_COLOR_BLACK);
                                    check_objects.push(field_object);
                                }
                            }
                        }
                    }

                    if check_objects.is_empty() {
                        break;
                    }
                    current_object = check_objects.pop().unwrap();
                }


                //Collect white
                for object in cycle_objects.iter() {
                    if color_map[object] == OBJECT_COLOR_WHITE {
                        (**object).state.store(OBJECT_STATE_WAITING_FOR_GC, Ordering::Relaxed);
                        release_objects.insert(*object);
                    }
                }

                for object in cycle_objects.iter() {
                    object_unlock(*object);
                }
            } else {
                for object in cycle_objects.iter() {
                    object_unlock(*object);
                }

                let mut non_cyclic_objects: HashSet<*mut HeapObject> = HashSet::new();
                let mut current_object = *root;
                let mut ready_to_release = false;

                loop {
                    if (*current_object).state.load(Ordering::Acquire) != OBJECT_STATE_WAITING_FOR_GC {
                        ready_to_release = false;
                        break;
                    }
                    non_cyclic_objects.insert(current_object);

                    let fields_ptr = (current_object as usize) + mem::size_of::<HeapObject>();
                    let field_length = (*current_object).field_length;
                    for i in 0..field_length {
                        let field_object = *((fields_ptr + mem::size_of::<u64>() * i) as *mut *mut HeapObject);
                        if field_object != null_mut() && non_cyclic_objects.contains(&field_object) {
                            check_objects.push(field_object);
                        }
                    }

                    if check_objects.is_empty() {
                        break;
                    }
                    current_object = check_objects.pop().unwrap();
                }

                if ready_to_release {
                    for object in non_cyclic_objects.iter() {
                        release_objects.insert(*object);
                    }
                }
            }
        }

        for object in release_objects.iter() {
            roots.remove(object);
            if (**object).is_cyclic_type && (**object).gc_release.load(Ordering::Acquire) {
                self.list_lock.lock();
                self.suspected_object_list.remove(object);
                self.list_lock.unlock();
            }

            let fields_ptr = (*object as usize) + mem::size_of::<HeapObject>();
            let field_length = (**object).field_length;
            for i in 0..field_length {
                let field_object = *((fields_ptr + mem::size_of::<u64>() * i) as *mut *mut HeapObject);
                if field_object != null_mut() {
                    if (*field_object).state.load(Ordering::Acquire) == OBJECT_STATE_LIVE {
                        decrement_reference_count(field_object);
                    }
                }
            }
        }

        for object in release_objects.iter() {
            (**object).state.store(OBJECT_STATE_DEAD, Ordering::Release);
        }

        for root in roots.iter() {
            self.add_suspected_object(*root);
        }

    }

}


#[inline(always)]
pub unsafe fn increment_reference_count(cycle_collector: *mut CycleCollector, object: *mut HeapObject) {
    let previous_count = (*object).reference_count.fetch_add(1, Ordering::Relaxed);
    if previous_count == 1 && (*object).is_cyclic_type {
        if !(*object).gc_release.swap(true, Ordering::Relaxed) {
            (*cycle_collector).add_suspected_object(object);
        }
    }
}

#[inline(always)]
pub unsafe fn decrement_reference_count(object: *mut HeapObject) {
    let previous_count = (*object).reference_count.fetch_sub(1, Ordering::Release);
    fence(Ordering::Acquire);

    if previous_count == 1 {
        if (*object).is_cyclic_type && (*object).gc_release.load(Ordering::Relaxed) {
            decrement_reference_count_waiting_gc_object(object);
            return;
        }
    } else {
        return;
    }

    let mut check_objects: Vec<*mut HeapObject> = Vec::new();
    let mut current_object = object;

    loop {
        let fields_ptr = (current_object as usize) + mem::size_of::<HeapObject>();
        let field_length = (*current_object).field_length;

        for i in 0..field_length {
            let field_object = *((fields_ptr + mem::size_of::<u64>() * i) as *mut *mut HeapObject);
            if field_object != null_mut() {
                let field_object_previous_rc = (*field_object).reference_count.fetch_sub(1, Ordering::Release);
                fence(Ordering::Acquire);

                if field_object_previous_rc == 1 {
                    if (*field_object).is_cyclic_type && (*field_object).gc_release.load(Ordering::Relaxed) {
                        decrement_reference_count_waiting_gc_object(field_object);
                        continue;
                    } else {
                        check_objects.push(field_object);
                    }
                }
            }
        }

        (*current_object).state.store(OBJECT_STATE_DEAD, Ordering::Release);
        if check_objects.is_empty() {
            break;
        }
        current_object = check_objects.pop().unwrap();
    }
}

#[inline(always)]
pub unsafe fn decrement_reference_count_waiting_gc_object(object: *mut HeapObject) {
    let mut current_root = object;
    let mut check_roots: Vec<*mut HeapObject> = Vec::new();

    loop {
        let mut release_objects: Vec<*mut HeapObject> = Vec::new();
        let mut check_objects: Vec<*mut HeapObject> = Vec::new();
        let mut current_object = current_root;

        loop {
            release_objects.push(current_object);
            (*current_object).state.store(OBJECT_STATE_WAITING_FOR_GC, Ordering::Release);
            fence(Ordering::Acquire);

            let fields_ptr = (current_object as usize) + mem::size_of::<HeapObject>();
            let field_length = (*current_object).field_length;

            for i in 0..field_length {
                let field_ptr = (fields_ptr + mem::size_of::<u64>() * i) as *mut *mut HeapObject;
                let field_object = *field_ptr;
                if field_object != null_mut() {
                    let field_object_previous_rc = (*field_object).reference_count.fetch_sub(1, Ordering::Release);
                    fence(Ordering::Acquire);

                    if field_object_previous_rc == 1 {
                        if (*field_object).is_cyclic_type && (*field_object).gc_release.load(Ordering::Relaxed) {
                            *field_ptr = null_mut();
                            check_roots.push(field_object);
                        } else {
                            check_objects.push(field_object);
                        }
                    } else {
                        *field_ptr = null_mut();
                    }
                }
            }

            if check_objects.is_empty() {
                break;
            }
            current_object = check_objects.pop().unwrap();
        }

        for object in release_objects.iter() {
            (**object).state.store(OBJECT_STATE_WAITING_FOR_GC, Ordering::Release);
        }

        if check_roots.is_empty() {
            break;
        }
        current_root = check_roots.pop().unwrap();
    }
}