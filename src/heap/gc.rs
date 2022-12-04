use std::collections::HashSet;
use std::sync::Mutex;
use crate::{HeapObject, SpinLock};
use crate::vm::tortie::TortieVM;

struct CycleCollector {
    virtual_machine: *mut TortieVM,
    suspected_object_list: HashSet<*mut HeapObject>,
    list_lock: SpinLock,
    collector_lock: Mutex<()>,
}