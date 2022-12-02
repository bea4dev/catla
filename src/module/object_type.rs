use std::sync::atomic::AtomicBool;

#[repr(C)]
pub struct ObjectType {
    pub is_cyclic: AtomicBool
}