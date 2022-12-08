use std::sync::atomic::AtomicBool;

#[repr(C)]
pub struct ObjectType {
    pub is_cyclic: AtomicBool
}

impl ObjectType {

    pub unsafe fn new(is_cyclic: bool) -> *mut Self {
        let boxed = Box::new(Self {
            is_cyclic: AtomicBool::new(is_cyclic)
        });
        return Box::into_raw(boxed);
    }

}