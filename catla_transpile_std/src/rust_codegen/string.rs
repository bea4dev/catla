use std::cell::UnsafeCell;

use crate::{drop::CatlaDrop, memory::CatlaRefObject};

pub enum String {
    Static(&'static str),
    NonStatic(UnsafeCell<Box<[u8]>>),
}

impl CatlaDrop for String {
    fn drop(&self) {
        if let Self::NonStatic(buffer) = self {
            unsafe {
                drop(std::ptr::read(buffer.get()));
            }
        }
    }

    fn drop_mutex(&self) {
        if let Self::NonStatic(buffer) = self {
            unsafe {
                drop(std::ptr::read(buffer.get()));
            }
        }
    }
}

impl String {
    pub fn from_str(str: &'static str) -> &'static CatlaRefObject<Self> {
        CatlaRefObject::init_on_heap(Self::Static(str))
    }
}
