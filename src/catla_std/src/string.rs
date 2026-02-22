use std::cell::UnsafeCell;

use crate::{
    borrow::Borrow,
    dispose::Drop,
    object::{CatlaObject, CatlaObjectRef, init_count_and_flags},
};

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct String {
    pub inner: StringInner,
}

#[derive(Debug, Clone)]
pub enum StringInner {
    Static {
        str: &'static str,
    },
    Heap {
        string: CatlaObjectRef<StdStringObject>,
    },
}

#[derive(Debug)]
pub struct StdStringObject {
    value: UnsafeCell<std::string::String>,
    count_and_flags: UnsafeCell<usize>,
}

unsafe impl Sync for StdStringObject {}
unsafe impl Send for StdStringObject {}

impl Drop for StdStringObject {
    fn drop(&self) {}
}

impl CatlaObject for StdStringObject {
    type Value = std::string::String;

    fn new(value: Self::Value, mutex: bool, heap: bool, drop: bool) -> Self {
        Self {
            value: UnsafeCell::new(value),
            count_and_flags: UnsafeCell::new(init_count_and_flags(mutex, heap, drop)),
        }
    }

    fn count_and_flags_ptr(&self) -> *mut usize {
        self.count_and_flags.get()
    }

    fn value_ptr(&self) -> *mut Self::Value {
        self.value.get()
    }
}

impl String {
    #[inline(always)]
    pub fn from_static_str(str: &'static str) -> Self {
        Self {
            inner: StringInner::Static { str },
        }
    }

    pub fn from_heap_string(string: std::string::String, mutex: bool) -> Self {
        Self {
            inner: StringInner::Heap {
                string: CatlaObjectRef::<StdStringObject>::heap(string, mutex, true),
            },
        }
    }
}

impl Borrow for String {
    fn borrow(&self) -> Self {
        let inner = match &self.inner {
            StringInner::Static { str } => StringInner::Static { str: *str },
            StringInner::Heap { string } => StringInner::Heap {
                string: string.borrow(),
            },
        };

        String { inner }
    }
}
