use crate::{borrow::Borrow, object::CatlaObjectRef};

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
        string: CatlaObjectRef<std::string::String>,
    },
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
                string: CatlaObjectRef::heap(string, mutex, true),
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
