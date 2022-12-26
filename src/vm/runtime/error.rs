use crate::HeapObject;

pub enum RuntimeException {
    NullPointerException(String),
    UserCustomException(*mut HeapObject)
}