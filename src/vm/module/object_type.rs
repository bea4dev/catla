use std::collections::HashMap;
use std::sync::atomic::AtomicBool;

pub struct ObjectType {
    pub name: String,
    pub is_cyclic: AtomicBool,
    pub field_map: HashMap<String, Type>
}

impl ObjectType {

    pub unsafe fn new(is_cyclic: bool) -> *mut Self {
        let boxed = Box::new(Self {
            is_cyclic: AtomicBool::new(is_cyclic)
        });
        return Box::into_raw(boxed);
    }

}

#[derive(Clone)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    ObjectReference(*mut ObjectType)
}

impl Type {
    pub fn is_integer(&self) -> bool {
        return match self {
            Type::I8 => true,
            Type::I16 => true,
            Type::I32 => true,
            Type::I64 => true,
            Type::U8 => true,
            Type::U16 => true,
            Type::U32 => true,
            Type::U64 => true,
            _ => false
        }
    }
}