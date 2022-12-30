use std::collections::HashMap;
use std::ptr::null_mut;
use std::sync::atomic::AtomicBool;
use crate::vm::module::parser::{FieldInfo, TypeDefineInfo, TypeInfo};

pub struct ObjectType {
    pub name: String,
    pub is_cyclic: AtomicBool,
    pub field_info_map: HashMap<String, FieldInfo>,
    pub field_map: HashMap<String, Type>,
    pub extends_type_info: Option<TypeInfo>,
    pub extends_type: *mut ObjectType
}

impl ObjectType {

    pub unsafe fn new(info: TypeDefineInfo) -> *mut Self {
        let boxed = Box::new(Self {
            name: info.type_name,
            is_cyclic: AtomicBool::new(false),
            field_info_map: HashMap::new(),
            field_map: HashMap::new(),
            extends_type_info: info.extends_type_info.clone(),
            extends_type: null_mut()
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
    Void,
    ObjectReference(*mut ObjectType, bool)
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
