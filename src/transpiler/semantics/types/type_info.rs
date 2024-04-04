use std::{collections::HashMap, ops::Range, sync::{Arc, Mutex}};

use catla_parser::parser::{DataStructKindEnum, Spanned};

use crate::transpiler::component::EntityID;


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
    Bool,
    Unit,
    UserType { user_type_info: Arc<DataStructInfo>, generics: Arc<Vec<Type>> },
    Function{ function_info: Arc<FunctionType>, generics: Arc<Vec<Type>> },
    Generic(Arc<GenericType>),
    LocalGeneric(LocalGenericID),
    Option(Arc<Type>),
    Result { value: Arc<Type>, error: Arc<Type> },
    Unknown
}

pub static PRIMITIVE_TYPE_NAMES: &[&str] = &[
    "int",
    "int8",
    "int16",
    "int32",
    "int64",
    "uint",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
    "float",
    "float32",
    "float64",
    "bool",
    "unit"
];


#[derive(Debug)]
pub struct DataStructInfo {
    pub module_name: String,
    pub name: Spanned<String>,
    pub define_span: Range<usize>,
    pub kind: DataStructKindEnum,
    pub generics_define: Vec<Arc<GenericType>>,
    pub element_types: Mutex<HashMap<String, Type>>
}

impl PartialEq for DataStructInfo {
    fn eq(&self, other: &Self) -> bool {
        self.module_name == other.module_name && self.name == other.name
    }
}
impl Eq for DataStructInfo {}

#[derive(Debug)]
pub struct GenericType {
    pub(crate) define_entity_id: EntityID,
    pub name: String,
    pub bounds: Mutex<Vec<Type>>
}

impl PartialEq for GenericType {
    fn eq(&self, other: &Self) -> bool {
        self.define_entity_id == other.define_entity_id
    }
}
impl Eq for GenericType {}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionType {
    pub is_extension: bool,
    pub generics_define: Vec<Arc<GenericType>>,
    pub argument_types: Vec<Type>,
    pub return_type: Spanned<Type>
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalGenericID(pub usize);