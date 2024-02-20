use std::{collections::HashMap, sync::Arc};

use crate::transpiler::component::RawEntityID;


#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Type {
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
    DataStruct(Option<Arc<DataStructInfo>>),
    Function(Arc<FunctionType>),
    Generic(Arc<GenericType>),
    Unknown
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct DataStructInfo {
    module_name: String,
    path: Vec<String>,
    name: String,
    kind: DataStructKind,
    generics_define: Vec<GenericType>,
    element_types: HashMap<String, Type>
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum DataStructKind {
    Struct,
    Class,
    Interface
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct GenericType {
    define_entity_id: RawEntityID,
    bounds: Vec<Type>
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct FunctionType {
    module_name: String,
    path: Vec<String>,
    name: String,
    generics_define: Vec<GenericType>,
    argument_types: Vec<Type>,
    return_type: Vec<Type>,
}