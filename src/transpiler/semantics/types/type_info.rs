use std::{collections::HashMap, ops::Range, sync::Arc};

use catla_parser::parser::{DataStructKindEnum, Spanned};

use crate::transpiler::component::AnyEntityID;


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
    DataStruct(Option<Arc<DataStructInfo>>),
    Function(Arc<FunctionType>),
    Generic(Arc<GenericType>),
    Unknown
}

#[derive(Debug, PartialEq, Eq)]
pub struct DataStructInfo {
    pub module_name: String,
    pub name: Spanned<String>,
    pub define_span: Range<usize>,
    pub kind: DataStructKindEnum,
    pub generics_define: Vec<Arc<GenericType>>,
    pub element_types: HashMap<String, Type>
}

#[derive(Debug, PartialEq, Eq)]
pub struct GenericType {
    pub(crate) define_entity_id: AnyEntityID,
    pub bounds: Vec<Type>
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionType {
    pub module_name: String,
    pub name: Spanned<String>,
    pub define_span: Range<usize>,
    pub extension_type: Option<Type>,
    pub generics_define: Vec<Arc<GenericType>>,
    pub argument_types: Vec<Type>,
    pub return_type: Vec<Type>,
}