use std::{ops::Range, sync::Arc};

use crate::type_infer::TypeVariableID;

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
    NumericLiteral(Arc<Vec<Type>>),
    UserType {
        user_type_info: Arc<UserTypeInfo>,
        generics: Arc<Vec<Type>>,
    },
    Function {
        function_info: Arc<FunctionTypeInfo>,
        generics: Arc<Vec<Type>>,
    },
    Generic(Arc<GenericType>),
    TypeVariable(TypeVariableID),
    Array(Arc<Type>),
    Tuple(Arc<Vec<Type>>),
    Option(Arc<Type>),
    Result {
        value: Arc<Type>,
        error: Arc<Type>,
    },
    This,
    Unreachable,
    Unknown,
}

#[derive(Debug, PartialEq, Eq)]
pub struct UserTypeInfo {}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionTypeInfo {}

#[derive(Debug, PartialEq, Eq)]
pub struct GenericType {}

