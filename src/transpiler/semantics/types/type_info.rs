use std::{collections::HashMap, ops::Range, sync::{Arc, Mutex}};

use catla_parser::parser::{DataStructKindEnum, Spanned};
use derivative::Derivative;

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

impl Type {
    
    fn get_type_with_generics(ty: &Type, generics_define: &Vec<Arc<GenericType>>, local_generics: &Arc<Vec<Type>>) -> Type {
        match ty {
            Type::UserType { user_type_info, generics } => {
                let mut new_generics = Vec::with_capacity(generics.len());
                for generic in generics.iter() {
                    let ty = Type::get_type_with_generics(generic, generics_define, local_generics);
                    new_generics.push(ty);
                }

                Type::UserType { user_type_info: user_type_info.clone(), generics: Arc::new(new_generics) }
            },
            Type::Function { function_info, generics } => {
                let mut new_generics = Vec::with_capacity(generics.len());
                for generic in generics.iter() {
                    let ty = Type::get_type_with_generics(generic, generics_define, local_generics);
                    new_generics.push(ty);
                }

                let mut argument_types = Vec::with_capacity(function_info.argument_types.len());
                for argument_type in function_info.argument_types.iter() {
                    let ty = Type::get_type_with_generics(argument_type, generics_define, local_generics);
                    argument_types.push(ty);
                }

                let return_type = Type::get_type_with_generics(
                    &function_info.return_type.value,
                    generics_define,
                    local_generics
                );

                let function_info = FunctionType {
                    is_extension: function_info.is_extension,
                    generics_define: function_info.generics_define.clone(),
                    argument_types,
                    return_type: Spanned::new(return_type, function_info.return_type.span.clone()),
                    define_info: function_info.define_info.clone()
                };

                Type::Function { function_info: Arc::new(function_info), generics: Arc::new(new_generics) }
            },
            Type::Generic(generic) => {
                // resolve generic id, if local generic exists
                match generics_define.iter().position(|element| { element == generic }) {
                    Some(index) => local_generics.get(index).cloned().unwrap_or(ty.clone()),
                    _ => ty.clone()
                }
            },
            Type::Option(value_type) => {
                let new_value_type = Type::get_type_with_generics(&value_type, generics_define, local_generics);
                Type::Option(Arc::new(new_value_type))
            },
            Type::Result { value, error } => {
                let new_value_type = Type::get_type_with_generics(value, generics_define, local_generics);
                let new_error_type = Type::get_type_with_generics(error, generics_define, local_generics);

                Type::Result { value: Arc::new(new_value_type), error: Arc::new(new_error_type) }
            },
            _ => ty.clone()
        }
    }

    pub(crate) fn get_element_type_with_local_generic(&self, name: &str) -> Option<Type> {
        match self {
            Type::UserType { user_type_info, generics } => {
                let element_type = {
                    let element_type_map = user_type_info.element_types.lock().unwrap();
                    element_type_map.get(name).cloned()
                };

                element_type.map(|ty| { Type::get_type_with_generics(&ty, &user_type_info.generics_define, generics) })
            },
            _ => None
        }
    }

    pub fn is_option_or_result(&self) -> bool {
        match self {
            Type::Option(_) => true,
            Type::Result { value: _, error: _ } => true,
            _ => false
        }
    }

    pub(crate) fn get_indexed_element_type_with_local_generic(&self, index: usize) -> Option<Type> {
        match self {
            Type::Function { function_info, generics } => {
                let element_type = function_info.argument_types.get(index);

                element_type.map(|ty| { Type::get_type_with_generics(ty, &function_info.generics_define, generics) })
            },
            _ => None
        }
    }

    pub(crate) fn get_return_type_with_local_generic(&self) -> Option<Type> {
        match self {
            Type::Function { function_info, generics } => {
                let return_type = &function_info.return_type.value;
                Some(Type::get_type_with_generics(return_type, &function_info.generics_define, generics))
            },
            _ => None
        }
    }

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

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Eq)]
pub struct FunctionType {
    pub is_extension: bool,
    pub generics_define: Vec<Arc<GenericType>>,
    pub argument_types: Vec<Type>,
    pub return_type: Spanned<Type>,
    #[derivative(PartialEq="ignore")]
    pub define_info: FunctionDefineInfo
}

#[derive(Debug, Clone)]
pub struct FunctionDefineInfo {
    pub module_name: String,
    pub arguments_span: Range<usize>,
    pub span: Range<usize>
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalGenericID(pub usize);


pub struct ImplementsInfo {
    pub generics: Vec<Arc<GenericType>>,
    pub interface: Type,
    pub concrete: Type
}