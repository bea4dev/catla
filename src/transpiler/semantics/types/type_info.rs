use std::{collections::HashMap, mem::swap, ops::Range, sync::{Arc, Mutex, MutexGuard, PoisonError}};

use catla_parser::parser::{DataStructKindEnum, Spanned};
use derivative::Derivative;
use either::Either;

use crate::transpiler::component::EntityID;

use super::type_inference::TypeEnvironment;


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
pub struct FreezableMutex<T: Default> {
    mutex: Mutex<Either<T, Arc<T>>>
}

impl<T: Default> FreezableMutex<T> {
    
    pub fn new(value: T) -> Self {
        Self {
            mutex: Mutex::new(Either::Left(value))
        }
    }

    pub fn lock(&self) -> Result<MutexGuard<Either<T, Arc<T>>>, PoisonError<MutexGuard<Either<T, Arc<T>>>>> {
        self.mutex.lock()
    }

    pub fn freeze_and_get(&self) -> Arc<T> {
        let mut lock = self.mutex.lock().unwrap();

        if lock.is_left() {
            let arc = if let Either::Left(value) = lock.as_mut() {
                let mut temp = T::default();
                swap(value, &mut temp);
                Arc::new(temp)
            } else {
                unreachable!()
            };

            *lock = Either::Right(arc.clone());

            arc
        } else {
            lock.as_mut().right().unwrap().clone()
        }
    }

}


#[derive(Debug)]
pub struct GenericType {
    pub(crate) define_entity_id: EntityID,
    pub name: String,
    pub bounds: FreezableMutex<Vec<Spanned<Type>>>
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


#[derive(Debug, Clone)]
pub struct ImplementsInfo {
    pub generics: Arc<Vec<Arc<GenericType>>>,
    pub interface: Type,
    pub concrete: Type
}

impl ImplementsInfo {
    
    fn contains_target_type(
        self_type: &Type,
        ty: &Type,
        lacked_bounds: &mut Vec<Spanned<Type>>,
        collect_lacked_bounds: bool
    ) -> bool {
        match self_type {
            Type::Int8 => ty == &Type::Int8,
            Type::Int16 => ty == &Type::Int16,
            Type::Int32 => ty == &Type::Int32,
            Type::Int64 => ty == &Type::Int64,
            Type::Uint8 => ty == &Type::Uint8,
            Type::Uint16 => ty == &Type::Uint16,
            Type::Uint32 => ty == &Type::Uint32,
            Type::Uint64 => ty == &Type::Uint64,
            Type::Float32 => ty == &Type::Float32,
            Type::Float64 => ty == &Type::Float64,
            Type::Bool => ty == &Type::Bool,
            Type::Unit => ty == &Type::Unit,
            Type::UserType { user_type_info: self_user_type_info, generics: self_generics } => {
                if let Type::UserType { user_type_info, generics } = ty {
                    if self_user_type_info != user_type_info {
                        return false;
                    }

                    
                } else {
                    false
                }
            },
            Type::Function { function_info, generics } => todo!(),
            Type::Generic(_) => todo!(),
            Type::LocalGeneric(_) => todo!(),
            Type::Option(_) => todo!(),
            Type::Result { value, error } => todo!(),
            Type::Unknown => todo!(),
        }
    }

    fn contains_target_generic_types(
        self_generics: &Vec<Arc<GenericType>>,
        generics: &Vec<Arc<GenericType>>,
        lacked_bounds: &mut Vec<Spanned<Type>>,
        collect_lacked_bounds: bool
    ) -> bool {
        if self_generics.len() != generics.len() {
            return false;
        }

        let mut contains = true;
        for i in 0..self_generics.len() {
            let self_generic = self_generics[i].as_ref();
            let generic = generics[i].as_ref();

            if !ImplementsInfo::contains_target_generic_type(
                self_generic,
                generic,
                lacked_bounds,
                collect_lacked_bounds
            ) {
                contains = false;
            }
        }

        contains
    }

    fn contains_target_generic_type(
        self_generic: &GenericType,
        generic: &GenericType,
        lacked_bounds: &mut Vec<Spanned<Type>>,
        collect_lacked_bounds: bool
    ) -> bool {
        let mut is_satisfy = true;

        for self_bound in self_generic.bounds.freeze_and_get().iter() {
            for bound in generic.bounds.freeze_and_get().iter() {
                if !ImplementsInfo::contains_target_type(
                    &self_bound.value,
                    &bound.value,
                    lacked_bounds,
                    collect_lacked_bounds
                ) {
                    is_satisfy = false;
                    if collect_lacked_bounds {
                        lacked_bounds.push(self_bound.clone());
                    }
                }
            }
        }

        is_satisfy
    }

}


pub struct ImplementsInfoSet {
    implements_infos: Vec<ImplementsInfo>
}


impl ImplementsInfoSet {
    
    pub fn new() -> Self {
        Self {
            implements_infos: vec![]
        }
    }

    pub fn add(&mut self, implements_info: ImplementsInfo) {
        self.implements_infos.push(implements_info);
    }

    pub fn merge(&mut self, other: &ImplementsInfoSet) {
        self.implements_infos.extend(other.implements_infos.clone());
    }

    pub fn is_satisfied(&self, ty: &Type, generic_type: &GenericType) -> Result<(), Vec<Spanned<Type>>> {
        
    }

    pub fn is_implemented(&self, ty: &Type, interface: &Type) -> bool {
        self.implements_infos.iter().any(|implements| {
            
        })
    }

}