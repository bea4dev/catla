use std::{mem::swap, ops::Range, sync::{Arc, Mutex, MutexGuard, PoisonError}};

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use bumpalo::Bump;
use catla_parser::parser::{Spanned, StatementAttribute, UserTypeKindEnum};
use derivative::Derivative;
use either::Either;
use fxhash::FxHashMap;
use indexmap::IndexMap;

use crate::transpiler::{component::EntityID, context::TranspileModuleContext, error::{ErrorMessageKey, ErrorMessageType, SimpleError, TranspileReport}, TranspileError};

use super::type_inference::{TypeEnvironment, TypeMismatchError};


#[derive(Debug, Clone, Derivative)]
#[derivative(PartialEq, Eq)]
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
    NumericLiteral(Vec<Type>),
    UserType {
        user_type_info: Arc<UserTypeInfo>,
        generics: Arc<Vec<Type>>,
        #[derivative(PartialEq="ignore")]
        generics_span: Option<Arc<Vec<Range<usize>>>>
    },
    Function{ function_info: Arc<FunctionType>, generics: Arc<Vec<Type>> },
    Generic(Arc<GenericType>),
    LocalGeneric(LocalGenericID),
    Array(Arc<Type>),
    Option(Arc<Type>),
    Result { value: Arc<Type>, error: Arc<Type> },
    This,
    Unreachable,
    Unknown
}

impl Type {
    
    pub(crate) fn get_type_with_replaced_generics(ty: &Type, generics_define: &Vec<Arc<GenericType>>, replace_generics: &Vec<Type>) -> Type {
        match ty {
            Type::UserType { user_type_info, generics, generics_span: _ } => {
                let mut new_generics = Vec::with_capacity(generics.len());
                for generic in generics.iter() {
                    let ty = Type::get_type_with_replaced_generics(generic, generics_define, replace_generics);
                    new_generics.push(ty);
                }

                Type::UserType { user_type_info: user_type_info.clone(), generics: Arc::new(new_generics), generics_span: None }
            },
            Type::Function { function_info, generics } => {
                let mut new_generics = Vec::with_capacity(generics.len());
                for generic in generics.iter() {
                    let ty = Type::get_type_with_replaced_generics(generic, generics_define, replace_generics);
                    new_generics.push(ty);
                }

                let mut argument_types = Vec::with_capacity(function_info.argument_types.len());
                for argument_type in function_info.argument_types.iter() {
                    let ty = Type::get_type_with_replaced_generics(argument_type, generics_define, replace_generics);
                    argument_types.push(ty);
                }

                let return_type = Type::get_type_with_replaced_generics(
                    &function_info.return_type.value,
                    generics_define,
                    replace_generics
                );

                let function_info = FunctionType {
                    is_extension: function_info.is_extension,
                    generics_define: function_info.generics_define.clone(),
                    argument_types,
                    return_type: Spanned::new(return_type, function_info.return_type.span.clone()),
                    define_info: function_info.define_info.clone(),
                    where_bounds: function_info.where_bounds.clone()
                };

                Type::Function { function_info: Arc::new(function_info), generics: Arc::new(new_generics) }
            },
            Type::Generic(generic) => {
                // resolve generic id, if local generic exists
                match generics_define.iter().position(|element| { element == generic }) {
                    Some(index) => replace_generics.get(index).cloned().unwrap_or(ty.clone()),
                    _ => ty.clone()
                }
            },
            Type::Array(base_type) => {
                let new_base_type = Type::get_type_with_replaced_generics(&base_type, generics_define, replace_generics);
                Type::Array(Arc::new(new_base_type))
            },
            Type::Option(value_type) => {
                let new_value_type = Type::get_type_with_replaced_generics(&value_type, generics_define, replace_generics);
                Type::Option(Arc::new(new_value_type))
            },
            Type::Result { value, error } => {
                let new_value_type = Type::get_type_with_replaced_generics(value, generics_define, replace_generics);
                let new_error_type = Type::get_type_with_replaced_generics(error, generics_define, replace_generics);

                Type::Result { value: Arc::new(new_value_type), error: Arc::new(new_error_type) }
            },
            _ => ty.clone()
        }
    }
    
    pub(crate) fn replace_this_type(&self, new_this_type: &Type, replace_field_recursive: bool, replace_bounds: bool) -> Self {
        match self {
            Type::UserType { user_type_info, generics, generics_span } => {
                let user_type_info = if replace_field_recursive {
                    let mut element_types = user_type_info.element_types.lock().unwrap().clone();
                    for (_, element_type) in element_types.iter_mut() {
                        *element_type = WithDefineInfo {
                            value: element_type.value.replace_this_type(new_this_type, false, replace_bounds),
                            module_name: element_type.module_name.clone(),
                            span: element_type.span.clone()
                        };
                    }
                    
                    let where_bounds = Type::replace_where_bounds_this_type(
                        &user_type_info.where_bounds.freeze_and_get(),
                        new_this_type
                    );
                    
                    let user_type_info = UserTypeInfo {
                        module_name: user_type_info.module_name.clone(),
                        name: user_type_info.name.clone(),
                        define_span: user_type_info.define_span.clone(),
                        kind: user_type_info.kind,
                        generics_define: Type::replace_generics_define_this_type(&user_type_info.generics_define, new_this_type),
                        generics_define_span: user_type_info.generics_define_span.clone(),
                        element_types: Mutex::new(element_types),
                        element_attributes: user_type_info.element_attributes.clone(),
                        where_bounds: FreezableMutex::new(where_bounds)
                    };
                    
                    Arc::new(user_type_info)
                } else {
                    user_type_info.clone()
                };
                
                let generics = generics.iter()
                    .map(|ty| { ty.replace_this_type(new_this_type, replace_field_recursive, replace_bounds) })
                    .collect();
                
                Type::UserType {
                    user_type_info,
                    generics: Arc::new(generics),
                    generics_span: generics_span.clone()
                }
            },
            Type::Function { function_info, generics } => {
                let argument_types = function_info.argument_types.iter()
                    .map(|ty| { ty.replace_this_type(new_this_type, replace_field_recursive, replace_bounds) })
                    .collect();
                
                let where_bounds = Type::replace_where_bounds_this_type(
                    &function_info.where_bounds.freeze_and_get(),
                    new_this_type
                );
                
                let function_info = FunctionType {
                    is_extension: function_info.is_extension,
                    generics_define: Type::replace_generics_define_this_type(&function_info.generics_define, new_this_type),
                    argument_types,
                    return_type: function_info.return_type.clone()
                        .map(|ty| { ty.replace_this_type(new_this_type, replace_field_recursive, replace_bounds) }),
                    define_info: function_info.define_info.clone(),
                    where_bounds: FreezableMutex::new(where_bounds)
                };
                
                let generics = generics.iter()
                    .map(|ty| { ty.replace_this_type(new_this_type, replace_field_recursive, replace_bounds) })
                    .collect();
                
                Type::Function {
                    function_info: Arc::new(function_info),
                    generics: Arc::new(generics)
                }
            },
            Type::Generic(generic) => {
                if !replace_bounds {
                    return self.clone();
                }

                let bounds = Type::replace_bounds_this_type(&generic.bounds.freeze_and_get(), new_this_type);
                
                Type::Generic(Arc::new(GenericType {
                    define_entity_id: generic.define_entity_id,
                    name: generic.name.clone(),
                    bounds: FreezableMutex::new(bounds),
                    location: generic.location.clone()
                }))
            },
            Type::Array(base_type) => Type::Array(Arc::new(base_type.replace_this_type(new_this_type, replace_field_recursive, replace_bounds))),
            Type::Option(value) => Type::Option(Arc::new(value.replace_this_type(new_this_type, replace_field_recursive, replace_bounds))),
            Type::Result { value, error } => {
                Type::Result {
                    value: Arc::new(value.replace_this_type(new_this_type, replace_field_recursive, replace_bounds)),
                    error: Arc::new(error.replace_this_type(new_this_type, replace_field_recursive, replace_bounds))
                }
            },
            Type::This => new_this_type.clone(),
            _ => self.clone()
        }
    }
    
    pub(crate) fn replace_bounds_this_type(bounds: &Vec<Arc<Bound>>, new_this_type: &Type) -> Vec<Arc<Bound>> {
        bounds.iter()
            .map(|bound| {
                Arc::new(Bound {
                    module_name: bound.module_name.clone(),
                    span: bound.span.clone(),
                    ty: bound.ty.replace_this_type(new_this_type, false, false),
                    entity_id: bound.entity_id
                })
            })
            .collect()
    }
    
    pub(crate) fn replace_generics_define_this_type(generics_define: &Vec<Arc<GenericType>>, new_this_type: &Type) -> Vec<Arc<GenericType>> {
        generics_define.iter()
            .map(|generic_define| {
                let bounds = Type::replace_bounds_this_type(
                    &generic_define.bounds.freeze_and_get(),
                    new_this_type
                );
                
                Arc::new(GenericType {
                    define_entity_id: generic_define.define_entity_id,
                    name: generic_define.name.clone(),
                    bounds: FreezableMutex::new(bounds),
                    location: generic_define.location.clone()
                })
            })
            .collect()
    }
    
    pub(crate) fn replace_where_bounds_this_type(where_bounds: &Vec<WhereBound>, new_this_type: &Type) -> Vec<WhereBound> {
        where_bounds.iter()
            .map(|where_bound| {
                WhereBound {
                    target_type: where_bound.target_type.clone().map(|ty| { ty.replace_this_type(new_this_type, false, false) }),
                    bounds: Type::replace_bounds_this_type(&where_bound.bounds, new_this_type)
                }
            })
            .collect()
    }

    fn get_where_bounds_with_generics(
        where_bounds: &Vec<WhereBound>,
        generics_define: &Vec<Arc<GenericType>>,
        local_generics: &Vec<Type>
    ) -> Vec<WhereBound> {
        let mut new_where_bounds = Vec::new();
        for where_bound in where_bounds.iter() {
            let target_type = Type::get_type_with_replaced_generics(
                &where_bound.target_type.value,
                generics_define,
                local_generics
            );
            let mut bounds = Vec::new();
            for bound in where_bound.bounds.iter() {
                let ty = Type::get_type_with_replaced_generics(&bound.ty, generics_define, local_generics);
                let bound = Bound {
                    module_name: bound.module_name.clone(),
                    span: bound.span.clone(),
                    ty,
                    entity_id: bound.entity_id
                };
                bounds.push(Arc::new(bound));
            }
            new_where_bounds.push(WhereBound {target_type: Spanned::new(target_type, where_bound.target_type.span.clone()), bounds })
        }
        new_where_bounds
    }
    
    pub(crate) fn init_generics(&self) -> Type {
        match self {
            Type::UserType { user_type_info, generics, generics_span } => {
                if !generics.is_empty() {
                    return self.clone();
                }
                
                let generics = user_type_info.generics_define.iter()
                    .map(|generic| { Type::Generic(generic.clone()) })
                    .collect::<Vec<_>>();
                Type::UserType { user_type_info: user_type_info.clone(), generics: Arc::new(generics), generics_span: generics_span.clone() }
            },
            Type::Function { function_info, generics } => {
                if !generics.is_empty() {
                    return self.clone();
                }
                
                let generics = function_info.generics_define.iter()
                    .map(|generic| { Type::Generic(generic.clone()) })
                    .collect::<Vec<_>>();
                Type::Function { function_info: function_info.clone(), generics: Arc::new(generics) }
            },
            _ => self.clone()
        }
    }

    pub(crate) fn get_element_type_with_replaced_generic(&self, name: &str) -> Option<WithDefineInfo<Type>> {
        match self {
            Type::UserType { user_type_info, generics, generics_span: _ } => {
                let element_type = {
                    let element_type_map = user_type_info.element_types.lock().unwrap();
                    element_type_map.get(name).cloned()
                };

                element_type.map(|ty| {
                    ty.map(|ty| {
                        Type::get_type_with_replaced_generics(&ty, &user_type_info.generics_define, generics)
                    })
                })
            },
            _ => None
        }
    }

    pub(crate) fn get_elements_with_replaced_generic(&self) -> Vec<(String, WithDefineInfo<Type>)> {
        match self {
            Type::UserType { user_type_info, generics, generics_span: _ } => {
                let element_types = {
                    let element_type_map = user_type_info.element_types.lock().unwrap();
                    element_type_map.iter().map(|(name, ty)| { (name.clone(), ty.clone()) }).collect::<Vec<_>>()
                };

                element_types.into_iter().map(|(name, ty)| {
                    let ty = ty.map(|ty| {
                        Type::get_type_with_replaced_generics(&ty, &user_type_info.generics_define, generics)
                    });
                    (name, ty)
                }).collect()
            },
            _ => Vec::new()
        }
    }

    pub(crate) fn get_where_bounds(&self) -> Option<Arc<Vec<WhereBound>>> {
        match self {
            Type::UserType { user_type_info, generics: _, generics_span: _ } => {
                Some(user_type_info.where_bounds.freeze_and_get())
            },
            Type::Function { function_info, generics: _ } => {
                Some(function_info.where_bounds.freeze_and_get())
            },
            _ => None
        }
    }
    
    pub(crate) fn get_where_bounds_with_replaced_generic(&self) -> Option<Vec<WhereBound>> {
        match self {
            Type::UserType { user_type_info, generics, generics_span: _ } => {
                Some(Type::get_where_bounds_with_generics(
                    &user_type_info.where_bounds.freeze_and_get(),
                    &user_type_info.generics_define,
                    generics
                ))
            },
            Type::Function { function_info, generics } => {
                Some(Type::get_where_bounds_with_generics(
                    &function_info.where_bounds.freeze_and_get(),
                    &function_info.generics_define,
                    generics
                ))
            },
            _ => None
        }
    }
    
    pub(crate) fn get_generics_define_with_replaced_generic(&self) -> Option<Vec<Arc<GenericType>>> {
        match self {
            Type::UserType { user_type_info, generics, generics_span: _ } => {
                Some(
                    user_type_info.generics_define.iter()
                        .map(|generic_define| { Arc::new(generic_define.replace_generic(&user_type_info.generics_define, generics)) })
                        .collect()
                )
            },
            Type::Function { function_info, generics } => {
                Some(
                    function_info.generics_define.iter()
                        .map(|generic_define| { Arc::new(generic_define.replace_generic(&function_info.generics_define, generics)) })
                        .collect()
                )
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

    pub(crate) fn get_indexed_element_type_with_replaced_generic(&self, index: usize) -> Option<Type> {
        match self {
            Type::Function { function_info, generics } => {
                let element_type = function_info.argument_types.get(index);

                element_type.map(|ty| { Type::get_type_with_replaced_generics(ty, &function_info.generics_define, generics) })
            },
            _ => None
        }
    }

    pub(crate) fn get_return_type_with_replaced_generic(&self) -> Option<Type> {
        match self {
            Type::Function { function_info, generics } => {
                let return_type = &function_info.return_type.value;
                Some(Type::get_type_with_replaced_generics(return_type, &function_info.generics_define, generics))
            },
            _ => None
        }
    }
    
    pub(crate) fn replace_method_instance_type(&self, instance_type: &Type) -> Type {
        match self {
            Type::Function { function_info, generics } => {
                let mut new_function_type = function_info.as_ref().clone();
                if !new_function_type.argument_types.is_empty() {
                    new_function_type.argument_types[0] = instance_type.clone();
                }

                Type::Function {
                    function_info: Arc::new(new_function_type),
                    generics: generics.clone()
                }
            },
            _ => self.clone()
        }
    }
    
    pub(crate) fn get_generics_define_length(&self) -> usize {
        match self {
            Type::UserType { user_type_info, generics: _, generics_span: _ } => {
                user_type_info.generics_define.len()
            },
            Type::Function { function_info, generics: _ } => {
                function_info.generics_define.len()
            },
            _ => 0
        }
    }

    pub(crate) fn as_original_type(&self) -> Type {
        match self {
            Type::UserType { user_type_info, generics: _, generics_span: _ } => {
                let generics = user_type_info.generics_define.iter()
                    .map(|generics_define| { Type::Generic(generics_define.clone()) })
                    .collect();
                Type::UserType { user_type_info: user_type_info.clone(), generics: Arc::new(generics), generics_span: None }
            },
            Type::Function { function_info, generics: _ } => {
                let generics = function_info.generics_define.iter()
                    .map(|generics_define| { Type::Generic(generics_define.clone()) })
                    .collect();
                Type::Function { function_info: function_info.clone(), generics: Arc::new(generics) }
            },
            _ => self.clone()
        }
    }

    pub(crate) fn contains_unknown(&self) -> bool {
        match self {
            Type::UserType { user_type_info, generics, generics_span: _ } => {
                let contains_unknown = user_type_info.generics_define.iter().any(|generics_define| {
                    generics_define.bounds.freeze_and_get().iter().any(|bound| {
                        bound.ty.contains_unknown()
                    })
                });
                if contains_unknown {
                    return true;
                }
                generics.iter().any(|generic| { generic.contains_unknown() })
            },
            Type::Function { function_info, generics } => {
                let contains_unknown = function_info.generics_define.iter().any(|generics_define| {
                    generics_define.bounds.freeze_and_get().iter().any(|bound| {
                        bound.ty.contains_unknown()
                    })
                });
                if contains_unknown {
                    return true;
                }
                generics.iter().any(|generic| { generic.contains_unknown() })
            },
            Type::Generic(generics) => {
                generics.bounds.freeze_and_get().iter()
                    .any(|bound| { bound.ty.contains_unknown() })
            },
            Type::Array(base_type) => base_type.contains_unknown(),
            Type::Option(value_type) => value_type.contains_unknown(),
            Type::Result { value, error } => {
                value.contains_unknown() || error.contains_unknown()
            },
            Type::Unknown => true,
            _ => false
        }
    }

    pub(crate) fn is_renamed_type(&self) -> bool {
        match self {
            Type::UserType { user_type_info, generics: _, generics_span: _ } => {
                let element_types = user_type_info.element_types.lock().unwrap();
                element_types.contains_key("type")
            },
            _ => false
        }
    }
    
    pub(crate) fn is_interface(&self) -> bool {
        match self {
            Type::UserType { user_type_info, generics: _, generics_span: _ } => {
                user_type_info.kind == UserTypeKindEnum::Interface
            },
            _ => false
        }
    }

    pub(crate) fn is_replaceable_with(&self, after: &Type) -> bool {
        match self {
            Type::Unknown | Type::Unreachable => true,
            Type::NumericLiteral(comaptible_types_1) => {
                if let Type::NumericLiteral(compatible_types_2) = after {
                    comaptible_types_1.iter().any(|ty| { compatible_types_2.contains(ty) })
                } else {
                    comaptible_types_1.contains(after)
                }
            }
            _ => false
        }
    }

    pub(crate) fn get_numeric_compatible_optimal_type(&self) -> Type {
        match self {
            Type::NumericLiteral(compatible_types) => {
                static OPTIMAL_TYPES: &[Type] = &[Type::Int32, Type::Int64, Type::Uint32, Type::Uint64, Type::Float32, Type::Float64];
                compatible_types.iter().find(|&ty| { OPTIMAL_TYPES.contains(ty) }).unwrap().clone()
            },
            _ => unreachable!()
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
    "unit",
    "This"
];


#[derive(Debug)]
pub struct UserTypeInfo {
    pub module_name: Arc<String>,
    pub name: Spanned<String>,
    pub define_span: Range<usize>,
    pub kind: UserTypeKindEnum,
    pub generics_define: Vec<Arc<GenericType>>,
    pub generics_define_span: Option<Range<usize>>,
    pub element_types: Mutex<FxHashMap<String, WithDefineInfo<Type>>>,
    pub element_attributes: Arc<Mutex<FxHashMap<String, Vec<StatementAttribute>>>>,
    pub where_bounds: FreezableMutex<Vec<WhereBound>>
}

impl PartialEq for UserTypeInfo {
    fn eq(&self, other: &Self) -> bool {
        self.module_name == other.module_name && self.name == other.name
    }
}
impl Eq for UserTypeInfo {}

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

impl<T: Default> Default for FreezableMutex<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T: Default> Clone for FreezableMutex<T> {
    fn clone(&self) -> Self {
        let value = self.freeze_and_get();
        Self { mutex: Mutex::new(Either::Right(value)) }
    }
}


#[derive(Debug, Clone)]
pub struct WithDefineInfo<T> {
    pub value: T,
    pub module_name: Arc<String>,
    pub span: Range<usize>
}

impl<T> WithDefineInfo<T> {
    pub fn map<U, F>(self, function: F) -> WithDefineInfo<U>
    where F: FnOnce(T) -> U {
        let value = function(self.value);
        WithDefineInfo {
            value,
            module_name: self.module_name,
            span: self.span
        }
    }

    pub fn to_spanned(self) -> Spanned<T> {
        Spanned::new(self.value, self.span)
    }
}


#[derive(Debug)]
pub struct GenericType {
    pub(crate) define_entity_id: EntityID,
    pub name: Arc<String>,
    pub bounds: FreezableMutex<Vec<Arc<Bound>>>,
    pub location: WithDefineInfo<()>
}

impl GenericType {
    
    fn get_bounds_with_replaced_generic(
        &self,
        generics_define: &Vec<Arc<GenericType>>,
        replace_generics: &Vec<Type>
    ) -> Vec<Arc<Bound>> {
        let mut bounds = Vec::new();
        for bound in self.bounds.freeze_and_get().iter() {
            let ty = Type::get_type_with_replaced_generics(&bound.ty, generics_define, replace_generics);
            bounds.push(Arc::new(Bound {
                module_name: bound.module_name.clone(),
                span: bound.span.clone(),
                ty,
                entity_id: bound.entity_id
            }));
        }
        bounds
    }
    
    pub(crate) fn replace_generic(
        &self,
        generics_define: &Vec<Arc<GenericType>>,
        replace_generics: &Vec<Type>
    ) -> GenericType {
        GenericType {
            define_entity_id: self.define_entity_id,
            name: self.name.clone(),
            bounds: FreezableMutex::new(self.get_bounds_with_replaced_generic(generics_define, replace_generics)),
            location: self.location.clone()
        }
    }
    
}

impl PartialEq for GenericType {
    fn eq(&self, other: &Self) -> bool {
        self.define_entity_id == other.define_entity_id
    }
}
impl Eq for GenericType {}

#[derive(Debug)]
pub struct WhereBound {
    pub target_type: Spanned<Type>,
    pub bounds: Vec<Arc<Bound>>
}

#[derive(Debug)]
pub struct Bound {
    pub module_name: Arc<String>,
    pub span: Range<usize>,
    pub ty: Type,
    pub entity_id: EntityID
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Eq, Clone)]
pub struct FunctionType {
    pub is_extension: bool,
    pub generics_define: Vec<Arc<GenericType>>,
    pub argument_types: Vec<Type>,
    pub return_type: Spanned<Type>,
    #[derivative(PartialEq="ignore")]
    pub define_info: FunctionDefineInfo,
    #[derivative(PartialEq="ignore")]
    pub where_bounds: FreezableMutex<Vec<WhereBound>>
} 

#[derive(Debug, Clone)]
pub struct FunctionDefineInfo {
    pub module_name: Arc<String>,
    pub generics_define_span: Option<Range<usize>>,
    pub arguments_span: Range<usize>,
    pub is_closure: bool,
    pub span: Range<usize>
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalGenericID(pub usize);


#[derive(Debug, Clone)]
pub struct ImplementsInfo {
    pub generics: Arc<Vec<Arc<GenericType>>>,
    pub interface: Spanned<Type>,
    pub concrete: Spanned<Type>,
    pub module_name: Arc<String>,
    pub where_bounds: Arc<Vec<WhereBound>>,
    pub element_types: Arc<FxHashMap<String, WithDefineInfo<Type>>>,
    pub is_bounds_info: bool
}

impl ImplementsInfo {
    
    fn contains_target_type(
        self_type: &Type,
        ty: &Type,
        global_implements_info_set: &ImplementsInfoSet,
        current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
        type_environment: &mut TypeEnvironment,
        allow_unknown: bool
    ) -> bool {
        if allow_unknown {
            if self_type == &Type::Unknown || ty == &Type::Unknown {
                return true;
            }
        }
        
        if self_type == &Type::Unreachable || ty == &Type::Unreachable {
            return true;
        }

        if let Type::LocalGeneric(generic_id) = ty {
            let (_z, ty) = type_environment.resolve_generic_type(*generic_id);

            return ImplementsInfo::contains_target_type(
                self_type,
                &ty.value,
                global_implements_info_set,
                current_scope_implements_info_set,
                type_environment,
                allow_unknown
            );
        }
        if let Type::LocalGeneric(generic_id) = self_type {
            let (_, self_type) = type_environment.resolve_generic_type(*generic_id);

            return ImplementsInfo::contains_target_type(
                &self_type.value,
                ty,
                global_implements_info_set,
                current_scope_implements_info_set,
                type_environment,
                allow_unknown
            );
        }

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
            Type::NumericLiteral(_) => ty == &self_type.get_numeric_compatible_optimal_type(),
            Type::UserType { user_type_info: self_user_type_info, generics: self_generics, generics_span: _ } => {
                if let Type::UserType { user_type_info, generics, generics_span: _ } = ty {
                    if self_user_type_info != user_type_info {
                        return false;
                    }
                    
                    if self_generics.len() != generics.len() && self_user_type_info.generics_define.len() != generics.len() {
                        return false;
                    }
                    
                    for i in 0..self_generics.len() {
                        let self_generic = self_generics.get(i);
                        let generic = &generics[i];
            
                        if let Some(self_generic) = self_generic {
                            if !ImplementsInfo::contains_target_type(
                                self_generic,
                                generic,
                                global_implements_info_set,
                                current_scope_implements_info_set,
                                type_environment,
                                allow_unknown
                            ) {
                                return false;
                            }
                        } else {
                            let self_generic = Type::Generic(self_user_type_info.generics_define[i].clone());

                            if !ImplementsInfo::contains_target_type(
                                &self_generic,
                                generic,
                                global_implements_info_set,
                                current_scope_implements_info_set,
                                type_environment,
                                allow_unknown
                            ) {
                                return false;
                            }
                        }
                    }

                    true
                } else {
                    false
                }
            },
            Type::Function { function_info: self_function_info, generics: _ } => {
                if let Type::Function { function_info, generics: _ } = ty {
                    if !ImplementsInfo::contains_target_type(
                        &self_function_info.return_type.value,
                        &function_info.return_type.value,
                        global_implements_info_set,
                        current_scope_implements_info_set,
                        type_environment,
                        allow_unknown
                    ) {
                        return false;
                    }
                    
                    if self_function_info.argument_types.len() != function_info.argument_types.len() {
                        return false;
                    }

                    for i in 0..self_function_info.argument_types.len() {
                        let self_argument_type = &self_function_info.argument_types[i];
                        let argument_type = &function_info.argument_types[i];

                        if !ImplementsInfo::contains_target_type(
                            self_argument_type,
                            argument_type,
                            global_implements_info_set,
                            current_scope_implements_info_set,
                            type_environment,
                            allow_unknown
                        ) {
                            return false;
                        }
                    }
                    
                    true
                } else {
                    false
                }
            },
            Type::Generic(self_generic_type) => {
                if let Type::Generic(generic_type) = ty {
                    for self_bound in self_generic_type.bounds.freeze_and_get().iter() {
                        let mut is_satisfied = false;
                        for bound in generic_type.bounds.freeze_and_get().iter() {
                            if ImplementsInfo::contains_target_type(
                                &self_bound.ty,
                                &bound.ty,
                                global_implements_info_set,
                                current_scope_implements_info_set,
                                type_environment,
                                allow_unknown
                            ) {
                                is_satisfied = true;
                                break;
                            }
                        }

                        if !is_satisfied {
                            return false;
                        }
                    }
                    
                    return true;
                }
                
                let generics_before = vec![self_generic_type.clone()];
                let generics_after = vec![ty.clone()];

                for bound in self_generic_type.bounds.freeze_and_get().iter() {
                    let replaced_bound_type = Type::get_type_with_replaced_generics(
                        &bound.ty,
                        &generics_before,
                        &generics_after
                    );

                    if !global_implements_info_set.is_implemented(
                        ty,
                        &replaced_bound_type,
                        type_environment,
                        current_scope_implements_info_set,
                        allow_unknown
                    ) {
                        return false;
                    }
                }
                true
            },
            Type::LocalGeneric(_) => unreachable!(),
            Type::Array(self_base_type) => {
                if let Type::Array(base_type) = ty {
                    ImplementsInfo::contains_target_type(
                        &self_base_type,
                        &base_type,
                        global_implements_info_set,
                        current_scope_implements_info_set,
                        type_environment,
                        allow_unknown
                    )
                } else {
                    false
                }
            },
            Type::Option(self_value_type) => {
                if let Type::Option(value_type) = ty {
                    ImplementsInfo::contains_target_type(
                        &self_value_type,
                        &value_type,
                        global_implements_info_set,
                        current_scope_implements_info_set,
                        type_environment,
                        allow_unknown
                    )
                } else {
                    false
                }
            },
            Type::Result { value: self_value, error: self_error } => {
                if let Type::Result { value, error } = ty {
                    ImplementsInfo::contains_target_type(
                        &self_value,
                        &value,
                        global_implements_info_set,
                        current_scope_implements_info_set,
                        type_environment,
                        allow_unknown
                    ) && ImplementsInfo::contains_target_type(
                        &self_error,
                        &error,
                        global_implements_info_set,
                        current_scope_implements_info_set,
                        type_environment,
                        allow_unknown
                    )
                } else {
                    false
                }
            },
            Type::This => ty == &Type::This,
            Type::Unreachable => true,
            Type::Unknown => false
        }
    }
    
    pub(crate) fn get_element_type(&self, element_name: &str) -> Option<WithDefineInfo<Type>> {
        if self.is_bounds_info {
            self.interface.value.get_element_type_with_replaced_generic(element_name)
                .map(|ty| {
                    WithDefineInfo {
                        value: ty.value.replace_this_type(&self.concrete.value, true, true),
                        module_name: ty.module_name.clone(),
                        span: ty.span.clone()
                    }
                })
        } else {
            self.element_types.get(element_name).cloned()
        }
    }

}


#[derive(Debug, Clone, Default)]
pub struct ImplementsInfoSet {
    pub(crate) implements_infos: IndexMap<EntityID, ImplementsInfo>
}


impl ImplementsInfoSet {
    
    pub fn new() -> Self {
        Self {
            implements_infos: IndexMap::new()
        }
    }

    pub fn insert(&mut self, entity_id: EntityID, implements_info: ImplementsInfo) {
        self.implements_infos.insert(entity_id, implements_info);
    }

    pub fn get(&self, entity_id: EntityID) -> Option<&ImplementsInfo> {
        self.implements_infos.get(&entity_id)
    }

    pub fn merge(&mut self, other: &ImplementsInfoSet) {
        self.implements_infos.extend(other.implements_infos.clone());
    }

    pub(crate) fn is_implemented(
        &self,
        ty: &Type,
        interface: &Type,
        type_environment: &mut TypeEnvironment,
        current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
        allow_unknown: bool
    ) -> bool {

        let resolved_ty = type_environment.resolve_type(ty);

        if ImplementsInfo::contains_target_type(
            interface,
            &resolved_ty,
            self,
            current_scope_implements_info_set,
            type_environment,
            allow_unknown
        ) {
            return true;
        }
        
        if let Type::Generic(generic) = resolved_ty {
            for bound in generic.bounds.freeze_and_get().iter() {
                if ImplementsInfo::contains_target_type(
                    interface,
                    &bound.ty,
                    self,
                    current_scope_implements_info_set,
                    type_environment,
                    allow_unknown
                ) {
                    return true;
                }
            }
        }

        let empty_map = IndexMap::new();
        let iter = match current_scope_implements_info_set {
            Some(scope_implements_info_set) => {
                scope_implements_info_set.implements_infos.values().chain(self.implements_infos.values())
            },
            _ => self.implements_infos.values().chain(empty_map.values())
        };

        for implements_info in iter {
            if (ImplementsInfo::contains_target_type(
                &implements_info.interface.value,
                interface,
                self,
                current_scope_implements_info_set,
                type_environment,
                allow_unknown
            ) || ImplementsInfo::contains_target_type(
                interface,
                &implements_info.interface.value,
                self,
                current_scope_implements_info_set,
                type_environment,
                allow_unknown
            )) && ImplementsInfo::contains_target_type(
                &implements_info.concrete.value,
                ty,
                self,
                current_scope_implements_info_set,
                type_environment,
                allow_unknown
            ) {
                // init generic variables
                let generics_define = &implements_info.generics;
                let mut local_generics = Vec::new();
                for _ in 0..generics_define.len() {
                    let generic_id = type_environment.new_local_generic_id(
                        0..0,
                        implements_info.module_name.clone()
                    );
                    local_generics.push(Type::LocalGeneric(generic_id));
                }

                let impl_concrete = Type::get_type_with_replaced_generics(
                    &implements_info.concrete.value,
                    generics_define,
                    &local_generics
                );
                let impl_interface = Type::get_type_with_replaced_generics(
                    &implements_info.interface.value,
                    generics_define,
                    &local_generics
                );

                let resolved_ty = type_environment.resolve_type(ty);
                let resolved_interface = type_environment.resolve_type(interface);

                // give answer to resolve generic variables
                let result1 = type_environment.unify_type(
                    &impl_concrete,
                    &(0..0),
                    &implements_info.module_name,
                    &resolved_ty,
                    &(0..0),
                    &implements_info.module_name,
                    &ScopeThisType::new(Type::This),
                    allow_unknown,
                    false
                );
                let result2 = type_environment.unify_type(
                    &impl_interface,
                    &(0..0),
                    &implements_info.module_name,
                    &resolved_interface,
                    &(0..0),
                    &implements_info.module_name,
                    &ScopeThisType::new(Type::This),
                    allow_unknown,
                    false
                );

                if result1.is_err() || result2.is_err() {
                    // ignore
                    continue;
                }

                let mut is_satisfied = true;
                'check: for where_bound in implements_info.where_bounds.iter() {
                    let target_type = Type::get_type_with_replaced_generics(
                        &where_bound.target_type.value,
                        generics_define,
                        &local_generics
                    );
                    for bound in where_bound.bounds.iter() {
                        let bound_type = Type::get_type_with_replaced_generics(
                            &bound.ty,
                            generics_define,
                            &local_generics
                        );

                        if !self.is_implemented(
                            &target_type,
                            &bound_type,
                            type_environment,
                            current_scope_implements_info_set,
                            allow_unknown
                        ) {
                            is_satisfied = false;
                            break 'check;
                        }
                    }
                }

                if is_satisfied {
                    return true;
                }
            }
        }
        false
    }

    pub(crate) fn is_satisfied(
        &self,
        ty: &Type,
        bounds: &Vec<Arc<Bound>>,
        type_environment: &mut TypeEnvironment,
        current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
        allow_unknown: bool
    ) -> Result<(), Vec<Arc<Bound>>> {
        let mut not_satisfied_types = Vec::new();
        for bound in bounds.iter() {
            if !self.is_implemented(
                ty,
                &bound.ty,
                type_environment,
                current_scope_implements_info_set,
                allow_unknown
            ) {
                not_satisfied_types.push(bound.clone())
            }
        }
        
        if not_satisfied_types.is_empty() {
            Ok(())
        } else {
            Err(not_satisfied_types)
        }
    }

    pub(crate) fn collect_satisfied_implementations<'allocator>(
        &self,
        ty: &Type,
        type_environment: &mut TypeEnvironment,
        current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
        allocator: &'allocator Bump
    ) -> Vec<CollectedImplementation, &'allocator Bump> {
        let mut satisfied_implementations = Vec::new_in(allocator);

        let empty_map = IndexMap::new();
        let iter = match current_scope_implements_info_set {
            Some(scope_implements_info_set) => {
                scope_implements_info_set.implements_infos.values().chain(self.implements_infos.values())
            },
            _ => self.implements_infos.values().chain(empty_map.values())
        };

        for implements_info in iter {
            if !ImplementsInfo::contains_target_type(
                &implements_info.concrete.value,
                ty,
                self,
                current_scope_implements_info_set,
                type_environment,
                true
            ) {
                continue;
            }

            // init generic variables
            let generics_define = &implements_info.generics;
            let mut local_generics = Vec::new();
            for _ in 0..generics_define.len() {
                let generic_id = type_environment.new_local_generic_id(
                    0..0,
                    implements_info.module_name.clone()
                );
                local_generics.push(Type::LocalGeneric(generic_id));
            }

            let impl_concrete = Type::get_type_with_replaced_generics(
                &implements_info.concrete.value,
                generics_define,
                &local_generics
            );
            let impl_interface = Type::get_type_with_replaced_generics(
                &implements_info.interface.value,
                generics_define,
                &local_generics
            );

            let resolved_ty = type_environment.resolve_type(ty);

            // give answer to resolve generic variables
            let result = type_environment.unify_type(
                &impl_concrete,
                &(0..0),
                &implements_info.module_name,
                &resolved_ty,
                &(0..0),
                &implements_info.module_name,
                &ScopeThisType::new(impl_concrete.clone()),
                true,
                false
            );

            if result.is_err() {
                continue;
            }

            let where_bounds = Type::get_where_bounds_with_generics(
                &implements_info.where_bounds,
                generics_define,
                &local_generics
            );

            let mut is_satisfied = true;
            'check: for where_bound in where_bounds.iter() {
                for bound in where_bound.bounds.iter() {
                    if !self.is_implemented(
                        &where_bound.target_type.value,
                        &bound.ty,
                        type_environment,
                        current_scope_implements_info_set,
                        true
                    ) {
                        is_satisfied = false;
                        break 'check;
                    }
                }
            }

            if is_satisfied {
                let implements_info = ImplementsInfo {
                    generics: implements_info.generics.clone(),
                    interface: Spanned::new(impl_interface, implements_info.interface.span.clone()),
                    concrete: Spanned::new(impl_concrete, implements_info.concrete.span.clone()),
                    module_name: implements_info.module_name.clone(),
                    where_bounds: Arc::new(where_bounds),
                    element_types: implements_info.element_types.clone(),
                    is_bounds_info: implements_info.is_bounds_info
                };

                satisfied_implementations.push(CollectedImplementation {
                    implements_info,
                    local_generics
                })
            }
        }

        satisfied_implementations
    }

    pub(crate) fn type_inference_for_generic_bounds<'allocator>(
        &self,
        original_bound: &Type,
        local_generic_replaced_bound: &Type,
        local_generic_replaced_target: &Type,
        target_span: &Range<usize>,
        target_type_module_name: &Arc<String>,
        bound_span: &Range<usize>,
        bound_type_module_name: &Arc<String>,
        allow_unknown: bool,
        type_environment: &mut TypeEnvironment,
        current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
        errors: &mut Vec<TypeMismatchError, &'allocator Bump>,
    ) {
        let resolved_target = if let Type::LocalGeneric(generic_id) = local_generic_replaced_target {
            type_environment.resolve_generic_type(*generic_id).1.value
        } else {
            local_generic_replaced_target.clone()
        };

        if &Type::Unknown == &resolved_target {
            return;
        }

        match &resolved_target {
            Type::UserType { user_type_info: target_user_type_info, generics: _, generics_span: _ } => {
                if let Type::UserType { user_type_info: bound_user_type_info, generics: _, generics_span: _ } = original_bound {
                    if target_user_type_info.module_name.as_str() == bound_user_type_info.module_name.as_str()
                        && target_user_type_info.name.value.as_str() == bound_user_type_info.name.value.as_str() {
                        
                        if let Err(error) = type_environment.unify_type(
                            local_generic_replaced_bound,
                            bound_span,
                            bound_type_module_name,
                            local_generic_replaced_target,
                            target_span,
                            target_type_module_name,
                            &ScopeThisType::new(Type::This),
                            allow_unknown,
                            false
                        ) {
                            errors.push(error);
                        }
                    }
                }
            },
            Type::Generic(generic) => {
                for bound in generic.bounds.freeze_and_get().iter() {
                    let resolved_bound = if let Type::LocalGeneric(generic_id) = &bound.ty {
                        type_environment.resolve_generic_type(*generic_id).1.value
                    } else {
                        bound.ty.clone()
                    };

                    if let Type::UserType { user_type_info: user_type_info_0, generics: _, generics_span: _ } = &resolved_bound {
                        if let Type::UserType { user_type_info: user_type_info_1, generics: _, generics_span: _ } = &local_generic_replaced_bound {
                            if user_type_info_0.module_name.as_str() == user_type_info_1.module_name.as_str()
                                && user_type_info_0.name.value.as_str() == user_type_info_1.name.value.as_str() {
                                
                                if let Err(error) = type_environment.unify_type(
                                    local_generic_replaced_bound,
                                    bound_span,
                                    bound_type_module_name,
                                    &resolved_bound,
                                    target_span,
                                    target_type_module_name,
                                    &ScopeThisType::new(Type::This),
                                    allow_unknown,
                                    false
                                ) {
                                    errors.push(error);
                                }
                            }
                        }
                    }
                }
            },
            _ => {}
        }


        let empty_map = IndexMap::new();
        let iter = match current_scope_implements_info_set {
            Some(scope_implements_info_set) => {
                scope_implements_info_set.implements_infos.values().chain(self.implements_infos.values())
            },
            _ => self.implements_infos.values().chain(empty_map.values())
        };

        for implements_info in iter {
            if (ImplementsInfo::contains_target_type(
                &implements_info.interface.value,
                local_generic_replaced_bound,
                self,
                current_scope_implements_info_set,
                type_environment,
                allow_unknown
            ) || ImplementsInfo::contains_target_type(
                local_generic_replaced_bound,
                &implements_info.interface.value,
                self,
                current_scope_implements_info_set,
                type_environment,
                allow_unknown
            )) && ImplementsInfo::contains_target_type(
                &implements_info.concrete.value,
                local_generic_replaced_target,
                self,
                current_scope_implements_info_set,
                type_environment,
                allow_unknown
            ) {
                // init generic variables
                let generics_define = &implements_info.generics;
                let mut local_generics = Vec::new();
                for generic_define in generics_define.iter() {
                    let generic_id = type_environment.new_local_generic_id(
                        generic_define.location.span.clone(),
                        generic_define.location.module_name.clone()
                    );
                    local_generics.push(Type::LocalGeneric(generic_id));
                }

                let temp_concrete = Type::get_type_with_replaced_generics(
                    &implements_info.concrete.value,
                    generics_define,
                    &local_generics
                );
                let temp_interface = Type::get_type_with_replaced_generics(
                    &implements_info.interface.value,
                    generics_define,
                    &local_generics
                );

                let resolved_ty = type_environment.resolve_type(local_generic_replaced_target);
                let resolved_interface = type_environment.resolve_type(local_generic_replaced_bound);

                // give answer to resolve generic variables
                let result1 = type_environment.unify_type(
                    &temp_concrete,
                    &implements_info.concrete.span.clone(),
                    &implements_info.module_name,
                    &resolved_ty,
                    target_span,
                    target_type_module_name,
                    &ScopeThisType::new(Type::This),
                    allow_unknown,
                    false
                );
                let result2 = type_environment.unify_type(
                    &temp_interface,
                    &implements_info.interface.span,
                    &implements_info.module_name,
                    &resolved_interface,
                    bound_span,
                    bound_type_module_name,
                    &ScopeThisType::new(Type::This),
                    allow_unknown,
                    false
                );

                if result1.is_err() || result2.is_err() {
                    // ignore
                    continue;
                }

                let mut is_satisfied = true;
                'check: for where_bound in implements_info.where_bounds.iter() {
                    let target_type = Type::get_type_with_replaced_generics(
                        &where_bound.target_type.value,
                        generics_define,
                        &local_generics
                    );
                    for bound in where_bound.bounds.iter() {
                        let bound_type = Type::get_type_with_replaced_generics(
                            &bound.ty,
                            generics_define,
                            &local_generics
                        );

                        if !self.is_implemented(
                            &target_type,
                            &bound_type,
                            type_environment,
                            current_scope_implements_info_set,
                            allow_unknown
                        ) {
                            is_satisfied = false;
                            break 'check;
                        }
                    }
                }

                if is_satisfied {
                    for generic in implements_info.generics.iter() {
                        let target_type = Type::get_type_with_replaced_generics(
                            &Type::Generic(generic.clone()),
                            generics_define,
                            &local_generics
                        );

                        for bound in generic.bounds.freeze_and_get().iter() {
                            let replaced_bound_type = Type::get_type_with_replaced_generics(
                                &bound.ty,
                                generics_define,
                                &local_generics
                            );

                            self.type_inference_for_generic_bounds(
                                &bound.ty,
                                &replaced_bound_type,
                                &target_type,
                                &generic.location.span,
                                &generic.location.module_name,
                                &bound.span,
                                &bound.module_name,
                                allow_unknown,
                                type_environment,
                                current_scope_implements_info_set,
                                errors
                            );
                        }
                    }

                    for where_bound in implements_info.where_bounds.iter() {
                        let target_type = Type::get_type_with_replaced_generics(
                            &where_bound.target_type.value,
                            generics_define,
                            &local_generics
                        );
                        for bound in where_bound.bounds.iter() {
                            let replaced_bound_type = Type::get_type_with_replaced_generics(
                                &bound.ty,
                                generics_define,
                                &local_generics
                            );

                            self.type_inference_for_generic_bounds(
                                &bound.ty,
                                &replaced_bound_type,
                                &target_type,
                                &where_bound.target_type.span,
                                &implements_info.module_name,
                                &bound.span,
                                &bound.module_name,
                                allow_unknown,
                                type_environment,
                                current_scope_implements_info_set,
                                errors
                            );
                        }
                    }

                    if let Err(error) = type_environment.unify_type(
                        local_generic_replaced_target,
                        target_span,
                        target_type_module_name,
                        &temp_concrete,
                        &implements_info.concrete.span,
                        &implements_info.module_name,
                        &ScopeThisType::new(Type::This),
                        allow_unknown,
                        false
                    ) {
                        errors.push(error);
                    }

                    if let Err(error) = type_environment.unify_type(
                        local_generic_replaced_bound,
                        bound_span,
                        bound_type_module_name,
                        &temp_interface,
                        &implements_info.interface.span,
                        &implements_info.module_name,
                        &ScopeThisType::new(Type::This),
                        allow_unknown,
                        false
                    ) {
                        errors.push(error);
                    }
                }
            }
        }


    }

}


pub(crate) struct CollectedImplementation {
    pub implements_info: ImplementsInfo,
    pub local_generics: Vec<Type>
}


pub(crate) fn collect_duplicated_implementation_error(
    implementations: &ImplementsInfoSet,
    other_module_implementations: &ImplementsInfoSet,
    errors: &mut Vec<TranspileError>
) {
    let implementations = implementations.implements_infos.values().collect::<Vec<_>>();
    let mut duplicated = Vec::new();
    
    'root: for i in 0..implementations.len() {
        let implementation = implementations[i];
        
        for other_implementation in other_module_implementations.implements_infos.values() {
            if is_duplicated_implementation(implementation, other_implementation) {
                duplicated.push((implementation, other_implementation.clone()));
                continue 'root;
            }
        }
        
        for other_implementation in &implementations[0..i] {
            if is_duplicated_implementation(implementation, other_implementation) {
                duplicated.push((implementation, (*other_implementation).clone()));
            }
        }
    }
    
    let temp_alloc = Bump::new();
    let temp_env = TypeEnvironment::new_with_return_type(
        Either::Left(EntityID::dummy()),
        &temp_alloc
    );

    for (duplicated, first_implementation) in duplicated {
        let duplicated_interface_name = temp_env.get_type_display_string(&duplicated.interface.value);
        let duplicated_concrete_name = temp_env.get_type_display_string(&duplicated.concrete.value);
        let first_impl_interface_name = temp_env.get_type_display_string(&first_implementation.interface.value);
        let first_impl_concrete_name = temp_env.get_type_display_string(&first_implementation.concrete.value);

        let error = SimpleError::new(
            0061,
            duplicated.interface.span.clone(),
            vec![
                (first_impl_concrete_name, Color::Yellow),
                (first_impl_interface_name, Color::Yellow),
                (duplicated_concrete_name, Color::Yellow),
                (duplicated_interface_name, Color::Yellow)
            ],
            vec![
                ((first_implementation.module_name.clone(), first_implementation.interface.span), Color::Yellow),
                ((duplicated.module_name.clone(), duplicated.interface.span.clone()), Color::Red)
            ]
        );
        errors.push(TranspileError::new(error));
    }
}

fn is_duplicated_implementation(implementation_1: &ImplementsInfo, implementation_2: &ImplementsInfo) -> bool {
    if let Type::UserType { user_type_info, generics: _, generics_span: _ } = &implementation_1.concrete.value {
        if user_type_info.kind == UserTypeKindEnum::Interface {
            return false;
        }
    }
    if let Type::UserType { user_type_info, generics: _, generics_span: _ } = &implementation_2.concrete.value {
        if user_type_info.kind == UserTypeKindEnum::Interface {
            return false;
        }
    }
    
    is_duplicated_implementation_type(&implementation_1.interface.value, &implementation_2.interface.value)
        && is_duplicated_implementation_type(&implementation_1.concrete.value, &implementation_2.concrete.value)
}

fn is_duplicated_implementation_type(type_1: &Type, type_2: &Type) -> bool {
    if let Type::Generic(_) = type_1 {
        return true;
    }
    if let Type::Generic(_) = type_2 {
        return true;
    }
    
    match type_1 {
        Type::UserType { user_type_info: user_type_info_1, generics: generics_1, generics_span: _ } => {
            if let Type::UserType { user_type_info: user_type_info_2, generics: generics_2, generics_span: _ } = type_2 {
                if user_type_info_1 != user_type_info_2 {
                    return false;
                }
                
                if generics_1.len() != generics_2.len() {
                    return false;
                }
                
                for (generic_1, generic_2) in generics_1.iter().zip(generics_2.iter()) {
                    if !is_duplicated_implementation_type(generic_1, generic_2) {
                        return false;
                    }
                }
                
                return true;
            }
            false
        },
        Type::Function { function_info: function_info_1, generics: generics_1 } => {
            if let Type::Function { function_info: function_info_2, generics: generics_2 } = type_2 {
                if function_info_1.argument_types.len() != function_info_2.argument_types.len() {
                    return false;
                }
                
                if generics_1.len() != generics_2.len() {
                    return false;
                }
                
                if !is_duplicated_implementation_type(&function_info_1.return_type.value, &function_info_2.return_type.value) {
                    return false;
                }
                
                for (argument_1, argument_2) in function_info_1.argument_types.iter().zip(function_info_2.argument_types.iter()) {
                    if !is_duplicated_implementation_type(argument_1, argument_2) {
                        return false;
                    }
                }
                
                for (generic_1, generic_2) in generics_1.iter().zip(generics_2.iter()) {
                    if !is_duplicated_implementation_type(generic_1, generic_2) {
                        return false;
                    }
                }
                
                return true;
            }
            false
        },
        Type::Generic(_) => unreachable!(),
        Type::LocalGeneric(_) => unreachable!(),
        Type::Array(base_type_1) => {
            if let Type::Array(base_type_2) = type_2 {
                return is_duplicated_implementation_type(base_type_1, base_type_2);
            }
            false
        },
        Type::Option(value_1) => {
            if let Type::Option(value_2) = type_2 {
                return is_duplicated_implementation_type(value_1, value_2);
            }
            false
        },
        Type::Result { value: value_1, error: error_1 } => {
            if let Type::Result { value: value_2, error: error_2 } = type_2 {
                return is_duplicated_implementation_type(value_1, value_2)
                    && is_duplicated_implementation_type(error_1, error_2);
            }
            false
        },
        Type::Unknown => return false,
        _ => type_1 == type_2
    }
}


pub(crate) struct OverrideElementsEnvironment {
    elements: Vec<(Spanned<Type>, String, WithDefineInfo<Type>)>,
    is_found_flags: Vec<bool>
}

impl OverrideElementsEnvironment {
    
    pub fn new(implements_interfaces: &Vec<Spanned<Type>>) -> Self {
        let mut elements = Vec::new();
        for interface in implements_interfaces.iter() {
            for element in interface.value.get_elements_with_replaced_generic() {
                elements.push((interface.clone(), element.0, element.1));
            }
        }
        let length = elements.len();
        
        Self {
            elements,
            is_found_flags: vec![false; length]
        }
    }

    pub fn check(
        &mut self,
        element_name: Spanned<&str>,
        element_type: &Type,
        concrete_type: &Type,
        global_implements_info_set: &ImplementsInfoSet,
        type_environment: &mut TypeEnvironment,
        context: &TranspileModuleContext
    ) -> Result<(), NoOverrideElementError> {
        for ((interface, name, interface_element_type), is_found) in self.elements.iter().zip(self.is_found_flags.iter_mut()) {
            let element_type = element_type.replace_method_instance_type(&interface.value);
            
            let interface_element_generics_define = match &interface_element_type.value {
                Type::UserType { user_type_info, generics: _, generics_span: _ } => {
                    Some(&user_type_info.generics_define)
                },
                Type::Function { function_info, generics: _ } => {
                    Some(&function_info.generics_define)
                },
                _ => None
            };
            
            let element_type_replaced = if let Some(interface_element_generics) = &interface_element_generics_define {
                let interface_element_generics = interface_element_generics.iter()
                    .map(|generic| { Type::Generic(generic.clone()) })
                    .collect::<Vec<_>>();
                
                let element_type = element_type.init_generics();

                match &element_type {
                    Type::UserType { user_type_info, generics: _, generics_span: _ } => {
                        Type::get_type_with_replaced_generics(&element_type, &user_type_info.generics_define, &interface_element_generics)
                    },
                    Type::Function { function_info, generics: _ } => {
                        Type::get_type_with_replaced_generics(&element_type, &function_info.generics_define, &interface_element_generics)
                    },
                    _ => element_type.clone()
                }
            } else {
                element_type
            };

            if name == element_name.value {
                *is_found = true;
                
                if type_environment.unify_type(
                    &interface_element_type.value,
                    &(0..0),
                    &interface_element_type.module_name,
                    &element_type_replaced,
                    &(0..0),
                    &interface_element_type.module_name,
                    &ScopeThisType::new(concrete_type.clone()),
                    true,
                    false
                ).is_ok() {
                    
                    if interface_element_type.value.get_generics_define_length() != element_type_replaced.get_generics_define_length() {
                        continue;
                    }
                    
                    let mut interface_element_implements_info_set = ImplementsInfoSet::new();
                    for where_bound in interface_element_type.value.get_where_bounds().unwrap().iter() {
                        for bound in where_bound.bounds.iter() {
                            interface_element_implements_info_set.insert(
                                bound.entity_id,
                                ImplementsInfo {
                                    generics: Arc::new(Vec::new()),
                                    interface: Spanned::new(bound.ty.clone(), bound.span.clone()),
                                    concrete: where_bound.target_type.clone(),
                                    module_name: context.module_name.clone(),
                                    where_bounds: Arc::new(Vec::new()),
                                    element_types: Arc::new(FxHashMap::default()),
                                    is_bounds_info: true
                                }
                            );
                        }
                    }
                    let interface_element_implements_info_set = Some(Arc::new(interface_element_implements_info_set));
                    
                    if let Some(element_type_generics_define) = element_type_replaced.get_generics_define_with_replaced_generic() {
                        let replaced_generics = interface_element_generics_define.unwrap();
                        
                        for (replaced, bounds_generics) in replaced_generics.iter().zip(element_type_generics_define.iter()) {
                            if let Err(bounds) = global_implements_info_set.is_satisfied(
                                &Type::Generic(replaced.clone()),
                                &bounds_generics.bounds.freeze_and_get(),
                                type_environment,
                                &interface_element_implements_info_set,
                                false
                            ) {
                                return Err(NoOverrideElementError::ExtraBounds {
                                    target: Type::Generic(replaced.clone()),
                                    bounds
                                });
                            }
                        }
                    }
                    
                    if let Some(where_bounds) = element_type_replaced.get_where_bounds_with_replaced_generic() {
                        for where_bound in where_bounds.iter() {
                            if let Err(bounds) = global_implements_info_set.is_satisfied(
                                &where_bound.target_type.value,
                                &where_bound.bounds,
                                type_environment,
                                &interface_element_implements_info_set,
                                false
                            ) {
                                return Err(NoOverrideElementError::ExtraBounds {
                                    target: where_bound.target_type.value.clone(),
                                    bounds
                                });
                            }
                        }
                    }
                    
                    return Ok(());
                } else {
                    return Err(NoOverrideElementError::NotEqualsType {
                        origin: interface_element_type.clone(),
                        found: WithDefineInfo {
                            value: element_type_replaced.clone(),
                            module_name: context.module_name.clone(),
                            span: element_name.span.clone()
                        }
                    });
                }
            }
        }
        
        Err(NoOverrideElementError::NotFoundEqualsName {
            name: element_name.map(|name| { name.to_string() })
        })
    }

    pub fn collect_errors(self, errors: &mut Vec<TranspileError>, context: &TranspileModuleContext) {
        let not_impl_elements = self.elements.into_iter().zip(self.is_found_flags.into_iter())
            .filter(|(_, is_found)| { !*is_found })
            .map(|(element, _)| { element })
            .collect::<Vec<_>>();
        
        for (interface, _, element) in not_impl_elements {
            let error = SimpleError::new(
                0060,
                interface.span.clone(),
                vec![],
                vec![
                    ((context.module_name.clone(), interface.span), Color::Red),
                    ((element.module_name, element.span), Color::Yellow)
                ]
            );
            errors.push(TranspileError::new(error));
        }
    }

}


pub enum NoOverrideElementError {
    NotFoundEqualsName { name: Spanned<String> },
    NotEqualsType { origin: WithDefineInfo<Type>, found: WithDefineInfo<Type> },
    ExtraBounds { target: Type, bounds: Vec<Arc<Bound>> }
}

impl NoOverrideElementError {
    pub(crate) fn collect(
        &self,
        override_keyword_span: Range<usize>,
        interface_span: Range<usize>,
        type_environment: &mut TypeEnvironment,
        errors: &mut Vec<TranspileError>,
        context: &TranspileModuleContext
    ) {
        let error = match self {
            NoOverrideElementError::NotFoundEqualsName { name  } => {
                TranspileError::new(SimpleError::new(
                    0057,
                    override_keyword_span,
                    vec![],
                    vec![
                        ((context.module_name.clone(), name.span.clone()), Color::Red),
                        ((context.module_name.clone(), interface_span), Color::Yellow)
                    ]
                ))
            },
            NoOverrideElementError::NotEqualsType { origin, found } => {
                TranspileError::new(SimpleError::new(
                    0058,
                    override_keyword_span,
                    vec![
                        (type_environment.get_type_display_string(&origin.value), Color::Yellow),
                        (type_environment.get_type_display_string(&found.value), Color::Red)
                    ],
                    vec![
                        ((origin.module_name.clone(), origin.span.clone()), Color::Yellow),
                        ((found.module_name.clone(), found.span.clone()), Color::Red)
                    ]
                ))
            },
            NoOverrideElementError::ExtraBounds { target, bounds } => {
                TranspileError::new(ExtraBoundsError {
                    override_keyword_span,
                    target: type_environment.get_type_display_string(target),
                    bounds: bounds.iter()
                        .map(|bound| {
                            Spanned::new(
                                type_environment.get_type_display_string(&bound.ty),
                                bound.span.clone()
                            )
                        })
                        .collect()
                })
            }
        };
        errors.push(error);
    }
}


struct ExtraBoundsError {
    override_keyword_span: Range<usize>,
    target: String,
    bounds: Vec<Spanned<String>>
}

impl TranspileReport for ExtraBoundsError {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = &context.module_name;
        let text = &context.context.localized_text;
        let error_code = 0059;
        let key = ErrorMessageKey::new(error_code);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(ReportKind::Error, module_name, self.override_keyword_span.start)
            .with_code(error_code)
            .with_message(message);

        for bound in self.bounds.iter() {
            let bound_str = format!(
                "{}: {}",
                self.target.clone().fg(Color::Yellow).to_string(),
                bound.value.clone().fg(Color::Red).to_string()
            );
            
            builder.add_label(
                Label::new((module_name, bound.span.clone()))
                    .with_color(Color::Red)
                    .with_message(
                        key.get_massage(text, ErrorMessageType::Label(0))
                            .replace("%bound", &bound_str)
                    )
            );
        }

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        builder.finish().print((module_name, Source::from(context.source_code.code.as_str()))).unwrap();
    }
}


#[derive(Debug, Clone)]
pub struct ScopeThisType {
    pub ty: Type,
    nest_count: usize
}

impl ScopeThisType {
    pub fn new(ty: Type) -> Self {
        Self { ty, nest_count: 0 }
    }
    
    pub fn nest(&self) -> Self {
        let nest_count = self.nest_count + 1;
        if nest_count >= 2 {
            Self { ty: Type::Unknown, nest_count: 0 }
        } else {
            Self { ty: self.ty.clone(), nest_count }
        }
    }
}
