use std::{mem::swap, ops::Range, sync::{Arc, Mutex, MutexGuard, PoisonError}};

use bumpalo::Bump;
use catla_parser::parser::{Spanned, UserTypeKindEnum};
use derivative::Derivative;
use either::Either;
use fxhash::FxHashMap;

use crate::transpiler::component::EntityID;

use super::type_inference::TypeEnvironment;


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
    UserType {
        user_type_info: Arc<DataStructInfo>,
        generics: Arc<Vec<Type>>,
        #[derivative(PartialEq="ignore")]
        generics_span: Option<Arc<Vec<Range<usize>>>>
    },
    Function{ function_info: Arc<FunctionType>, generics: Arc<Vec<Type>> },
    Generic(Arc<GenericType>),
    LocalGeneric(LocalGenericID),
    Option(Arc<Type>),
    Result { value: Arc<Type>, error: Arc<Type> },
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

                let where_bounds = Type::get_where_bounds_with_generics(
                    &function_info.where_bounds.freeze_and_get(),
                    generics_define,
                    replace_generics
                );

                let function_info = FunctionType {
                    is_extension: function_info.is_extension,
                    generics_define: function_info.generics_define.clone(),
                    argument_types,
                    return_type: Spanned::new(return_type, function_info.return_type.span.clone()),
                    define_info: function_info.define_info.clone(),
                    where_bounds: FreezableMutex::new(where_bounds)
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
    pub module_name: Arc<String>,
    pub name: Spanned<String>,
    pub define_span: Range<usize>,
    pub kind: UserTypeKindEnum,
    pub generics_define: Vec<Arc<GenericType>>,
    pub generics_define_span: Option<Range<usize>>,
    pub element_types: Mutex<FxHashMap<String, WithDefineInfo<Type>>>,
    pub where_bounds: FreezableMutex<Vec<WhereBound>>
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
}


#[derive(Debug)]
pub struct GenericType {
    pub(crate) define_entity_id: EntityID,
    pub name: String,
    pub bounds: FreezableMutex<Vec<Arc<Bound>>>
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
#[derivative(Debug, PartialEq, Eq)]
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
    pub element_types: Arc<FxHashMap<String, WithDefineInfo<Type>>>
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

        if let Type::LocalGeneric(generic_id) = ty {
            let (_, ty) = type_environment.resolve_generic_type(*generic_id);

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
                
                for bound in self_generic_type.bounds.freeze_and_get().iter() {
                    if !global_implements_info_set.is_implemented(
                        ty,
                        &bound.ty,
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
            Type::Unknown => false
        }
    }

}


#[derive(Debug)]
pub struct ImplementsInfoSet {
    pub(crate) implements_infos: FxHashMap<EntityID, ImplementsInfo>
}


impl ImplementsInfoSet {
    
    pub fn new() -> Self {
        Self {
            implements_infos: FxHashMap::default()
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

        let empty_map = FxHashMap::default();
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
                        None,
                        &None
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

                // give answer to resolving generic variables
                let result1 = type_environment.unify_type(
                    &impl_concrete,
                    &(0..0),
                    &resolved_ty,
                    &(0..0),
                    allow_unknown
                );
                let result2 = type_environment.unify_type(
                    &impl_interface,
                    &(0..0),
                    &resolved_interface,
                    &(0..0),
                    allow_unknown
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

        let empty_map = FxHashMap::default();
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
                let generic_id = type_environment.new_local_generic_id(0..0, None, &None);
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

            // give answer to resolving generic variables
            let result = type_environment.unify_type(
                &impl_concrete,
                &(0..0),
                &resolved_ty,
                &(0..0),
                true
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
                    element_types: implements_info.element_types.clone()
                };

                satisfied_implementations.push(CollectedImplementation {
                    implements_info,
                    local_generics
                })
            }
        }

        satisfied_implementations
    }

}


pub(crate) struct CollectedImplementation {
    pub implements_info: ImplementsInfo,
    pub local_generics: Vec<Type>
}