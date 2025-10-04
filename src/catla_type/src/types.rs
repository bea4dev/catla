use std::sync::{
    Arc, RwLock,
    atomic::{AtomicUsize, Ordering},
};

use catla_parser::ast::{EntityID, Spanned};
use catla_util::module_path::ModulePath;
use derivative::Derivative;
use hashbrown::HashMap;
use indexmap::IndexMap;

use crate::type_infer::{TypeEnvironment, TypeVariableID};

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
    IntegerLiteral,
    FloatLiteral,
    UserType {
        user_type_info: GlobalUserTypeID,
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
    This,
    Unreachable,
    Unknown,
}

impl Type {
    pub fn is_integer(&self) -> bool {
        match self {
            Type::Int8 => true,
            Type::Int16 => true,
            Type::Int32 => true,
            Type::Int64 => true,
            Type::Uint8 => true,
            Type::Uint16 => true,
            Type::Uint32 => true,
            Type::Uint64 => true,
            Type::IntegerLiteral => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::Float32 => true,
            Type::Float64 => true,
            Type::FloatLiteral => true,
            _ => false,
        }
    }

    pub fn to_display_string(
        &self,
        user_type_set: &GlobalUserTypeSet,
        type_environment: Option<&TypeEnvironment>,
    ) -> String {
        match self {
            Type::Int8 => "int8".into(),
            Type::Int16 => "int16".into(),
            Type::Int32 => "int32".into(),
            Type::Int64 => "int64".into(),
            Type::Uint8 => "uint8".into(),
            Type::Uint16 => "uint16".into(),
            Type::Uint32 => "uint32".into(),
            Type::Uint64 => "uint64".into(),
            Type::Float32 => "float32".into(),
            Type::Float64 => "float64".into(),
            Type::Bool => "bool".into(),
            Type::Unit => "unit".into(),
            Type::IntegerLiteral => "int".into(),
            Type::FloatLiteral => "float".into(),
            Type::UserType {
                user_type_info,
                generics,
            } => {
                let user_type_info = user_type_set.get(*user_type_info);
                let user_type_name = user_type_info.read().unwrap().name.value.clone();

                if generics.is_empty() {
                    user_type_name
                } else {
                    format!(
                        "{}<{}>",
                        user_type_name,
                        generics
                            .iter()
                            .map(|generic| generic
                                .to_display_string(user_type_set, type_environment))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            Type::Function {
                function_info,
                generics,
            } => {
                let function_name = function_info.name.value.clone();

                if generics.is_empty() {
                    function_name
                } else {
                    format!(
                        "{}<{}>",
                        function_name,
                        generics
                            .iter()
                            .map(|generic| generic
                                .to_display_string(user_type_set, type_environment))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            Type::Generic(generic_type) => generic_type.name.value.clone(),
            Type::TypeVariable(type_variable_id) => match type_environment {
                Some(type_environment) => {
                    let ty = type_environment.get_type_variable_type(*type_variable_id);

                    match ty {
                        Some(ty) => ty
                            .value
                            .to_display_string(user_type_set, Some(type_environment)),
                        None => "Unknown".into(),
                    }
                }
                None => "_".into(),
            },
            Type::Array(base_type) => {
                format!(
                    "[{}]",
                    base_type.to_display_string(user_type_set, type_environment)
                )
            }
            Type::Tuple(items) => {
                format!(
                    "({})",
                    items
                        .iter()
                        .map(|item| item.to_display_string(user_type_set, type_environment))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Type::This => "This".into(),
            Type::Unreachable => "Unreachable".into(),
            Type::Unknown => "Unknown".into(),
        }
    }

    pub fn resolve_type_alias(&self, user_type_set: &GlobalUserTypeSet) -> Type {
        match self {
            Type::UserType {
                user_type_info,
                generics,
            } => {
                let user_type_info = user_type_set.get(*user_type_info);
                let user_type_info = user_type_info.read().unwrap();

                let replace_map = user_type_info
                    .generics
                    .iter()
                    .map(|generic| Type::Generic(generic.clone()))
                    .zip(generics.iter());

                let alias_type = user_type_info.element_type.get("").unwrap();

                match &alias_type.value {
                    Type::UserType {
                        user_type_info,
                        generics,
                    } => {
                        let mut new_generics = Vec::new();
                        for generic in generics.iter() {
                            let mut replaced = false;
                            for (before, after) in replace_map.clone() {
                                if generic == &before {
                                    new_generics.push(after.clone());
                                    replaced = true;
                                    break;
                                }
                            }

                            if !replaced {
                                new_generics.push(Type::Unknown);
                            }
                        }

                        Type::UserType {
                            user_type_info: *user_type_info,
                            generics: Arc::new(new_generics),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => self.clone(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalUserTypeID(usize);

#[derive(Debug, Clone)]
pub struct GlobalUserTypeSet {
    map: Arc<RwLock<HashMap<GlobalUserTypeID, Arc<RwLock<UserTypeInfo>>>>>,
    counter: Arc<AtomicUsize>,
}

impl GlobalUserTypeSet {
    pub fn new() -> Self {
        Self {
            map: Arc::new(RwLock::new(HashMap::new())),
            counter: Arc::new(AtomicUsize::new(0)),
        }
    }

    pub fn new_user_type(&self, user_type: UserTypeInfo) -> GlobalUserTypeID {
        let new_id = self.counter.fetch_add(1, Ordering::Relaxed);
        let new_id = GlobalUserTypeID(new_id);

        let mut lock = self.map.write().unwrap();
        lock.insert(new_id, Arc::new(RwLock::new(user_type)));

        new_id
    }

    pub fn get(&self, id: GlobalUserTypeID) -> Arc<RwLock<UserTypeInfo>> {
        let lock = self.map.read().unwrap();
        lock.get(&id).unwrap().clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct UserTypeInfo {
    pub module_path: ModulePath,
    pub name: Spanned<String>,
    pub is_alias: bool,
    pub element_type: HashMap<String, Spanned<Type>>,
    pub generics: Vec<Arc<GenericType>>,
    pub where_clause: Vec<WhereClauseInfo>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhereClauseInfo {
    pub target: Type,
    pub bounds: Vec<Type>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionTypeInfo {
    pub module_path: ModulePath,
    pub name: Spanned<String>,
    pub generics: Vec<Arc<GenericType>>,
    pub arguments: Vec<Type>,
    pub return_type: Type,
}

#[derive(Derivative)]
#[derivative(PartialEq, Eq)]
#[derive(Debug)]
pub struct GenericType {
    pub module_path: ModulePath,
    pub entity_id: EntityID,
    pub name: Spanned<String>,
    #[derivative(PartialEq = "ignore")]
    pub bounds: RwLock<Vec<Type>>,
}

#[derive(Debug)]
pub struct ImplementsInfoSet {
    map: IndexMap<EntityID, ImplementsInfo>,
}

impl ImplementsInfoSet {
    pub fn new() -> Self {
        Self {
            map: IndexMap::new(),
        }
    }

    pub fn register(&mut self, entity_id: EntityID, info: ImplementsInfo) {
        self.map.insert(entity_id, info);
    }
}

#[derive(Debug)]
pub struct ImplementsInfo {
    pub generics_define: Vec<Arc<GenericType>>,
    pub interface: Spanned<Type>,
    pub concrete: Spanned<Type>,
    pub where_clause: Vec<WhereClauseInfo>,
    pub element_type: HashMap<String, Spanned<Type>>,
    pub module_path: ModulePath,
}
