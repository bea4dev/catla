use std::sync::{
    Arc, RwLock,
    atomic::{AtomicUsize, Ordering},
};

use catla_parser::ast::{EntityID, Spanned};
use catla_util::module_path::ModulePath;
use derivative::Derivative;
use hashbrown::HashMap;

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
    #[derivative(PartialEq="ignore")]
    pub bounds: RwLock<Vec<Type>>,
}
