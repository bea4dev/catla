use std::{
    ops::Range,
    sync::{
        Arc, RwLock,
        atomic::{AtomicUsize, Ordering},
    },
};

use catla_parser::ast::{EntityID, Spanned};
use catla_util::module_path::{ModulePath, Moduled};
use derivative::Derivative;
use hashbrown::HashMap;
use indexmap::IndexMap;

use crate::{
    error::{TypeError, TypeErrorKind},
    type_infer::{TypeEnvironment, TypeVariableID},
};

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
                let arguments = function_info
                    .arguments
                    .iter()
                    .map(|argument| argument.to_display_string(user_type_set, type_environment))
                    .collect::<Vec<_>>()
                    .join(", ");

                let return_type = function_info
                    .return_type
                    .to_display_string(user_type_set, type_environment);

                match generics.is_empty() {
                    true => format!("function ({}) -> {}", arguments, return_type),
                    false => format!(
                        "function <{}>({}) -> {}",
                        generics
                            .iter()
                            .map(|generic| generic
                                .to_display_string(user_type_set, type_environment))
                            .collect::<Vec<_>>()
                            .join(", "),
                        arguments,
                        return_type
                    ),
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

                if !user_type_info.is_alias {
                    return self.clone();
                }

                let alias_type = user_type_info.element_types.get("").unwrap();

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

    pub fn replace_generics(
        &self,
        replace_before: &[Arc<GenericType>],
        replace_after: &[Type],
    ) -> Type {
        match self {
            Type::UserType {
                user_type_info,
                generics,
            } => {
                let mut new_generics = Vec::with_capacity(generics.len());

                for generic in generics.iter() {
                    new_generics.push(generic.replace_generics(replace_before, replace_after));
                }

                Type::UserType {
                    user_type_info: *user_type_info,
                    generics: Arc::new(new_generics),
                }
            }
            Type::Function {
                function_info,
                generics,
            } => {
                let mut new_generics = Vec::with_capacity(generics.len());

                for generic in generics.iter() {
                    new_generics.push(generic.replace_generics(replace_before, replace_after));
                }

                let mut new_arguments = Vec::with_capacity(function_info.arguments.len());

                for argument in function_info.arguments.iter() {
                    new_arguments.push(argument.replace_generics(replace_before, replace_after));
                }

                let new_return_type = function_info
                    .return_type
                    .replace_generics(replace_before, replace_after);

                let new_function_info = FunctionTypeInfo {
                    module_path: function_info.module_path.clone(),
                    name: function_info.name.as_ref().cloned(),
                    generics: function_info.generics.clone(),
                    arguments: new_arguments,
                    return_type: new_return_type,
                };

                Type::Function {
                    function_info: Arc::new(new_function_info),
                    generics: Arc::new(new_generics),
                }
            }
            Type::Generic(generic_type) => {
                for (before, after) in replace_before.iter().zip(replace_after.iter()) {
                    if before.module_path == generic_type.module_path
                        && before.entity_id == generic_type.entity_id
                    {
                        return after.clone();
                    }
                }
                return self.clone();
            }
            Type::Array(base_type) => Type::Array(Arc::new(
                base_type.replace_generics(replace_before, replace_after),
            )),
            Type::Tuple(items) => {
                let mut new_items = Vec::with_capacity(items.len());

                for item in items.iter() {
                    new_items.push(item.replace_generics(replace_before, replace_after));
                }

                Type::Tuple(Arc::new(new_items.clone()))
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
    generics_check: Arc<RwLock<Vec<Moduled<Type>>>>,
    counter: Arc<AtomicUsize>,
}

impl GlobalUserTypeSet {
    pub fn new() -> Self {
        Self {
            map: Arc::new(RwLock::new(HashMap::new())),
            generics_check: Arc::new(RwLock::new(Vec::new())),
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

    pub fn add_generics_count_check(&self, ty: Moduled<Type>) {
        let mut checks = self.generics_check.write().unwrap();
        checks.push(ty);
    }

    pub fn check_generics_count(&self, errors: &mut Vec<TypeError>) {
        let checks = self.generics_check.read().unwrap();

        for check_type in checks.iter() {
            if let Type::UserType {
                user_type_info,
                generics,
            } = &check_type.value
            {
                let user_type_info = self.get(*user_type_info);
                let user_type_info = user_type_info.read().unwrap();

                if user_type_info.generics.len() != generics.len() {
                    let error = TypeError {
                        kind: TypeErrorKind::InvalidGenericsCount {
                            expected: user_type_info.generics.len(),
                            found: generics.len(),
                            defined: Moduled::new(
                                (),
                                user_type_info.module_path.clone(),
                                user_type_info.span.clone(),
                            ),
                        },
                        span: check_type.span.clone(),
                        module_path: check_type.module_path.clone(),
                    };
                    errors.push(error);
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct UserTypeInfo {
    pub module_path: ModulePath,
    pub name: Spanned<String>,
    pub is_alias: bool,
    pub element_types: HashMap<String, Spanned<Type>>,
    pub generics: Vec<Arc<GenericType>>,
    pub where_clause: Vec<WhereClauseInfo>,
    pub span: Range<usize>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhereClauseInfo {
    pub target: Spanned<Type>,
    pub bounds: Vec<Spanned<Type>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionTypeInfo {
    pub module_path: ModulePath,
    pub name: Option<Spanned<String>>,
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
    map: IndexMap<EntityID, Arc<ImplementsInfo>>,
}

impl ImplementsInfoSet {
    pub fn new() -> Self {
        Self {
            map: IndexMap::new(),
        }
    }

    pub fn register(&mut self, entity_id: EntityID, info: ImplementsInfo) {
        self.map.insert(entity_id, Arc::new(info));
    }

    pub fn find_implements_for(
        &self,
        ty: Moduled<Type>,
        user_type_set: &GlobalUserTypeSet,
        type_environment: &mut TypeEnvironment,
    ) -> Vec<(Arc<ImplementsInfo>, ImplementsCheckResult)> {
        let mut results = Vec::new();

        for implements_info in self.map.values() {
            let result = implements_info.is_implements(
                ty.clone(),
                None,
                user_type_set,
                type_environment,
                self,
            );

            match &result {
                ImplementsCheckResult::NoImplementsFound => continue,
                ImplementsCheckResult::Conflicts { conflicts: _ }
                | ImplementsCheckResult::Success {
                    new_concrete: _,
                    new_interface: _,
                    new_generics: _,
                } => results.push((implements_info.clone(), result)),
            }
        }

        results
    }

    pub fn is_implements(
        &self,
        concrete: Moduled<Type>,
        interface: Moduled<Type>,
        user_type_set: &GlobalUserTypeSet,
        type_environment: &mut TypeEnvironment,
    ) -> ImplementsCheckResult {
        println!("1");
        for implements_info in self.map.values() {
            let result = implements_info.is_implements(
                concrete.clone(),
                Some(interface.clone()),
                user_type_set,
                type_environment,
                self,
            );

            match &result {
                ImplementsCheckResult::NoImplementsFound => continue,
                ImplementsCheckResult::Conflicts { conflicts: _ }
                | ImplementsCheckResult::Success {
                    new_concrete: _,
                    new_interface: _,
                    new_generics: _,
                } => return result,
            }
        }

        ImplementsCheckResult::NoImplementsFound
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

impl ImplementsInfo {
    pub fn is_implements(
        &self,
        concrete: Moduled<Type>,
        interface: Option<Moduled<Type>>,
        user_type_set: &GlobalUserTypeSet,
        type_environment: &mut TypeEnvironment,
        all_implements_info_set: &ImplementsInfoSet,
    ) -> ImplementsCheckResult {
        let retake_concrete = concrete.map(|ty| type_environment.retake_type_variable(&ty));

        let retake_interface =
            interface.map(|ty| ty.map(|ty| type_environment.retake_type_variable(&ty)));

        let old_generics = &self.generics_define;
        let new_generics = (0..old_generics.len())
            .map(|_| Type::TypeVariable(type_environment.create_type_variable_id_with_set()))
            .collect::<Vec<_>>();

        let new_concrete = self
            .concrete
            .value
            .replace_generics(&old_generics, &new_generics);

        let new_interface = self
            .interface
            .value
            .replace_generics(&old_generics, &new_generics);

        if !type_environment.test_unify_type(
            Moduled::new(
                new_concrete,
                self.module_path.clone(),
                self.concrete.span.clone(),
            ),
            retake_concrete.clone(),
            user_type_set,
        ) {
            return ImplementsCheckResult::NoImplementsFound;
        }

        if let Some(retake_interface) = &retake_interface {
            if !type_environment.test_unify_type(
                Moduled::new(
                    new_interface,
                    self.module_path.clone(),
                    self.interface.span.clone(),
                ),
                retake_interface.clone(),
                user_type_set,
            ) {
                return ImplementsCheckResult::NoImplementsFound;
            }
        }

        for where_bounds in self.where_clause.iter() {
            let new_target = where_bounds
                .target
                .value
                .replace_generics(&old_generics, &new_generics);

            for bound in where_bounds.bounds.iter() {
                let new_bound = bound.value.replace_generics(&old_generics, &new_generics);

                match all_implements_info_set.is_implements(
                    Moduled::new(
                        new_target.clone(),
                        self.module_path.clone(),
                        where_bounds.target.span.clone(),
                    ),
                    Moduled::new(
                        new_bound.clone(),
                        self.module_path.clone(),
                        bound.span.clone(),
                    ),
                    user_type_set,
                    type_environment,
                ) {
                    ImplementsCheckResult::NoImplementsFound => {
                        return ImplementsCheckResult::NoImplementsFound;
                    }
                    ImplementsCheckResult::Conflicts { conflicts } => {
                        return ImplementsCheckResult::Conflicts { conflicts };
                    }
                    ImplementsCheckResult::Success {
                        new_concrete,
                        new_interface,
                        new_generics: _,
                    } => {
                        let target_result = type_environment.test_unify_type(
                            Moduled::new(
                                new_target.clone(),
                                self.module_path.clone(),
                                where_bounds.target.span.clone(),
                            ),
                            new_concrete,
                            user_type_set,
                        );
                        if !target_result {
                            unreachable!("target unify must not fail!");
                        }

                        let bound_result = type_environment.test_unify_type(
                            Moduled::new(new_bound, self.module_path.clone(), bound.span.clone()),
                            new_interface.unwrap(),
                            user_type_set,
                        );
                        if !bound_result {
                            unreachable!("bound unify must not fail!");
                        }
                    }
                }
            }
        }

        ImplementsCheckResult::Success {
            new_concrete: retake_concrete,
            new_interface: retake_interface,
            new_generics,
        }
    }
}

#[derive(Debug)]
pub enum ImplementsCheckResult {
    NoImplementsFound,
    Conflicts {
        conflicts: Vec<Moduled<Type>>,
    },
    Success {
        new_concrete: Moduled<Type>,
        new_interface: Option<Moduled<Type>>,
        new_generics: Vec<Type>,
    },
}
