use std::{
    ops::Range,
    sync::{
        Arc, RwLock,
        atomic::{AtomicUsize, Ordering},
    },
};

use bumpalo::Bump;
use catla_parser::ast::{EntityID, Spanned, UserTypeKind, WithSpan};
use catla_util::module_path::{ModulePath, Moduled, ToModuled};
use derivative::Derivative;
use hashbrown::HashMap;

use crate::{
    error::{TypeError, TypeErrorKind},
    type_infer::{TypeEnvironment, TypeVariableID},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
                generics: _,
            } => {
                let arguments = function_info
                    .arguments
                    .iter()
                    .map(|argument| {
                        argument
                            .value
                            .to_display_string(user_type_set, type_environment)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                let return_type = function_info
                    .return_type
                    .value
                    .to_display_string(user_type_set, type_environment);

                format!("function ({}) -> {}", arguments, return_type)
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

                        let next = Type::UserType {
                            user_type_info: *user_type_info,
                            generics: Arc::new(new_generics),
                        };

                        return next.resolve_type_alias(user_type_set);
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
                    new_arguments.push(
                        argument.clone().map(|argument| {
                            argument.replace_generics(replace_before, replace_after)
                        }),
                    );
                }

                let new_return_type = function_info
                    .return_type
                    .clone()
                    .map(|return_type| return_type.replace_generics(replace_before, replace_after));

                let new_function_info = FunctionTypeInfo {
                    module_path: function_info.module_path.clone(),
                    name: function_info.name.as_ref().cloned(),
                    generics: function_info
                        .generics
                        .iter()
                        .map(|generic| {
                            GenericType::replace_generics(generic, replace_before, replace_after)
                        })
                        .collect(),
                    arguments: new_arguments,
                    return_type: new_return_type,
                    where_clause: function_info
                        .where_clause
                        .iter()
                        .map(|where_clause| {
                            where_clause.replace_generics(replace_before, replace_after)
                        })
                        .collect(),
                    span: function_info.span.clone(),
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
                Type::Generic(Arc::new(GenericType {
                    module_path: generic_type.module_path.clone(),
                    entity_id: generic_type.entity_id,
                    name: generic_type.name.clone(),
                    bounds: RwLock::new(
                        generic_type
                            .bounds
                            .read()
                            .unwrap()
                            .iter()
                            .map(|ty| {
                                ty.clone()
                                    .map(|ty| ty.replace_generics(replace_before, replace_after))
                            })
                            .collect(),
                    ),
                }))
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
    pub kind: Option<Spanned<UserTypeKind>>,
    pub module_path: ModulePath,
    pub name: Spanned<String>,
    pub is_alias: bool,
    pub element_types: HashMap<String, Spanned<Type>>,
    pub generics: Vec<Arc<GenericType>>,
    pub where_clause: Vec<WhereClauseInfo>,
    pub span: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct WhereClauseInfo {
    pub target: Spanned<Type>,
    pub bounds: Vec<Spanned<Type>>,
}

impl WhereClauseInfo {
    pub fn replace_generics(
        &self,
        replace_before: &[Arc<GenericType>],
        replace_after: &[Type],
    ) -> Self {
        Self {
            target: self
                .target
                .clone()
                .map(|ty| ty.replace_generics(replace_before, replace_after)),
            bounds: self
                .bounds
                .iter()
                .map(|ty| {
                    ty.clone()
                        .map(|ty| ty.replace_generics(replace_before, replace_after))
                })
                .collect(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FunctionTypeInfo {
    pub module_path: ModulePath,
    pub name: Option<Spanned<String>>,
    pub generics: Vec<Arc<GenericType>>,
    pub arguments: Vec<Spanned<Type>>,
    pub return_type: Spanned<Type>,
    pub where_clause: Vec<WhereClauseInfo>,
    pub span: Range<usize>,
}

#[derive(Derivative)]
#[derivative(PartialEq, Eq, Hash)]
#[derive(Debug)]
pub struct GenericType {
    pub module_path: ModulePath,
    pub entity_id: EntityID,
    pub name: Spanned<String>,
    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    pub bounds: RwLock<Vec<Spanned<Type>>>,
}

impl GenericType {
    pub fn replace_generics(
        generic_type: &Arc<GenericType>,
        replace_before: &[Arc<GenericType>],
        replace_after: &[Type],
    ) -> Arc<GenericType> {
        for (before, after) in replace_before.iter().zip(replace_after.iter()) {
            if before.module_path == generic_type.module_path
                && before.entity_id == generic_type.entity_id
            {
                // replace generic only for extra bounds check
                if let Type::Generic(generic) = after {
                    return generic.clone();
                }
            }
        }
        Arc::new(GenericType {
            module_path: generic_type.module_path.clone(),
            entity_id: generic_type.entity_id,
            name: generic_type.name.clone(),
            bounds: RwLock::new(
                generic_type
                    .bounds
                    .read()
                    .unwrap()
                    .iter()
                    .map(|ty| {
                        ty.clone()
                            .map(|ty| ty.replace_generics(replace_before, replace_after))
                    })
                    .collect(),
            ),
        })
    }
}

#[derive(Debug)]
pub struct ImplementsElementChecker {
    implements: Arc<RwLock<Vec<Arc<ImplementsInfo>>>>,
}

impl ImplementsElementChecker {
    pub fn new() -> Self {
        Self {
            implements: Arc::new(RwLock::new(Vec::new())),
        }
    }

    pub fn register_implements(&self, implements_infos: &ImplementsInfoSet) {
        let mut implements = self.implements.write().unwrap();

        for implements_info in implements_infos.infos.read().unwrap().iter() {
            if !implements_info.is_instant {
                implements.push(implements_info.clone());
            }
        }
    }

    pub fn check(
        &self,
        user_type_set: &GlobalUserTypeSet,
        global_implements_infos: &ImplementsInfoSet,
        errors: &mut Vec<TypeError>,
    ) {
        let implements = self.implements.read().unwrap();

        let temp_allocator = Bump::new();
        let mut temp_type_environment = TypeEnvironment::new(&ModulePath::dummy(), &temp_allocator);

        let implements_infos = ImplementsInfoSet::new(Some(global_implements_infos));

        for implements in implements.iter() {
            let implements_infos = ImplementsInfoSet::new(Some(&implements_infos));
            for generic in implements.generics_define.iter() {
                for bound in generic.bounds.read().unwrap().iter() {
                    implements_infos.register_implements_info(ImplementsInfo {
                        generics_define: Vec::new(),
                        interface: bound.clone(),
                        concrete: Type::Generic(generic.clone())
                            .with_span(generic.name.span.clone()),
                        where_clause: Vec::new(),
                        element_type: HashMap::new(),
                        module_path: generic.module_path.clone(),
                        is_instant: true,
                    });
                }
            }

            for where_bound in implements.where_clause.iter() {
                let target_type = where_bound.target.clone();

                for bound in where_bound.bounds.iter() {
                    implements_infos.register_implements_info(ImplementsInfo {
                        generics_define: Vec::new(),
                        interface: bound.clone(),
                        concrete: target_type.clone(),
                        where_clause: Vec::new(),
                        element_type: HashMap::new(),
                        module_path: implements.module_path.clone(),
                        is_instant: true,
                    });
                }
            }

            let interface_type_id = match &implements.interface.value {
                Type::UserType {
                    user_type_info,
                    generics: _,
                } => *user_type_info,
                _ => {
                    let error = TypeError {
                        kind: TypeErrorKind::NotInterfaceInImplements,
                        span: implements.interface.span.clone(),
                        module_path: implements.module_path.clone(),
                    };
                    errors.push(error);
                    continue;
                }
            };

            let interface_info = user_type_set.get(interface_type_id);
            let interface_info = interface_info.read().unwrap();

            if interface_info
                .kind
                .as_ref()
                .expect("Unresolved type alias")
                .value
                != UserTypeKind::Interface
            {
                let error = TypeError {
                    kind: TypeErrorKind::NotInterfaceInImplements,
                    span: implements.interface.span.clone(),
                    module_path: implements.module_path.clone(),
                };
                errors.push(error);
                continue;
            }

            let Type::UserType {
                user_type_info: interface_type_info,
                generics: interface_generics,
            } = &implements.interface.value
            else {
                continue;
            };

            let interface_type_info = user_type_set.get(*interface_type_info);
            let interface_type_info = interface_type_info.read().unwrap();

            for generic in interface_type_info.generics.iter() {
                for bound in generic.bounds.read().unwrap().iter() {
                    implements_infos.register_implements_info(ImplementsInfo {
                        generics_define: Vec::new(),
                        interface: bound.clone(),
                        concrete: Type::Generic(generic.clone())
                            .with_span(generic.name.span.clone()),
                        where_clause: Vec::new(),
                        element_type: HashMap::new(),
                        module_path: generic.module_path.clone(),
                        is_instant: true,
                    });
                }
            }

            for where_bound in interface_type_info.where_clause.iter() {
                let target_type = where_bound.target.clone();

                for bound in where_bound.bounds.iter() {
                    implements_infos.register_implements_info(ImplementsInfo {
                        generics_define: Vec::new(),
                        interface: bound.clone(),
                        concrete: target_type.clone(),
                        where_clause: Vec::new(),
                        element_type: HashMap::new(),
                        module_path: interface_type_info.module_path.clone(),
                        is_instant: true,
                    });
                }
            }

            let interface_old_generics = &interface_type_info.generics;
            let interface_new_generics = interface_generics;

            for (origin_element_name, origin_element) in interface_info.element_types.iter() {
                let Some(impl_element) = implements.element_type.get(origin_element_name) else {
                    let error = TypeError {
                        kind: TypeErrorKind::NoElementFoundInImplements {
                            element: origin_element_name.to_string().moduled(
                                interface_info.module_path.clone(),
                                origin_element.span.clone(),
                            ),
                        },
                        span: implements.concrete.span.clone(),
                        module_path: implements.module_path.clone(),
                    };
                    errors.push(error);
                    continue;
                };

                let Type::Function {
                    function_info: origin_function_info,
                    generics: _,
                } = &origin_element.value
                else {
                    continue;
                };

                let Type::Function {
                    function_info: impl_function_info,
                    generics: _,
                } = &impl_element.value
                else {
                    continue;
                };

                let old_generics = &origin_function_info.generics;
                let new_generics = impl_function_info
                    .generics
                    .iter()
                    .map(|generic| Type::Generic(generic.clone()))
                    .collect::<Vec<_>>();

                let replaced_origin_element = origin_element
                    .value
                    .replace_generics(interface_old_generics, interface_new_generics)
                    .replace_generics(old_generics, &new_generics);

                if !temp_type_environment.test_unify_type(
                    impl_element
                        .value
                        .clone()
                        .moduled(ModulePath::dummy(), 0..0),
                    replaced_origin_element.moduled(ModulePath::dummy(), 0..0),
                    user_type_set,
                ) {
                    let error = TypeError {
                        kind: TypeErrorKind::TypeMismatchInImplements {
                            origin: origin_element
                                .value
                                .to_display_string(user_type_set, None)
                                .moduled(
                                    origin_function_info.module_path.clone(),
                                    origin_function_info.span.clone(),
                                ),
                            impls: impl_element
                                .value
                                .to_display_string(user_type_set, None)
                                .moduled(
                                    impl_function_info.module_path.clone(),
                                    impl_function_info.span.clone(),
                                ),
                        },
                        span: impl_function_info.span.clone(),
                        module_path: impl_function_info.module_path.clone(),
                    };
                    errors.push(error);
                    continue;
                }

                let old_generics = &impl_function_info.generics;
                let new_generics = origin_function_info
                    .generics
                    .iter()
                    .map(|generic| Type::Generic(generic.clone()))
                    .collect::<Vec<_>>();

                let replaced_impl_function = impl_element
                    .value
                    .replace_generics(old_generics, &new_generics);

                let implements_infos = ImplementsInfoSet::new(Some(&implements_infos));

                for generic in origin_function_info.generics.iter() {
                    for bound in generic.bounds.read().unwrap().iter() {
                        implements_infos.register_implements_info(ImplementsInfo {
                            generics_define: Vec::new(),
                            interface: bound.clone(),
                            concrete: Type::Generic(generic.clone())
                                .with_span(generic.name.span.clone()),
                            where_clause: Vec::new(),
                            element_type: HashMap::new(),
                            module_path: generic.module_path.clone(),
                            is_instant: true,
                        });
                    }
                }

                for where_bound in origin_function_info.where_clause.iter() {
                    let target_type = where_bound.target.clone();

                    for bound in where_bound.bounds.iter() {
                        implements_infos.register_implements_info(ImplementsInfo {
                            generics_define: Vec::new(),
                            interface: bound.clone(),
                            concrete: target_type.clone(),
                            where_clause: Vec::new(),
                            element_type: HashMap::new(),
                            module_path: origin_function_info.module_path.clone(),
                            is_instant: true,
                        });
                    }
                }

                let Type::Function {
                    function_info: replaced_impl_function_info,
                    generics: _,
                } = replaced_impl_function
                else {
                    continue;
                };

                let mut has_extra_bounds = false;
                for generic in replaced_impl_function_info.generics.iter() {
                    for bound in generic.bounds.read().unwrap().iter() {
                        match implements_infos.is_implements(
                            Type::Generic(generic.clone()).moduled(ModulePath::dummy(), 0..0),
                            bound.value.clone().moduled(ModulePath::dummy(), 0..0),
                            None,
                            user_type_set,
                            &mut temp_type_environment,
                        ) {
                            ImplementsCheckResult::NoImplementsFound => {
                                has_extra_bounds = true;
                                break;
                            }
                            ImplementsCheckResult::Conflicts { conflicts: _ } => {
                                unreachable!("conflict in extra bounds!")
                            }
                            ImplementsCheckResult::Success {
                                new_concrete: _,
                                new_interface: _,
                                new_generics: _,
                            } => continue,
                        }
                    }
                }

                if !has_extra_bounds {
                    for where_bound in replaced_impl_function_info.where_clause.iter() {
                        let target_type = where_bound
                            .target
                            .value
                            .clone()
                            .moduled(ModulePath::dummy(), 0..0);

                        for bound in where_bound.bounds.iter() {
                            match implements_infos.is_implements(
                                target_type.clone(),
                                bound.value.clone().moduled(ModulePath::dummy(), 0..0),
                                None,
                                user_type_set,
                                &mut temp_type_environment,
                            ) {
                                ImplementsCheckResult::NoImplementsFound => {
                                    has_extra_bounds = true;
                                    break;
                                }
                                ImplementsCheckResult::Conflicts { conflicts: _ } => {
                                    unreachable!("conflict in extra bounds!")
                                }
                                ImplementsCheckResult::Success {
                                    new_concrete: _,
                                    new_interface: _,
                                    new_generics: _,
                                } => continue,
                            }
                        }
                    }
                }

                if has_extra_bounds {
                    let error = TypeError {
                        kind: TypeErrorKind::ExtraBoundsInImplements {
                            origin: ().moduled(
                                origin_function_info.module_path.clone(),
                                origin_function_info.span.clone(),
                            ),
                            impls: ().moduled(
                                impl_function_info.module_path.clone(),
                                impl_function_info.span.clone(),
                            ),
                        },
                        span: impl_function_info.span.clone(),
                        module_path: impl_function_info.module_path.clone(),
                    };
                    errors.push(error);
                }
            }

            for (impl_element_name, impl_element) in implements.element_type.iter() {
                if !interface_info.element_types.contains_key(impl_element_name) {
                    let error = TypeError {
                        kind: TypeErrorKind::UnknownImplementsElement {
                            interface: interface_info.name.value.to_string().moduled(
                                interface_info.module_path.clone(),
                                interface_info.span.clone(),
                            ),
                            impl_element: impl_element_name
                                .to_string()
                                .moduled(implements.module_path.clone(), impl_element.span.clone()),
                        },
                        span: impl_element.span.clone(),
                        module_path: implements.module_path.clone(),
                    };
                    errors.push(error);
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImplementsInfoSet {
    infos: Arc<RwLock<Vec<Arc<ImplementsInfo>>>>,
}

impl ImplementsInfoSet {
    pub fn new(parent: Option<&ImplementsInfoSet>) -> Self {
        let mut infos = Vec::new();
        if let Some(parent) = parent {
            infos.extend(parent.infos.read().unwrap().clone());
        }

        Self {
            infos: Arc::new(RwLock::new(infos)),
        }
    }

    pub fn register_implements_info(&self, implements_info: ImplementsInfo) {
        self.infos.write().unwrap().push(Arc::new(implements_info));
    }

    pub fn find_implements_for(
        &self,
        ty: Moduled<Type>,
        function_name: &str,
        arguments: &[Moduled<Type>],
        user_type_set: &GlobalUserTypeSet,
        type_environment: &mut TypeEnvironment,
    ) -> Vec<(Arc<ImplementsInfo>, ImplementsCheckResult)> {
        let mut results = Vec::new();

        for implements_info in self.infos.read().unwrap().iter() {
            let result = implements_info.is_implements(
                ty.clone(),
                None,
                Some((function_name, arguments)),
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
        function_type_hints: Option<(&str, &[Moduled<Type>])>,
        user_type_set: &GlobalUserTypeSet,
        type_environment: &mut TypeEnvironment,
    ) -> ImplementsCheckResult {
        let mut results = Vec::new();

        for implements_info in self.infos.read().unwrap().iter() {
            let result = implements_info.is_implements(
                concrete.clone(),
                Some(interface.clone()),
                function_type_hints,
                user_type_set,
                type_environment,
                self,
            );

            match &result {
                ImplementsCheckResult::NoImplementsFound => continue,
                ImplementsCheckResult::Conflicts { conflicts: _ } => return result,
                ImplementsCheckResult::Success {
                    new_concrete: _,
                    new_interface: _,
                    new_generics: _,
                } => {
                    results.push(result);
                }
            }
        }

        match results.len() {
            0 => ImplementsCheckResult::NoImplementsFound,
            1 => results.pop().unwrap(),
            _ => ImplementsCheckResult::Conflicts {
                conflicts: results
                    .into_iter()
                    .map(|result| {
                        let ImplementsCheckResult::Success {
                            new_concrete,
                            new_interface: _,
                            new_generics: _,
                        } = result
                        else {
                            unreachable!()
                        };
                        new_concrete
                    })
                    .collect(),
            },
        }
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
    pub is_instant: bool,
}

impl ImplementsInfo {
    pub fn is_implements(
        &self,
        concrete: Moduled<Type>,
        interface: Option<Moduled<Type>>,
        function_type_hints: Option<(&str, &[Moduled<Type>])>,
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

        if let Some((function_name, arguments)) = function_type_hints {
            let retake_arguments = arguments
                .iter()
                .map(|ty| {
                    ty.clone()
                        .map(|ty| type_environment.retake_type_variable(&ty))
                })
                .collect::<Vec<_>>();

            let element_type = match self.element_type.get(function_name) {
                Some(element_type) => element_type.value.clone(),
                None => {
                    if !self.is_instant {
                        return ImplementsCheckResult::NoImplementsFound;
                    }

                    let Type::UserType {
                        user_type_info,
                        generics: new_generics,
                    } = &self.interface.value
                    else {
                        return ImplementsCheckResult::NoImplementsFound;
                    };

                    let user_type_info = user_type_set.get(*user_type_info);
                    let user_type_info = user_type_info.read().unwrap();

                    let old_generics = &user_type_info.generics;

                    let Some(element_type) = user_type_info.element_types.get(function_name) else {
                        return ImplementsCheckResult::NoImplementsFound;
                    };

                    element_type
                        .value
                        .replace_generics(old_generics, new_generics)
                }
            };

            let new_element_type = element_type.replace_generics(&old_generics, &new_generics);

            let Type::Function {
                function_info,
                generics: _,
            } = new_element_type
            else {
                return ImplementsCheckResult::NoImplementsFound;
            };

            for (argument, retake_argument) in function_info
                .arguments
                .iter()
                .zip(retake_arguments.into_iter())
            {
                type_environment.test_unify_type(
                    argument
                        .value
                        .clone()
                        .moduled(self.module_path.clone(), argument.span.clone()),
                    retake_argument,
                    user_type_set,
                );
            }
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
                    None,
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
