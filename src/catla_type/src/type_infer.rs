use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_import::{ImportElement, resource::PackageResourceSet};
use catla_name_resolver::{DefineKind, ResolvedInfo};
use catla_parser::ast::{
    AddOrSubExpression, AndExpression, Define, EntityID, EqualsExpression, Expression, Factor,
    LessOrGreaterExpression, Literal, MulOrDivExpression, NewObjectExpression, OrExpression,
    Primary, PrimaryLeftExpr, PrimarySeparator, Program, SimplePrimary, Spanned, Statement,
    VariableBinding,
};
use catla_util::module_path::{ModulePath, Moduled};
use hashbrown::{DefaultHashBuilder, HashMap, HashSet};
use std::{mem::swap, sync::Arc};

use crate::{
    error::{TypeError, TypeErrorKind},
    types::{
        FunctionTypeInfo, GenericType, GlobalUserTypeID, GlobalUserTypeSet, ImplementsInfoSet, Type,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVariableID(usize);

/// (Hindley-Milner based) Union-Find like type inference
///
/// ```catla
/// let a = 100;
/// let b = a;
/// let c = b;
/// ```
///
/// set(a, b, c): int
///
///           │
///           ▼
///
/// a: int | b: int | c: int
#[derive(Debug)]
pub struct TypeEnvironment<'type_env_alloc> {
    module_path: ModulePath,
    entity_var_map: HashMap<EntityID, TypeVariableID, DefaultHashBuilder, &'type_env_alloc Bump>,
    type_variable_set_components:
        Vec<&'type_env_alloc mut TypeVariableSet<'type_env_alloc>, &'type_env_alloc Bump>,
    var_set_map:
        HashMap<TypeVariableID, TypeVariableSetID, DefaultHashBuilder, &'type_env_alloc Bump>,
    type_variable_id_counter: usize,
}

impl<'type_env_alloc> TypeEnvironment<'type_env_alloc> {
    pub fn new(module_path: &ModulePath, allocator: &'type_env_alloc Bump) -> Self {
        Self {
            module_path: module_path.clone(),
            entity_var_map: HashMap::new_in(allocator),
            type_variable_set_components: Vec::new_in(allocator),
            var_set_map: HashMap::new_in(allocator),
            type_variable_id_counter: 0,
        }
    }

    fn get_or_allocate_variable_for_entity(&mut self, entity_id: EntityID) -> TypeVariableID {
        match self.entity_var_map.get(&entity_id) {
            Some(id) => *id,
            None => {
                let type_variable_id = self.create_type_variable_id_without_set();
                self.create_variable_set(type_variable_id);

                self.entity_var_map.insert(entity_id, type_variable_id);

                type_variable_id
            }
        }
    }

    fn create_and_set_entity_type(
        &mut self,
        entity_id: EntityID,
        ty: Moduled<Type>,
    ) -> TypeVariableID {
        let type_variable_id = self.get_or_allocate_variable_for_entity(entity_id);
        let type_variable_set_id = self.var_set_map.get(&type_variable_id).unwrap();
        let type_variable_set = &mut self.type_variable_set_components[type_variable_set_id.0];

        type_variable_set.ty = Some(ty);

        type_variable_id
    }

    fn get_entity_type(&self, entity_id: EntityID) -> Option<Moduled<Type>> {
        let type_variable_id = self.entity_var_map.get(&entity_id).unwrap();
        let type_variable_set_id = self.var_set_map.get(type_variable_id).unwrap();
        let type_variable_set = &self.type_variable_set_components[type_variable_set_id.0];

        type_variable_set.ty.as_ref().cloned()
    }

    pub(crate) fn get_type_variable_type(
        &self,
        type_variable_id: TypeVariableID,
    ) -> Option<Moduled<Type>> {
        let type_variable_set_id = self.var_set_map.get(&type_variable_id).unwrap();
        let type_variable_set = &self.type_variable_set_components[type_variable_set_id.0];

        type_variable_set.ty.as_ref().cloned()
    }

    fn create_type_variable_id_without_set(&mut self) -> TypeVariableID {
        let old_id = self.type_variable_id_counter;
        self.type_variable_id_counter += 1;
        TypeVariableID(old_id)
    }

    pub fn create_type_variable_id_with_set(&mut self) -> TypeVariableID {
        let type_variable_id = self.create_type_variable_id_without_set();
        self.create_variable_set(type_variable_id);
        type_variable_id
    }

    fn create_type_variable_id_with_type(&mut self, ty: Moduled<Type>) -> TypeVariableID {
        let type_variable_id = self.create_type_variable_id_without_set();
        let type_variable_set_id = self.create_variable_set(type_variable_id);
        let type_variable_set = &mut self.type_variable_set_components[type_variable_set_id.0];
        type_variable_set.ty = Some(ty);

        type_variable_id
    }

    fn create_variable_set(&mut self, type_variable_id: TypeVariableID) -> TypeVariableSetID {
        let allocator = *self.entity_var_map.allocator();
        let set = TypeVariableSet::new(type_variable_id, allocator);
        self.register_variable_set(allocator.alloc(set))
    }

    fn register_variable_set(
        &mut self,
        set: &'type_env_alloc mut TypeVariableSet<'type_env_alloc>,
    ) -> TypeVariableSetID {
        let type_variable_set_id = TypeVariableSetID(self.type_variable_set_components.len());

        for type_variable_id in set.members.iter() {
            self.var_set_map
                .insert(*type_variable_id, type_variable_set_id);
        }

        self.type_variable_set_components.push(set);

        type_variable_set_id
    }

    fn unify_entity_id(
        &mut self,
        left: EntityID,
        right: EntityID,
        user_type_set: &GlobalUserTypeSet,
        errors: &mut std::vec::Vec<TypeError>,
    ) {
        let left = self.get_or_allocate_variable_for_entity(left);
        let right = self.get_or_allocate_variable_for_entity(right);

        self.unify(left, right, user_type_set, errors);
    }

    /// ## unify(merge into left)
    ///
    /// ```none
    /// let a = 100
    /// b = a
    ///
    /// set(b): None, set(a): Some(int)
    ///
    ///              │
    ///              ▼
    ///
    /// set(a, b): Some(int)
    /// ```
    ///
    pub fn unify(
        &mut self,
        left: TypeVariableID,
        right: TypeVariableID,
        user_type_set: &GlobalUserTypeSet,
        errors: &mut std::vec::Vec<TypeError>,
    ) {
        let allocator = *self.type_variable_set_components.allocator();

        let left_set_id = *self.var_set_map.get(&left).unwrap();
        let right_set_id = *self.var_set_map.get(&right).unwrap();

        if left_set_id == right_set_id {
            return;
        }

        let right_set = &mut self.type_variable_set_components[right_set_id.0];

        let mut right_members = HashSet::new_in(allocator);
        swap(&mut right_members, &mut right_set.members);

        let mut right_type = None;
        swap(&mut right_type, &mut right_set.ty);

        for right_member in right_members.iter() {
            self.var_set_map.insert(*right_member, left_set_id);
        }

        let left_set = &mut self.type_variable_set_components[left_set_id.0];

        let mut left_type = None;
        swap(&mut left_type, &mut left_set.ty);

        left_set.members.extend(right_members);

        match (left_type, right_type) {
            (None, None) => {}
            (None, Some(ty)) | (Some(ty), None) => {
                left_set.ty = Some(ty);
            }
            (Some(left_type), Some(right_type)) => {
                // numeric type inference
                if let Type::IntegerLiteral = &left_type.value {
                    if right_type.value.is_integer() {
                        left_set.ty = Some(right_type);
                        return;
                    }
                }
                if let Type::IntegerLiteral = &right_type.value {
                    if left_type.value.is_integer() {
                        left_set.ty = Some(left_type);
                        return;
                    }
                }
                if let Type::FloatLiteral = &left_type.value {
                    if right_type.value.is_float() {
                        left_set.ty = Some(right_type);
                        return;
                    }
                }
                if let Type::FloatLiteral = &right_type.value {
                    if left_type.value.is_float() {
                        left_set.ty = Some(left_type);
                        return;
                    }
                }

                self.unify_type(left_type, right_type, user_type_set, errors);
            }
        }
    }

    pub fn unify_type(
        &mut self,
        left: Moduled<Type>,
        right: Moduled<Type>,
        user_type_set: &GlobalUserTypeSet,
        errors: &mut std::vec::Vec<TypeError>,
    ) {
        // resolve type alias
        if let Type::UserType {
            user_type_info,
            generics: _,
        } = &left.value
        {
            let user_type_info = user_type_set.get(*user_type_info);
            let user_type_info = user_type_info.read().unwrap();

            if user_type_info.is_alias {
                return self.unify_type(
                    Moduled::new(
                        left.value.resolve_type_alias(user_type_set),
                        left.module_path.clone(),
                        left.span.clone(),
                    ),
                    right,
                    user_type_set,
                    errors,
                );
            }
        }
        if let Type::UserType {
            user_type_info,
            generics: _,
        } = &right.value
        {
            let user_type_info = user_type_set.get(*user_type_info);
            let user_type_info = user_type_info.read().unwrap();

            if user_type_info.is_alias {
                return self.unify_type(
                    left,
                    Moduled::new(
                        right.value.resolve_type_alias(user_type_set),
                        right.module_path.clone(),
                        right.span.clone(),
                    ),
                    user_type_set,
                    errors,
                );
            }
        }

        match (&left.value, &right.value) {
            (Type::TypeVariable(left_variable_id), Type::TypeVariable(right_variable_id)) => {
                self.unify(*left_variable_id, *right_variable_id, user_type_set, errors);
            }
            (Type::TypeVariable(left_variable_id), _) => {
                let left_type = self.get_type_variable_type(*left_variable_id);

                match left_type {
                    Some(left_type) => {
                        // numeric type inference
                        if let Type::IntegerLiteral = &left_type.value {
                            if right.value.is_integer() {
                                let variable_set_id =
                                    self.var_set_map.get(left_variable_id).unwrap();
                                let variable_set =
                                    &mut self.type_variable_set_components[variable_set_id.0];
                                variable_set.ty = Some(right.clone());
                                return;
                            }
                        }
                        if let Type::FloatLiteral = &left_type.value {
                            if right.value.is_float() {
                                let variable_set_id =
                                    self.var_set_map.get(left_variable_id).unwrap();
                                let variable_set =
                                    &mut self.type_variable_set_components[variable_set_id.0];
                                variable_set.ty = Some(right.clone());
                                return;
                            }
                        }

                        self.unify_type(left_type, right.clone(), user_type_set, errors)
                    }
                    None => {
                        let variable_set_id = self.var_set_map.get(left_variable_id).unwrap();
                        let variable_set =
                            &mut self.type_variable_set_components[variable_set_id.0];
                        variable_set.ty = Some(right.clone());
                    }
                }
            }
            (_, Type::TypeVariable(right_variable_id)) => {
                let right_type = self.get_type_variable_type(*right_variable_id);

                match right_type {
                    Some(right_type) => {
                        // numeric type inference
                        if let Type::IntegerLiteral = &right_type.value {
                            if left.value.is_integer() {
                                let variable_set_id =
                                    self.var_set_map.get(right_variable_id).unwrap();
                                let variable_set =
                                    &mut self.type_variable_set_components[variable_set_id.0];
                                variable_set.ty = Some(left.clone());
                            }
                        }
                        if let Type::FloatLiteral = &right_type.value {
                            if left.value.is_float() {
                                let variable_set_id =
                                    self.var_set_map.get(right_variable_id).unwrap();
                                let variable_set =
                                    &mut self.type_variable_set_components[variable_set_id.0];
                                variable_set.ty = Some(left.clone());
                            }
                        }

                        self.unify_type(right_type, right.clone(), user_type_set, errors);
                    }
                    None => {
                        let variable_set_id = self.var_set_map.get(right_variable_id).unwrap();
                        let variable_set =
                            &mut self.type_variable_set_components[variable_set_id.0];
                        variable_set.ty = Some(left.clone());
                    }
                }
            }
            _ => {}
        }

        let equals = match &left.value {
            Type::IntegerLiteral => unreachable!(),
            Type::FloatLiteral => unreachable!(),
            Type::UserType {
                user_type_info: left_user_type,
                generics: left_generics,
            } => match &right.value {
                Type::UserType {
                    user_type_info: right_user_type,
                    generics: right_generics,
                } => {
                    if left_user_type == right_user_type {
                        if left_generics.len() == right_generics.len() {
                            for (left_generic, right_generic) in
                                left_generics.iter().zip(right_generics.iter())
                            {
                                self.unify_type(
                                    Moduled::new(
                                        left_generic.clone(),
                                        left.module_path.clone(),
                                        left.span.clone(),
                                    ),
                                    Moduled::new(
                                        right_generic.clone(),
                                        right.module_path.clone(),
                                        right.span.clone(),
                                    ),
                                    user_type_set,
                                    errors,
                                );
                            }

                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
                _ => false,
            },
            Type::Function {
                function_info: left_function_info,
                generics: left_generics,
            } => match &right.value {
                Type::Function {
                    function_info: right_function_info,
                    generics: right_generics,
                } => {
                    if left_function_info == right_function_info {
                        if left_generics.len() == right_generics.len() {
                            for (left_generic, right_generic) in
                                left_generics.iter().zip(right_generics.iter())
                            {
                                self.unify_type(
                                    Moduled::new(
                                        left_generic.clone(),
                                        left.module_path.clone(),
                                        left.span.clone(),
                                    ),
                                    Moduled::new(
                                        right_generic.clone(),
                                        right.module_path.clone(),
                                        right.span.clone(),
                                    ),
                                    user_type_set,
                                    errors,
                                );
                            }

                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
                _ => false,
            },
            Type::TypeVariable(_) => unreachable!(),
            Type::Array(left_base_type) => match &right.value {
                Type::Array(right_base_type) => {
                    self.unify_type(
                        Moduled::new(
                            left_base_type.as_ref().clone(),
                            left.module_path.clone(),
                            left.span.clone(),
                        ),
                        Moduled::new(
                            right_base_type.as_ref().clone(),
                            right.module_path.clone(),
                            right.span.clone(),
                        ),
                        user_type_set,
                        errors,
                    );

                    true
                }
                _ => false,
            },
            Type::Tuple(left_items) => match &right.value {
                Type::Tuple(right_items) => {
                    if left_items.len() == right_items.len() {
                        for (left_item, right_item) in left_items.iter().zip(right_items.iter()) {
                            self.unify_type(
                                Moduled::new(
                                    left_item.clone(),
                                    left.module_path.clone(),
                                    left.span.clone(),
                                ),
                                Moduled::new(
                                    right_item.clone(),
                                    right.module_path.clone(),
                                    right.span.clone(),
                                ),
                                user_type_set,
                                errors,
                            );
                        }

                        true
                    } else {
                        false
                    }
                }
                _ => false,
            },
            Type::Unreachable => unreachable!(),
            _ => left.value == right.value,
        };

        if !equals {
            let error = TypeError {
                kind: TypeErrorKind::TypeMismatch {
                    left: left
                        .clone()
                        .map(|left| left.to_display_string(user_type_set, Some(self))),
                    right: right
                        .clone()
                        .map(|right| right.to_display_string(user_type_set, Some(self))),
                },
                span: left.span.clone(),
                module_path: self.module_path.clone(),
            };
            errors.push(error);
        }
    }

    pub fn test_unify_type(
        &mut self,
        left: Moduled<Type>,
        right: Moduled<Type>,
        user_type_set: &GlobalUserTypeSet,
    ) -> bool {
        let mut errors = std::vec::Vec::new();

        self.unify_type(left, right, user_type_set, &mut errors);

        errors.is_empty()
    }

    pub fn create_instance(&mut self, ty: &Type) -> Type {
        match ty {
            Type::UserType {
                user_type_info,
                generics,
            } => {
                let mut new_generics = std::vec::Vec::new();
                for _ in generics.iter() {
                    new_generics.push(Type::TypeVariable(self.create_type_variable_id_with_set()));
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
                let mut new_generics = std::vec::Vec::new();
                for _ in generics.iter() {
                    new_generics.push(Type::TypeVariable(self.create_type_variable_id_with_set()));
                }

                Type::Function {
                    function_info: function_info.clone(),
                    generics: Arc::new(new_generics),
                }
            }
            _ => ty.clone(),
        }
    }

    fn resolve_type_variable_id(
        &self,
        type_variable_id: TypeVariableID,
        user_type_set: &GlobalUserTypeSet,
    ) -> Type {
        let type_variable_set_id = self.var_set_map.get(&type_variable_id).unwrap();
        let type_variable_set = &self.type_variable_set_components[type_variable_set_id.0];

        let ty = type_variable_set.ty.as_ref().cloned();

        ty.map(|ty| self.resolve_type(&ty.value, user_type_set))
            .unwrap_or(Type::Unknown)
    }

    pub fn resolve_type(&self, ty: &Type, user_type_set: &GlobalUserTypeSet) -> Type {
        match ty {
            Type::IntegerLiteral => Type::Int32,
            Type::FloatLiteral => Type::Float32,
            Type::UserType {
                user_type_info: _,
                generics: _,
            } => {
                let resolved_type = ty.resolve_type_alias(user_type_set);

                if let Type::UserType {
                    user_type_info,
                    generics,
                } = resolved_type
                {
                    let generics = generics
                        .iter()
                        .map(|generic| self.resolve_type(generic, user_type_set))
                        .collect();

                    Type::UserType {
                        user_type_info,
                        generics: Arc::new(generics),
                    }
                } else {
                    unreachable!()
                }
            }
            Type::Function {
                function_info,
                generics,
            } => {
                let generics = generics
                    .iter()
                    .map(|generic| self.resolve_type(generic, user_type_set))
                    .collect();

                let mut new_arguments = std::vec::Vec::with_capacity(function_info.arguments.len());
                for argument in function_info.arguments.iter() {
                    new_arguments.push(self.resolve_type(argument, user_type_set));
                }

                let new_return_type = self.resolve_type(&function_info.return_type, user_type_set);

                Type::Function {
                    function_info: Arc::new(FunctionTypeInfo {
                        module_path: function_info.module_path.clone(),
                        name: function_info.name.clone(),
                        generics: function_info.generics.clone(),
                        arguments: new_arguments,
                        return_type: new_return_type,
                    }),
                    generics: Arc::new(generics),
                }
            }
            Type::TypeVariable(type_variable_id) => {
                self.resolve_type_variable_id(*type_variable_id, user_type_set)
            }
            Type::Array(base_type) => {
                Type::Array(Arc::new(self.resolve_type(&base_type, user_type_set)))
            }
            Type::Tuple(items) => {
                let items = items
                    .iter()
                    .map(|item| self.resolve_type(item, user_type_set))
                    .collect();
                Type::Tuple(Arc::new(items))
            }
            _ => ty.clone(),
        }
    }

    pub fn retake_type_variable(&mut self, ty: &Type) -> Type {
        match ty {
            Type::UserType {
                user_type_info,
                generics,
            } => {
                let mut new_generics = std::vec::Vec::with_capacity(generics.len());
                for generic in generics.iter() {
                    new_generics.push(self.retake_type_variable(generic));
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
                let mut new_generics = std::vec::Vec::with_capacity(generics.len());
                for generic in generics.iter() {
                    new_generics.push(self.retake_type_variable(generic));
                }

                let mut new_arguments = std::vec::Vec::with_capacity(function_info.arguments.len());
                for argument in function_info.arguments.iter() {
                    new_arguments.push(self.retake_type_variable(argument));
                }

                let new_return_type = self.retake_type_variable(&function_info.return_type);

                Type::Function {
                    function_info: Arc::new(FunctionTypeInfo {
                        module_path: function_info.module_path.clone(),
                        name: function_info.name.clone(),
                        generics: function_info.generics.clone(),
                        arguments: new_arguments,
                        return_type: new_return_type,
                    }),
                    generics: Arc::new(new_generics),
                }
            }
            Type::TypeVariable(type_variable_id) => {
                let type_variable_set_id = self.var_set_map.get(type_variable_id).unwrap();
                let type_variable_set = &self.type_variable_set_components[type_variable_set_id.0];
                let resolved_type = type_variable_set
                    .ty
                    .as_ref()
                    .cloned()
                    .map(|ty| ty.map(|ty| self.retake_type_variable(&ty)));

                let new_type_variable_id = self.create_type_variable_id_with_set();
                let new_type_variable_set_id = self.var_set_map.get(&new_type_variable_id).unwrap();
                let new_type_variable_set =
                    &mut self.type_variable_set_components[new_type_variable_set_id.0];

                new_type_variable_set.ty = resolved_type;

                Type::TypeVariable(new_type_variable_id)
            }
            Type::Array(base_type) => Type::Array(Arc::new(self.retake_type_variable(&base_type))),
            Type::Tuple(items) => {
                let mut new_items = std::vec::Vec::with_capacity(items.len());
                for item in items.iter() {
                    new_items.push(self.retake_type_variable(item));
                }

                Type::Tuple(Arc::new(new_items))
            }
            _ => ty.clone(),
        }
    }

    fn export_entity_type(
        &self,
        user_type_set: &GlobalUserTypeSet,
    ) -> HashMap<EntityID, Spanned<Type>> {
        let mut entity_type_map = HashMap::new();

        for (entity_id, type_variable_id) in self.entity_var_map.iter() {
            let type_variable_set_id = self.var_set_map.get(type_variable_id).unwrap();
            let type_variable_set = &self.type_variable_set_components[type_variable_set_id.0];

            if let Some(ty) = &type_variable_set.ty {
                entity_type_map.insert(
                    *entity_id,
                    Spanned::new(self.resolve_type(&ty.value, user_type_set), ty.span.clone()),
                );
            }
        }

        entity_type_map
    }
}

impl Drop for TypeEnvironment<'_> {
    fn drop(&mut self) {
        // explicit drop to avoid memory leak
        self.type_variable_set_components.clear();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct TypeVariableSetID(usize);

#[derive(Debug, Clone)]
pub(crate) struct TypeVariableSet<'type_env_alloc> {
    members: HashSet<TypeVariableID, DefaultHashBuilder, &'type_env_alloc Bump>,
    ty: Option<Moduled<Type>>,
}

impl<'type_env_alloc> TypeVariableSet<'type_env_alloc> {
    fn new(init_member: TypeVariableID, allocator: &'type_env_alloc Bump) -> Self {
        let mut members = HashSet::with_capacity_in(1, allocator);
        members.insert(init_member);
        Self { members, ty: None }
    }
}

impl Drop for TypeVariableSet<'_> {
    fn drop(&mut self) {
        // explicit drop to avoid memory leak.
        self.ty = None;
    }
}

pub fn infer_type(
    ast: &Program,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_type_map: &HashMap<String, HashMap<String, Type>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    package_resource_set: &PackageResourceSet,
    errors: &mut std::vec::Vec<TypeError>,
) -> HashMap<EntityID, Spanned<Type>> {
    let allocator = Bump::new();
    let mut type_environment = TypeEnvironment::new(module_path, &allocator);

    infer_type_for_program(
        ast,
        false,
        &mut type_environment,
        &Moduled::new(Type::Unit, module_path.clone(), ast.span.clone()),
        &None,
        generics,
        implements_infos,
        import_map,
        entity_user_type_map,
        moduled_name_type_map,
        name_resolved_map,
        user_type_set,
        module_path,
        package_resource_set,
        errors,
    );

    type_environment.export_entity_type(user_type_set)
}

fn infer_type_for_program(
    ast: &Program,
    as_expression: bool,
    type_environment: &mut TypeEnvironment,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_type_map: &HashMap<String, HashMap<String, Type>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    package_resource_set: &PackageResourceSet,
    errors: &mut std::vec::Vec<TypeError>,
) {
    for statement in ast.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => {
                infer_type_for_expression(
                    &assignment.left,
                    as_expression,
                    type_environment,
                    return_type,
                    this_type,
                    generics,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    package_resource_set,
                    errors,
                );
                if let Ok(right) = &assignment.right {
                    infer_type_for_expression(
                        right,
                        as_expression,
                        type_environment,
                        return_type,
                        this_type,
                        generics,
                        implements_infos,
                        import_map,
                        entity_user_type_map,
                        moduled_name_type_map,
                        name_resolved_map,
                        user_type_set,
                        module_path,
                        package_resource_set,
                        errors,
                    );

                    type_environment.unify_entity_id(
                        EntityID::from(&assignment.left),
                        EntityID::from(right),
                        user_type_set,
                        errors,
                    );
                }
            }
            Statement::Swap(swap_statement) => todo!(),
            Statement::Import(_) => {}
            Statement::DefineWithAttribute(define_with_attribute) => {
                if let Ok(define) = &define_with_attribute.define {
                    match define {
                        Define::Function(function_define) => todo!(),
                        Define::UserType(user_type_define) => todo!(),
                        Define::Variable(variable_define) => {
                            let binding_id = match &variable_define.binding {
                                Ok(binding) => infer_type_for_variable_binding(
                                    binding,
                                    type_environment,
                                    module_path,
                                ),
                                Err(_) => type_environment.create_type_variable_id_with_set(),
                            };

                            let expression_id = match &variable_define.expression {
                                Some(expression) => infer_type_for_expression(
                                    expression,
                                    as_expression,
                                    type_environment,
                                    return_type,
                                    this_type,
                                    generics,
                                    implements_infos,
                                    import_map,
                                    entity_user_type_map,
                                    moduled_name_type_map,
                                    name_resolved_map,
                                    user_type_set,
                                    module_path,
                                    package_resource_set,
                                    errors,
                                ),
                                None => type_environment.create_type_variable_id_with_set(),
                            };

                            type_environment.unify(
                                binding_id,
                                expression_id,
                                user_type_set,
                                errors,
                            );
                        }
                        Define::TypeAlias(_) => {}
                    }
                }
            }
            Statement::Drop(drop_statement) => todo!(),
            Statement::Expression(expression) => todo!(),
            Statement::Implements(implements) => todo!(),
        }
    }
}

fn infer_type_for_variable_binding(
    ast: &VariableBinding,
    type_environment: &mut TypeEnvironment,
    module_path: &ModulePath,
) -> TypeVariableID {
    match ast {
        VariableBinding::Literal(literal) => {
            type_environment.get_or_allocate_variable_for_entity(EntityID::from(literal))
        }
        VariableBinding::Binding { bindings, span } => {
            let tuple_items = bindings
                .iter()
                .map(|binding| {
                    infer_type_for_variable_binding(binding, type_environment, module_path)
                })
                .map(|id| Type::TypeVariable(id))
                .collect();
            let tuple_type = Type::Tuple(Arc::new(tuple_items));

            type_environment.create_and_set_entity_type(
                EntityID::from(ast),
                Moduled::new(tuple_type, module_path.clone(), span.clone()),
            )
        }
    }
}

fn infer_type_for_expression(
    ast: &Expression,
    as_expression: bool,
    type_environment: &mut TypeEnvironment,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_type_map: &HashMap<String, HashMap<String, Type>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    package_resource_set: &PackageResourceSet,
    errors: &mut std::vec::Vec<TypeError>,
) -> TypeVariableID {
    match ast {
        Expression::Return(return_expression) => {
            let type_variable_id = match &return_expression.expression {
                Some(expression) => infer_type_for_expression(
                    expression,
                    as_expression,
                    type_environment,
                    return_type,
                    this_type,
                    generics,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    package_resource_set,
                    errors,
                ),
                None => type_environment.create_and_set_entity_type(
                    EntityID::from(ast),
                    Moduled::new(
                        Type::Unit,
                        module_path.clone(),
                        return_expression.span.clone(),
                    ),
                ),
            };

            let return_expression_type = Type::TypeVariable(type_variable_id);

            type_environment.unify_type(
                return_type.clone(),
                Moduled::new(
                    return_expression_type,
                    module_path.clone(),
                    return_expression.span.clone(),
                ),
                user_type_set,
                errors,
            );

            type_variable_id
        }
        Expression::Closure(closure) => todo!(),
        Expression::Or(or_expression) => infer_type_for_or_expression(
            or_expression,
            as_expression,
            type_environment,
            return_type,
            this_type,
            generics,
            implements_infos,
            import_map,
            entity_user_type_map,
            moduled_name_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            package_resource_set,
            errors,
        ),
    }
}

fn infer_type_for_or_expression(
    ast: &OrExpression,
    as_expression: bool,
    type_environment: &mut TypeEnvironment,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_type_map: &HashMap<String, HashMap<String, Type>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    package_resource_set: &PackageResourceSet,
    errors: &mut std::vec::Vec<TypeError>,
) -> TypeVariableID {
    if ast.chain.is_empty() {
        infer_type_for_and_expression(
            &ast.left,
            as_expression,
            type_environment,
            return_type,
            this_type,
            generics,
            implements_infos,
            import_map,
            entity_user_type_map,
            moduled_name_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            package_resource_set,
            errors,
        )
    } else {
        todo!()
    }
}

fn infer_type_for_and_expression(
    ast: &AndExpression,
    as_expression: bool,
    type_environment: &mut TypeEnvironment,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_type_map: &HashMap<String, HashMap<String, Type>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    package_resource_set: &PackageResourceSet,
    errors: &mut std::vec::Vec<TypeError>,
) -> TypeVariableID {
    if ast.chain.is_empty() {
        infer_type_for_equals_expression(
            &ast.left,
            as_expression,
            type_environment,
            return_type,
            this_type,
            generics,
            implements_infos,
            import_map,
            entity_user_type_map,
            moduled_name_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            package_resource_set,
            errors,
        )
    } else {
        todo!()
    }
}

fn infer_type_for_equals_expression(
    ast: &EqualsExpression,
    as_expression: bool,
    type_environment: &mut TypeEnvironment,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_type_map: &HashMap<String, HashMap<String, Type>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    package_resource_set: &PackageResourceSet,
    errors: &mut std::vec::Vec<TypeError>,
) -> TypeVariableID {
    if ast.chain.is_empty() {
        infer_type_for_less_or_greater_expression(
            &ast.left,
            as_expression,
            type_environment,
            return_type,
            this_type,
            generics,
            implements_infos,
            import_map,
            entity_user_type_map,
            moduled_name_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            package_resource_set,
            errors,
        )
    } else {
        todo!()
    }
}

fn infer_type_for_less_or_greater_expression(
    ast: &LessOrGreaterExpression,
    as_expression: bool,
    type_environment: &mut TypeEnvironment,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_type_map: &HashMap<String, HashMap<String, Type>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    package_resource_set: &PackageResourceSet,
    errors: &mut std::vec::Vec<TypeError>,
) -> TypeVariableID {
    if ast.chain.is_empty() {
        infer_type_for_add_or_sub_expression(
            &ast.left,
            as_expression,
            type_environment,
            return_type,
            this_type,
            generics,
            implements_infos,
            import_map,
            entity_user_type_map,
            moduled_name_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            package_resource_set,
            errors,
        )
    } else {
        todo!()
    }
}

fn infer_type_for_add_or_sub_expression(
    ast: &AddOrSubExpression,
    as_expression: bool,
    type_environment: &mut TypeEnvironment,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_type_map: &HashMap<String, HashMap<String, Type>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    package_resource_set: &PackageResourceSet,
    errors: &mut std::vec::Vec<TypeError>,
) -> TypeVariableID {
    if ast.chain.is_empty() {
        infer_type_for_mul_or_div_expression(
            &ast.left,
            as_expression,
            type_environment,
            return_type,
            this_type,
            generics,
            implements_infos,
            import_map,
            entity_user_type_map,
            moduled_name_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            package_resource_set,
            errors,
        )
    } else {
        todo!()
    }
}

fn infer_type_for_mul_or_div_expression(
    ast: &MulOrDivExpression,
    as_expression: bool,
    type_environment: &mut TypeEnvironment,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_type_map: &HashMap<String, HashMap<String, Type>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    package_resource_set: &PackageResourceSet,
    errors: &mut std::vec::Vec<TypeError>,
) -> TypeVariableID {
    if ast.chain.is_empty() {
        infer_type_for_factor(
            &ast.left,
            as_expression,
            type_environment,
            return_type,
            this_type,
            generics,
            implements_infos,
            import_map,
            entity_user_type_map,
            moduled_name_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            package_resource_set,
            errors,
        )
    } else {
        todo!()
    }
}

fn infer_type_for_factor(
    ast: &Factor,
    as_expression: bool,
    type_environment: &mut TypeEnvironment,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_type_map: &HashMap<String, HashMap<String, Type>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    package_resource_set: &PackageResourceSet,
    errors: &mut std::vec::Vec<TypeError>,
) -> TypeVariableID {
    match &ast.minus {
        Some(_) => todo!(),
        None => match &ast.primary {
            Ok(primary) => infer_type_for_primary(
                primary,
                as_expression,
                type_environment,
                return_type,
                this_type,
                generics,
                implements_infos,
                import_map,
                entity_user_type_map,
                moduled_name_type_map,
                name_resolved_map,
                user_type_set,
                module_path,
                package_resource_set,
                errors,
            ),
            Err(_) => type_environment.create_type_variable_id_with_set(),
        },
    }
}

fn infer_type_for_primary(
    ast: &Primary,
    as_expression: bool,
    type_environment: &mut TypeEnvironment,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_type_map: &HashMap<String, HashMap<String, Type>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    package_resource_set: &PackageResourceSet,
    errors: &mut std::vec::Vec<TypeError>,
) -> TypeVariableID {
    let mut chain_start_position = 0;

    let first_type_variable_id = match &ast.left.first {
        PrimaryLeftExpr::Simple {
            left,
            generics,
            function_call,
            span,
        } => match left {
            SimplePrimary::Tuple { expressions, span } => todo!(),
            SimplePrimary::Literal(literal) => {
                match name_resolved_map.get(&EntityID::from(literal)) {
                    Some(resolved) => match resolved.define.kind {
                        DefineKind::Import => {
                            match import_map.get(&resolved.define.entity_id).unwrap() {
                                ImportElement::ModuleAlias { path } => {
                                    let path_name =
                                        path.iter().cloned().collect::<Vec<_>>().join("::");

                                    get_module_import_type(
                                        ast,
                                        literal,
                                        path_name,
                                        &mut chain_start_position,
                                        type_environment,
                                        moduled_name_type_map,
                                        package_resource_set,
                                        module_path,
                                        errors,
                                    )
                                }
                                ImportElement::ModuleElement { path, element } => {
                                    let module_name =
                                        path.iter().cloned().collect::<Vec<_>>().join("::");
                                    let module_type = moduled_name_type_map
                                        .get(&module_name)
                                        .unwrap()
                                        .get(element)
                                        .unwrap();

                                    let instance = type_environment.create_instance(module_type);

                                    type_environment.create_and_set_entity_type(
                                        literal.into(),
                                        Moduled::new(
                                            instance,
                                            module_path.clone(),
                                            literal.span.clone(),
                                        ),
                                    )
                                }
                                ImportElement::Unknown => type_environment
                                    .create_and_set_entity_type(
                                        literal.into(),
                                        Moduled::new(
                                            Type::Unknown,
                                            module_path.clone(),
                                            literal.span.clone(),
                                        ),
                                    ),
                            }
                        }
                        DefineKind::Function => todo!(),
                        DefineKind::Variable => type_environment
                            .get_or_allocate_variable_for_entity(resolved.define.entity_id),
                        DefineKind::UserType => todo!(),
                        DefineKind::Generics => todo!(),
                        DefineKind::TypeAlias => todo!(),
                    },
                    None => match package_resource_set.get(literal.value) {
                        Some(_) => get_module_import_type(
                            ast,
                            literal,
                            literal.value.to_string(),
                            &mut chain_start_position,
                            type_environment,
                            moduled_name_type_map,
                            package_resource_set,
                            module_path,
                            errors,
                        ),
                        None => type_environment.create_type_variable_id_with_set(),
                    },
                }
            }
            SimplePrimary::StringLiteral(literal) => todo!(),
            SimplePrimary::NumericLiteral(literal) => type_environment.create_and_set_entity_type(
                EntityID::from(literal),
                Moduled::new(
                    Type::IntegerLiteral,
                    module_path.clone(),
                    literal.span.clone(),
                ),
            ),
            SimplePrimary::Null(range) => todo!(),
            SimplePrimary::True(range) => todo!(),
            SimplePrimary::False(range) => todo!(),
            SimplePrimary::This(range) => todo!(),
            SimplePrimary::LargeThis(range) => todo!(),
        },
        PrimaryLeftExpr::NewObject { new_object } => {
            fn infer_type_for_new_object_expression(
                ast: &NewObjectExpression,
                object_type: Spanned<Type>,
                type_environment: &mut TypeEnvironment,
                return_type: &Moduled<Type>,
                this_type: &Option<Type>,
                generics: &HashMap<EntityID, Arc<GenericType>>,
                implements_infos: &ImplementsInfoSet,
                import_map: &HashMap<EntityID, ImportElement>,
                entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
                moduled_name_type_map: &HashMap<String, HashMap<String, Type>>,
                name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
                user_type_set: &GlobalUserTypeSet,
                module_path: &ModulePath,
                package_resource_set: &PackageResourceSet,
                errors: &mut std::vec::Vec<TypeError>,
            ) -> TypeVariableID {
                match object_type.value.resolve_type_alias(user_type_set) {
                    Type::UserType {
                        user_type_info,
                        generics: _,
                    } => {
                        let user_type_info = user_type_set.get(user_type_info);
                        let user_type_info = user_type_info.read().unwrap();

                        let old_generics = &user_type_info.generics;
                        let new_generics = (0..old_generics.len())
                            .map(|_| type_environment.create_type_variable_id_with_set())
                            .collect::<Vec<_>>();

                        let mut assigned_field_names = HashSet::new();
                        for field_assign in ast.field_assign.elements.iter() {
                            let field_name = field_assign.field.value;
                            let field_type = match user_type_info.element_types.get(field_name) {
                                Some(field_type) => field_type
                                    .clone()
                                    .map(|ty| ty.replace_generics(&old_generics, &new_generics)),
                                None => {
                                    let error = TypeError {
                                        kind: TypeErrorKind::UnknownUserTypeElement,
                                        span: field_assign.field.span.clone(),
                                        module_path: module_path.clone(),
                                    };
                                    errors.push(error);
                                    continue;
                                }
                            };

                            if let Type::Function {
                                function_info,
                                generics: _,
                            } = &field_type.value
                            {
                                if function_info.name.is_none() {
                                    let error = TypeError {
                                        kind: TypeErrorKind::NotFieldInFieldAssign,
                                        span: field_assign.field.span.clone(),
                                        module_path: module_path.clone(),
                                    };
                                    errors.push(error);
                                }
                            }

                            let origin_field =
                                type_environment.create_type_variable_id_with_type(Moduled::new(
                                    field_type.value,
                                    user_type_info.module_path.clone(),
                                    field_type.span.clone(),
                                ));
                            let assign_field = infer_type_for_expression(
                                &field_assign.expression,
                                true,
                                type_environment,
                                return_type,
                                this_type,
                                generics,
                                implements_infos,
                                import_map,
                                entity_user_type_map,
                                moduled_name_type_map,
                                name_resolved_map,
                                user_type_set,
                                module_path,
                                package_resource_set,
                                errors,
                            );

                            type_environment.unify(
                                origin_field,
                                assign_field,
                                user_type_set,
                                errors,
                            );

                            assigned_field_names.insert(field_name);
                        }

                        todo!()
                    }
                    _ => {
                        let error = TypeError {
                            kind: TypeErrorKind::NotUserTypeInFieldAssign,
                            span: object_type.span.clone(),
                            module_path: module_path.clone(),
                        };
                        errors.push(error);

                        type_environment.create_type_variable_id_with_type(Moduled::new(
                            Type::Unknown,
                            module_path.clone(),
                            ast.span.clone(),
                        ))
                    }
                }
            }

            match new_object.path.first() {
                Some(literal) => match name_resolved_map.get(&EntityID::from(literal)) {
                    Some(resolved) => todo!(),
                    None => {
                        let type_module_path = &new_object.path[..new_object.path.len() - 1];
                        let type_literal = new_object.path.last().unwrap();

                        let type_module_path_name = type_module_path
                            .iter()
                            .map(|literal| literal.value.to_string())
                            .collect::<Vec<_>>()
                            .join("::");

                        match package_resource_set.get(&type_module_path_name) {
                            Some(_) => {
                                match moduled_name_type_map
                                    .get(&type_module_path_name)
                                    .unwrap()
                                    .get(type_literal.value)
                                {
                                    Some(ty) => infer_type_for_new_object_expression(
                                        new_object,
                                        Spanned::new(ty.clone(), type_literal.span.clone()),
                                        type_environment,
                                        return_type,
                                        this_type,
                                        generics,
                                        implements_infos,
                                        import_map,
                                        entity_user_type_map,
                                        moduled_name_type_map,
                                        name_resolved_map,
                                        user_type_set,
                                        module_path,
                                        package_resource_set,
                                        errors,
                                    ),
                                    None => {
                                        let error = TypeError {
                                            kind: TypeErrorKind::UnknownModuleElementType,
                                            span: type_literal.span.clone(),
                                            module_path: module_path.clone(),
                                        };
                                        errors.push(error);

                                        type_environment.create_type_variable_id_with_type(
                                            Moduled::new(
                                                Type::Unknown,
                                                module_path.clone(),
                                                new_object.span.clone(),
                                            ),
                                        )
                                    }
                                }
                            }
                            None => {
                                type_environment.create_type_variable_id_with_type(Moduled::new(
                                    Type::Unknown,
                                    module_path.clone(),
                                    new_object.span.clone(),
                                ))
                            }
                        }
                    }
                },
                None => type_environment.create_type_variable_id_with_type(Moduled::new(
                    Type::Unknown,
                    module_path.clone(),
                    new_object.span.clone(),
                )),
            }
        }
        PrimaryLeftExpr::NewArray { new_array } => todo!(),
        PrimaryLeftExpr::NewArrayInit { new_array_init } => todo!(),
        PrimaryLeftExpr::If { if_expression } => todo!(),
        PrimaryLeftExpr::Loop { loop_expression } => todo!(),
    };

    let mut last_type_variable_id = first_type_variable_id;
    loop {
        if chain_start_position >= ast.chain.len() {
            break;
        }

        let chain = &ast.chain[chain_start_position];
        chain_start_position += 1;

        if let Some(second) = &chain.second {}
    }

    last_type_variable_id
}

fn get_module_import_type(
    ast: &Primary,
    first_literal: &Literal,
    mut path_name: String,
    chain_start_position: &mut usize,
    type_environment: &mut TypeEnvironment,
    moduled_name_type_map: &HashMap<String, HashMap<String, Type>>,
    package_resource_set: &PackageResourceSet,
    module_path: &ModulePath,
    errors: &mut std::vec::Vec<TypeError>,
) -> TypeVariableID {
    let mut import_type = Type::Unknown;
    let mut last_literal = first_literal;
    for (index, chain) in ast.chain.iter().enumerate() {
        if chain.separator.value != PrimarySeparator::DoubleColon {
            break;
        }

        let Some(second) = &chain.second else {
            break;
        };

        let mut new_path = path_name.clone();
        new_path += "::";
        new_path += second.literal.value;

        last_literal = &second.literal;

        if package_resource_set.get(&new_path).is_none() {
            *chain_start_position = index;

            match moduled_name_type_map
                .get(&path_name)
                .unwrap()
                .get(second.literal.value)
            {
                Some(ty) => {
                    import_type = ty.clone();
                }
                None => {
                    let error = TypeError {
                        kind: TypeErrorKind::UnknownModuleElementType,
                        span: second.literal.span.clone(),
                        module_path: module_path.clone(),
                    };
                    errors.push(error);
                }
            }

            break;
        }

        path_name = new_path;
    }

    type_environment.create_and_set_entity_type(
        last_literal.into(),
        Moduled::new(import_type, module_path.clone(), last_literal.span.clone()),
    )
}
