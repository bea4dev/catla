use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_import::{ImportElement, resource::PackageResourceSet};
use catla_name_resolver::{DefineKind, ResolvedInfo};
use catla_parser::ast::{
    AddOrSubExpression, AndExpression, Define, EntityID, EqualsExpression, Expression, Factor,
    GenericsDefine, LessOrGreaterExpression, Literal, MulOrDivExpression, NewObjectExpression,
    OrExpression, Primary, PrimaryLeftExpr, PrimarySeparator, Program, SimplePrimary, Spanned,
    Statement, StatementAttribute, VariableBinding, WhereClause, WithSpan,
};
use catla_util::module_path::{ModulePath, Moduled, ToModuled};
use hashbrown::{DefaultHashBuilder, HashMap, HashSet};
use std::{mem::swap, sync::Arc};

use crate::{
    error::{TypeError, TypeErrorKind},
    module_element_collector::get_type,
    types::{
        FunctionTypeInfo, GenericType, GlobalUserTypeSet, ImplementsCheckResult,
        ImplementsElementChecker, ImplementsInfo, ImplementsInfoSet, Type, WhereClauseInfo,
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
        if let Type::TypeVariable(type_variable_id) = &ty.value {
            return *type_variable_id;
        }

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

        let left_type = self.borrow_type_variable_set(left).ty.as_ref().cloned();
        let right_type = self.borrow_type_variable_set(right).ty.as_ref().cloned();

        match (left_type, right_type) {
            (None, None) => {
                let right_set = self.borrow_mut_type_variable_set(right);

                let mut members = HashSet::new_in(allocator);
                swap(&mut right_set.members, &mut members);

                let left_set_id = *self.var_set_map.get(&left).unwrap();

                for member in members.iter().cloned() {
                    self.var_set_map.insert(member, left_set_id);
                }

                self.borrow_mut_type_variable_set(left)
                    .members
                    .extend(members);
            }
            (None, Some(ty)) | (Some(ty), None) => {
                let right_set = self.borrow_mut_type_variable_set(right);

                let mut members = HashSet::new_in(allocator);
                swap(&mut right_set.members, &mut members);

                let left_set_id = *self.var_set_map.get(&left).unwrap();

                for member in members.iter().cloned() {
                    self.var_set_map.insert(member, left_set_id);
                }

                let left_set = self.borrow_mut_type_variable_set(left);

                left_set.members.extend(members);
                left_set.ty = Some(ty);
            }
            (Some(left_type), Some(right_type)) => {
                // numeric type inference
                let left_set = self.borrow_mut_type_variable_set(left);
                let mut merged = false;
                if let Type::IntegerLiteral = &left_type.value {
                    if right_type.value.is_integer() {
                        left_set.ty = Some(right_type.clone());
                        merged = true;
                    }
                }
                if let Type::IntegerLiteral = &right_type.value {
                    if left_type.value.is_integer() {
                        left_set.ty = Some(left_type.clone());
                        merged = true;
                    }
                }
                if let Type::FloatLiteral = &left_type.value {
                    if right_type.value.is_float() {
                        left_set.ty = Some(right_type.clone());
                        merged = true;
                    }
                }
                if let Type::FloatLiteral = &right_type.value {
                    if left_type.value.is_float() {
                        left_set.ty = Some(left_type.clone());
                        merged = true;
                    }
                }

                if merged {
                    let right_set = self.borrow_mut_type_variable_set(right);

                    let mut members = HashSet::new_in(allocator);
                    swap(&mut right_set.members, &mut members);

                    let left_set_id = *self.var_set_map.get(&left).unwrap();

                    for member in members.iter().cloned() {
                        self.var_set_map.insert(member, left_set_id);
                    }

                    self.borrow_mut_type_variable_set(left)
                        .members
                        .extend(members);
                } else {
                    self.unify_type(left_type.clone(), right_type.clone(), user_type_set, errors);
                }
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
                return;
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

                return;
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
                                return;
                            }
                        }
                        if let Type::FloatLiteral = &right_type.value {
                            if left.value.is_float() {
                                let variable_set_id =
                                    self.var_set_map.get(right_variable_id).unwrap();
                                let variable_set =
                                    &mut self.type_variable_set_components[variable_set_id.0];
                                variable_set.ty = Some(left.clone());
                                return;
                            }
                        }

                        self.unify_type(left.clone(), right_type, user_type_set, errors);
                    }
                    None => {
                        let variable_set_id = self.var_set_map.get(right_variable_id).unwrap();
                        let variable_set =
                            &mut self.type_variable_set_components[variable_set_id.0];
                        variable_set.ty = Some(left.clone());
                    }
                }

                return;
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
                    if left_function_info.arguments.len() != right_function_info.arguments.len() {
                        let left_arguments_span_start = left_function_info
                            .arguments
                            .first()
                            .map(|argument| argument.span.start)
                            .unwrap_or(left_function_info.span.start);
                        let left_arguments_span_end = left_function_info
                            .arguments
                            .last()
                            .map(|argument| argument.span.end)
                            .unwrap_or(left_function_info.span.end);
                        let right_arguemnts_span_start = right_function_info
                            .arguments
                            .first()
                            .map(|argument| argument.span.start)
                            .unwrap_or(right_function_info.span.start);
                        let right_arguemnts_span_end = right_function_info
                            .arguments
                            .last()
                            .map(|argument| argument.span.end)
                            .unwrap_or(right_function_info.span.end);

                        let error = TypeError {
                            kind: TypeErrorKind::FunctionArgumentCountMismatchInInfer {
                                expected: left_function_info.arguments.len().moduled(
                                    left_function_info.module_path.clone(),
                                    left_arguments_span_start..left_arguments_span_end,
                                ),
                                found: right_function_info.arguments.len().moduled(
                                    right_function_info.module_path.clone(),
                                    right_arguemnts_span_start..right_arguemnts_span_end,
                                ),
                            },
                            span: left.span.clone(),
                            module_path: left.module_path.clone(),
                        };
                        errors.push(error);
                        return;
                    }

                    for (left_argument, right_argument) in left_function_info
                        .arguments
                        .iter()
                        .zip(right_function_info.arguments.iter())
                    {
                        self.unify_type(
                            left_argument.value.clone().moduled(
                                left_function_info.module_path.clone(),
                                left_argument.span.clone(),
                            ),
                            right_argument.value.clone().moduled(
                                right_function_info.module_path.clone(),
                                right_argument.span.clone(),
                            ),
                            user_type_set,
                            errors,
                        );
                    }

                    self.unify_type(
                        left_function_info.return_type.value.clone().moduled(
                            left_function_info.module_path.clone(),
                            left_function_info.return_type.span.clone(),
                        ),
                        right_function_info.return_type.value.clone().moduled(
                            right_function_info.module_path.clone(),
                            right_function_info.return_type.span.clone(),
                        ),
                        user_type_set,
                        errors,
                    );

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

    fn borrow_type_variable_set(
        &self,
        type_variable_id: TypeVariableID,
    ) -> &TypeVariableSet<'type_env_alloc> {
        let type_variable_set_id = self.var_set_map.get(&type_variable_id).unwrap();
        &self.type_variable_set_components[type_variable_set_id.0]
    }

    fn borrow_mut_type_variable_set(
        &mut self,
        type_variable_id: TypeVariableID,
    ) -> &mut TypeVariableSet<'type_env_alloc> {
        let type_variable_set_id = self.var_set_map.get(&type_variable_id).unwrap();
        &mut self.type_variable_set_components[type_variable_set_id.0]
    }

    fn get_user_type_element(
        &mut self,
        type_variable_id: Spanned<TypeVariableID>,
        literal: &Literal,
        function_type_hints: Option<(&str, &[Moduled<Type>])>,
        implements_infos: &ImplementsInfoSet,
        user_type_set: &GlobalUserTypeSet,
        errors: &mut std::vec::Vec<TypeError>,
    ) -> TypeVariableID {
        let mut canditates = Vec::new();

        if let Some(ty) = self
            .borrow_type_variable_set(type_variable_id.value)
            .ty
            .as_ref()
            .cloned()
        {
            if let Type::UserType {
                user_type_info,
                generics,
            } = &ty.value
            {
                let user_type_info = user_type_set.get(*user_type_info);
                let user_type_info = user_type_info.read().unwrap();

                let old_generics = &user_type_info.generics;
                let new_generics = generics;

                if let Some(element_type) = user_type_info.element_types.get(literal.value) {
                    let element_type = match &element_type.value {
                        Type::Function {
                            function_info: _,
                            generics: _,
                        } => self
                            .create_instance(&element_type.value, user_type_set)
                            .with_span(element_type.span.clone()),
                        _ => element_type.clone(),
                    };

                    canditates.push(Moduled::new(
                        element_type
                            .value
                            .replace_generics(&old_generics, &new_generics),
                        user_type_info.module_path.clone(),
                        element_type.span.clone(),
                    ));
                }
            }
        }

        let mut concretes = Vec::new();

        if let Some((function_name, arguments)) = function_type_hints {
            let results = implements_infos.find_implements_for(
                Moduled::new(
                    Type::TypeVariable(type_variable_id.value),
                    self.module_path.clone(),
                    type_variable_id.span.clone(),
                ),
                function_name,
                arguments,
                user_type_set,
                self,
            );

            for (implements_info, result) in results {
                match result {
                    ImplementsCheckResult::NoImplementsFound => unreachable!(),
                    ImplementsCheckResult::Conflicts { conflicts } => {
                        let error = TypeError {
                            kind: TypeErrorKind::ConflictedImplementInTypeInfer {
                                conflicts: conflicts
                                    .into_iter()
                                    .map(|conflict| {
                                        conflict.map(|ty| {
                                            ty.to_display_string(user_type_set, Some(self))
                                        })
                                    })
                                    .collect(),
                            },
                            span: literal.span.clone(),
                            module_path: self.module_path.clone(),
                        };
                        errors.push(error);
                    }
                    ImplementsCheckResult::Success {
                        new_concrete,
                        new_interface: _,
                        new_generics,
                    } => {
                        let element_type = match implements_info.element_type.get(literal.value) {
                            Some(element_type) => element_type.value.clone(),
                            None => {
                                if !implements_info.is_instant {
                                    continue;
                                }

                                let Type::UserType {
                                    user_type_info,
                                    generics: new_generics,
                                } = &implements_info.interface.value
                                else {
                                    continue;
                                };

                                let user_type_info = user_type_set.get(*user_type_info);
                                let user_type_info = user_type_info.read().unwrap();

                                let old_generics = &user_type_info.generics;

                                let Some(element_type) =
                                    user_type_info.element_types.get(literal.value)
                                else {
                                    continue;
                                };

                                element_type
                                    .value
                                    .replace_generics(old_generics, new_generics)
                            }
                        };

                        let element_type = match &element_type {
                            Type::Function {
                                function_info: _,
                                generics: _,
                            } => self.create_instance(&element_type, user_type_set),
                            _ => element_type,
                        };

                        self.test_unify_type(
                            Moduled::new(
                                Type::TypeVariable(type_variable_id.value),
                                self.module_path.clone(),
                                type_variable_id.span.clone(),
                            ),
                            new_concrete.clone(),
                            user_type_set,
                        );

                        concretes.push(new_concrete);

                        let old_generics = &implements_info.generics_define;

                        let element_type =
                            element_type.replace_generics(&old_generics, &new_generics);

                        canditates.push(Moduled::new(
                            element_type,
                            self.module_path.clone(),
                            literal.span.clone(),
                        ));
                    }
                }
            }
        }

        match canditates.len() {
            0 => {
                let error = TypeError {
                    kind: TypeErrorKind::NoElement,
                    span: literal.span.clone(),
                    module_path: self.module_path.clone(),
                };
                errors.push(error);

                self.create_type_variable_id_with_type(Moduled::new(
                    Type::Unknown,
                    self.module_path.clone(),
                    literal.span.clone(),
                ))
            }
            1 => self.create_type_variable_id_with_type(Moduled::new(
                canditates.first().unwrap().value.clone(),
                self.module_path.clone(),
                literal.span.clone(),
            )),
            _ => {
                let error = TypeError {
                    kind: TypeErrorKind::MultiImplements {
                        concretes: concretes
                            .into_iter()
                            .map(|concrete| {
                                concrete.map(|ty| ty.to_display_string(user_type_set, Some(self)))
                            })
                            .collect(),
                    },
                    span: literal.span.clone(),
                    module_path: self.module_path.clone(),
                };
                errors.push(error);

                self.create_type_variable_id_with_type(Moduled::new(
                    canditates.first().unwrap().value.clone(),
                    self.module_path.clone(),
                    literal.span.clone(),
                ))
            }
        }
    }

    pub fn create_instance(&mut self, ty: &Type, user_type_set: &GlobalUserTypeSet) -> Type {
        match ty {
            Type::UserType {
                user_type_info: user_type_id,
                generics,
            } => {
                let user_type_info = user_type_set.get(*user_type_id);
                let user_type_info = user_type_info.read().unwrap();

                let mut new_generics = std::vec::Vec::with_capacity(generics.len());
                for _ in user_type_info.generics.iter() {
                    new_generics.push(Type::TypeVariable(self.create_type_variable_id_with_set()));
                }

                Type::UserType {
                    user_type_info: *user_type_id,
                    generics: Arc::new(new_generics),
                }
            }
            Type::Function {
                function_info,
                generics,
            } => {
                let mut new_generics = std::vec::Vec::with_capacity(generics.len());
                for _ in function_info.generics.iter() {
                    new_generics.push(Type::TypeVariable(self.create_type_variable_id_with_set()));
                }

                let new_generics = Arc::new(new_generics.clone());

                let ty = Type::Function {
                    function_info: function_info.clone(),
                    generics: new_generics.clone(),
                };

                let old_generics = &function_info.generics;

                ty.replace_generics(old_generics, &new_generics)
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
                    new_arguments.push(
                        argument
                            .clone()
                            .map(|argument| self.resolve_type(&argument, user_type_set)),
                    );
                }

                let new_return_type = function_info
                    .return_type
                    .clone()
                    .map(|return_type| self.resolve_type(&return_type, user_type_set));

                let where_clause = function_info
                    .where_clause
                    .iter()
                    .map(|where_clause| WhereClauseInfo {
                        target: where_clause
                            .target
                            .clone()
                            .map(|ty| self.resolve_type(&ty, user_type_set)),
                        bounds: where_clause
                            .bounds
                            .iter()
                            .map(|ty| ty.clone().map(|ty| self.resolve_type(&ty, user_type_set)))
                            .collect(),
                    })
                    .collect();

                Type::Function {
                    function_info: Arc::new(FunctionTypeInfo {
                        module_path: function_info.module_path.clone(),
                        name: function_info.name.as_ref().cloned(),
                        generics: function_info.generics.clone(),
                        arguments: new_arguments,
                        return_type: new_return_type,
                        where_clause,
                        span: function_info.span.clone(),
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
                    new_arguments.push(
                        argument
                            .clone()
                            .map(|argument| self.retake_type_variable(&argument)),
                    );
                }

                let new_return_type = function_info
                    .return_type
                    .clone()
                    .map(|return_type| self.retake_type_variable(&return_type));

                let where_clause = function_info
                    .where_clause
                    .iter()
                    .map(|where_clause| WhereClauseInfo {
                        target: where_clause
                            .target
                            .clone()
                            .map(|ty| self.retake_type_variable(&ty)),
                        bounds: where_clause
                            .bounds
                            .iter()
                            .map(|ty| ty.clone().map(|ty| self.retake_type_variable(&ty)))
                            .collect(),
                    })
                    .collect();

                Type::Function {
                    function_info: Arc::new(FunctionTypeInfo {
                        module_path: function_info.module_path.clone(),
                        name: function_info.name.as_ref().cloned(),
                        generics: function_info.generics.clone(),
                        arguments: new_arguments,
                        return_type: new_return_type,
                        where_clause,
                        span: function_info.span.clone(),
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
    implements_element_checker: &ImplementsElementChecker,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    moduled_name_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
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
        false,
        &mut type_environment,
        implements_element_checker,
        &Moduled::new(Type::Unit, module_path.clone(), ast.span.clone()),
        &None,
        generics,
        implements_infos,
        import_map,
        module_entity_type_map,
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
    in_user_type_define: bool,
    type_environment: &mut TypeEnvironment,
    implements_element_checker: &ImplementsElementChecker,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    moduled_name_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
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
                    implements_element_checker,
                    return_type,
                    this_type,
                    generics,
                    implements_infos,
                    import_map,
                    module_entity_type_map,
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
                        implements_element_checker,
                        return_type,
                        this_type,
                        generics,
                        implements_infos,
                        import_map,
                        module_entity_type_map,
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
                        Define::Function(function_define) => {
                            let mut implements_infos =
                                ImplementsInfoSet::new(Some(implements_infos));

                            generics_define_register_and_bounds_check(
                                &function_define.generics,
                                &function_define.where_clause,
                                this_type,
                                generics,
                                import_map,
                                module_entity_type_map,
                                moduled_name_type_map,
                                name_resolved_map,
                                user_type_set,
                                module_path,
                                errors,
                                &mut implements_infos,
                                type_environment,
                            );

                            if let Ok(arguments) = &function_define.arguments {
                                for argument in arguments.arguments.iter() {
                                    let binding_type_id = infer_type_for_variable_binding(
                                        &argument.binding,
                                        type_environment,
                                        implements_element_checker,
                                        module_path,
                                    );

                                    if let Ok(type_info) = &argument.type_tag.type_info {
                                        let ty = get_type(
                                            type_info,
                                            this_type,
                                            generics,
                                            import_map,
                                            module_entity_type_map,
                                            moduled_name_type_map,
                                            name_resolved_map,
                                            user_type_set,
                                            module_path,
                                            errors,
                                            &mut Some((&implements_infos, type_environment)),
                                        );

                                        type_environment.unify_type(
                                            Moduled::new(
                                                Type::TypeVariable(binding_type_id),
                                                module_path.clone(),
                                                argument.binding.span(),
                                            ),
                                            Moduled::new(
                                                ty,
                                                module_path.clone(),
                                                type_info.span.clone(),
                                            ),
                                            user_type_set,
                                            errors,
                                        );
                                    }
                                }
                            }

                            let return_type = match &function_define.return_type {
                                Some(return_type_tag) => match &return_type_tag.type_info {
                                    Ok(return_type_info) => {
                                        let ty = get_type(
                                            return_type_info,
                                            this_type,
                                            generics,
                                            import_map,
                                            module_entity_type_map,
                                            moduled_name_type_map,
                                            name_resolved_map,
                                            user_type_set,
                                            module_path,
                                            errors,
                                            &mut Some((&implements_infos, type_environment)),
                                        );

                                        Moduled::new(
                                            ty,
                                            module_path.clone(),
                                            return_type_info.span.clone(),
                                        )
                                    }
                                    Err(_) => Moduled::new(
                                        Type::Unknown,
                                        module_path.clone(),
                                        return_type_tag.span.clone(),
                                    ),
                                },
                                None => Moduled::new(
                                    Type::Unit,
                                    module_path.clone(),
                                    function_define.span.clone(),
                                ),
                            };

                            if let Some(block) = &function_define.block {
                                infer_type_for_program(
                                    block.program,
                                    as_expression,
                                    in_user_type_define,
                                    type_environment,
                                    implements_element_checker,
                                    &return_type,
                                    this_type,
                                    generics,
                                    &implements_infos,
                                    import_map,
                                    module_entity_type_map,
                                    moduled_name_type_map,
                                    name_resolved_map,
                                    user_type_set,
                                    module_path,
                                    package_resource_set,
                                    errors,
                                );
                            }
                        }
                        Define::UserType(user_type_define) => {
                            let mut implements_infos =
                                ImplementsInfoSet::new(Some(implements_infos));

                            generics_define_register_and_bounds_check(
                                &user_type_define.generics,
                                &user_type_define.where_clause,
                                this_type,
                                generics,
                                import_map,
                                module_entity_type_map,
                                moduled_name_type_map,
                                name_resolved_map,
                                user_type_set,
                                module_path,
                                errors,
                                &mut implements_infos,
                                type_environment,
                            );

                            let user_type = module_entity_type_map
                                .get(&EntityID::from(user_type_define))
                                .unwrap()
                                .clone();

                            if let Some(super_type_info) = &user_type_define.super_type {
                                for super_type in super_type_info.types.iter() {
                                    get_type(
                                        super_type,
                                        this_type,
                                        generics,
                                        import_map,
                                        module_entity_type_map,
                                        moduled_name_type_map,
                                        name_resolved_map,
                                        user_type_set,
                                        module_path,
                                        errors,
                                        &mut Some((&implements_infos, type_environment)),
                                    );
                                }
                            }

                            if let Ok(block) = &user_type_define.block {
                                infer_type_for_program(
                                    block.program,
                                    false,
                                    true,
                                    type_environment,
                                    implements_element_checker,
                                    return_type,
                                    &Some(user_type),
                                    generics,
                                    &implements_infos,
                                    import_map,
                                    module_entity_type_map,
                                    moduled_name_type_map,
                                    name_resolved_map,
                                    user_type_set,
                                    module_path,
                                    package_resource_set,
                                    errors,
                                );
                            }
                        }
                        Define::Variable(variable_define) => {
                            let required_type_infer = !define_with_attribute
                                .attribute
                                .iter()
                                .any(|attribute| attribute.value == StatementAttribute::Static)
                                && !in_user_type_define;

                            if required_type_infer {
                                let binding_id = match &variable_define.binding {
                                    Ok(binding) => infer_type_for_variable_binding(
                                        binding,
                                        type_environment,
                                        implements_element_checker,
                                        module_path,
                                    ),
                                    Err(_) => type_environment.create_type_variable_id_with_set(),
                                };

                                let expression_id = match &variable_define.expression {
                                    Some(expression) => infer_type_for_expression(
                                        expression,
                                        as_expression,
                                        type_environment,
                                        implements_element_checker,
                                        return_type,
                                        this_type,
                                        generics,
                                        implements_infos,
                                        import_map,
                                        module_entity_type_map,
                                        moduled_name_type_map,
                                        name_resolved_map,
                                        user_type_set,
                                        module_path,
                                        package_resource_set,
                                        errors,
                                    ),
                                    None => type_environment.create_type_variable_id_with_set(),
                                };

                                if let Some(type_tag) = &variable_define.type_tag {
                                    if let Ok(type_info) = &type_tag.type_info {
                                        let ty = get_type(
                                            type_info,
                                            this_type,
                                            generics,
                                            import_map,
                                            module_entity_type_map,
                                            moduled_name_type_map,
                                            name_resolved_map,
                                            user_type_set,
                                            module_path,
                                            errors,
                                            &mut Some((implements_infos, type_environment)),
                                        );

                                        let ty_variable_id = type_environment
                                            .create_type_variable_id_with_type(ty.moduled(
                                                module_path.clone(),
                                                type_info.span.clone(),
                                            ));

                                        type_environment.unify(
                                            binding_id,
                                            ty_variable_id,
                                            user_type_set,
                                            errors,
                                        );
                                    }
                                }

                                type_environment.unify(
                                    binding_id,
                                    expression_id,
                                    user_type_set,
                                    errors,
                                );
                            }
                        }
                        Define::TypeAlias(_) => {}
                    }
                }
            }
            Statement::Drop(drop_statement) => todo!(),
            Statement::Expression(expression) => {
                infer_type_for_expression(
                    expression,
                    as_expression,
                    type_environment,
                    implements_element_checker,
                    return_type,
                    this_type,
                    generics,
                    implements_infos,
                    import_map,
                    module_entity_type_map,
                    moduled_name_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    package_resource_set,
                    errors,
                );
            }
            Statement::Implements(implements) => {
                let mut implements_infos = ImplementsInfoSet::new(Some(implements_infos));

                generics_define_register_and_bounds_check(
                    &implements.generics,
                    &implements.where_clause,
                    this_type,
                    generics,
                    import_map,
                    module_entity_type_map,
                    moduled_name_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                    &mut implements_infos,
                    type_environment,
                );

                let this_type = match &implements.concrete {
                    Ok(concrete) => get_type(
                        concrete,
                        this_type,
                        generics,
                        import_map,
                        module_entity_type_map,
                        moduled_name_type_map,
                        name_resolved_map,
                        user_type_set,
                        module_path,
                        errors,
                        &mut Some((&implements_infos, type_environment)),
                    ),
                    Err(_) => Type::Unknown,
                };

                let interface = match &implements.interface {
                    Ok(interface) => get_type(
                        interface,
                        &Some(this_type.clone()),
                        generics,
                        import_map,
                        module_entity_type_map,
                        moduled_name_type_map,
                        name_resolved_map,
                        user_type_set,
                        module_path,
                        errors,
                        &mut Some((&implements_infos, type_environment)),
                    ),
                    Err(_) => Type::Unknown,
                };

                if let Ok(block) = &implements.block {
                    infer_type_for_program(
                        block.program,
                        as_expression,
                        in_user_type_define,
                        type_environment,
                        implements_element_checker,
                        return_type,
                        &Some(this_type),
                        generics,
                        &implements_infos,
                        import_map,
                        module_entity_type_map,
                        moduled_name_type_map,
                        name_resolved_map,
                        user_type_set,
                        module_path,
                        package_resource_set,
                        errors,
                    );
                }
            }
        }
    }
}

fn generics_define_register_and_bounds_check(
    generics_ast: &Option<GenericsDefine>,
    where_clause_ast: &Option<WhereClause>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    moduled_name_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut std::vec::Vec<TypeError>,
    implements_infos: &mut ImplementsInfoSet,
    type_environment: &mut TypeEnvironment,
) {
    if let Some(generics_define) = generics_ast {
        for generic in generics_define.elements.iter() {
            let generic_type = generics.get(&EntityID::from(generic)).unwrap();

            for bound in generic.bounds.iter() {
                let bound_type = get_type(
                    bound,
                    this_type,
                    generics,
                    import_map,
                    module_entity_type_map,
                    moduled_name_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                    &mut None,
                );

                implements_infos.register_implements_info(ImplementsInfo {
                    generics_define: std::vec::Vec::new(),
                    interface: bound_type.with_span(bound.span.clone()),
                    concrete: Type::Generic(generic_type.clone())
                        .with_span(generic.name.span.clone()),
                    where_clause: std::vec::Vec::new(),
                    element_type: HashMap::new(),
                    module_path: module_path.clone(),
                    is_instant: true,
                });
            }
        }
    }

    if let Some(where_clause) = where_clause_ast {
        for where_element in where_clause.elements.iter() {
            let target_type = get_type(
                &where_element.target_type,
                this_type,
                generics,
                import_map,
                module_entity_type_map,
                moduled_name_type_map,
                name_resolved_map,
                user_type_set,
                module_path,
                errors,
                &mut None,
            );

            for bound in where_element.bounds.iter() {
                let bound_type = get_type(
                    bound,
                    this_type,
                    generics,
                    import_map,
                    module_entity_type_map,
                    moduled_name_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                    &mut None,
                );

                implements_infos.register_implements_info(ImplementsInfo {
                    generics_define: std::vec::Vec::new(),
                    interface: bound_type.with_span(bound.span.clone()),
                    concrete: target_type
                        .clone()
                        .with_span(where_element.target_type.span.clone()),
                    where_clause: std::vec::Vec::new(),
                    element_type: HashMap::new(),
                    module_path: module_path.clone(),
                    is_instant: true,
                });
            }
        }
    }

    if let Some(generics_define) = generics_ast {
        for generic in generics_define.elements.iter() {
            for bound in generic.bounds.iter() {
                // generics bounds check
                get_type(
                    bound,
                    this_type,
                    generics,
                    import_map,
                    module_entity_type_map,
                    moduled_name_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                    &mut Some((implements_infos, type_environment)),
                );
            }
        }
    }

    if let Some(where_clause) = where_clause_ast {
        for where_element in where_clause.elements.iter() {
            // generics bounds check
            get_type(
                &where_element.target_type,
                this_type,
                generics,
                import_map,
                module_entity_type_map,
                moduled_name_type_map,
                name_resolved_map,
                user_type_set,
                module_path,
                errors,
                &mut Some((implements_infos, type_environment)),
            );

            for bound in where_element.bounds.iter() {
                // generics bounds check
                get_type(
                    bound,
                    this_type,
                    generics,
                    import_map,
                    module_entity_type_map,
                    moduled_name_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                    &mut Some((implements_infos, type_environment)),
                );
            }
        }
    }
}

fn infer_type_for_variable_binding(
    ast: &VariableBinding,
    type_environment: &mut TypeEnvironment,
    implements_element_checker: &ImplementsElementChecker,
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
                    infer_type_for_variable_binding(
                        binding,
                        type_environment,
                        implements_element_checker,
                        module_path,
                    )
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
    implements_element_checker: &ImplementsElementChecker,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    moduled_name_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
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
                    implements_element_checker,
                    return_type,
                    this_type,
                    generics,
                    implements_infos,
                    import_map,
                    module_entity_type_map,
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
            implements_element_checker,
            return_type,
            this_type,
            generics,
            implements_infos,
            import_map,
            module_entity_type_map,
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
    implements_element_checker: &ImplementsElementChecker,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    moduled_name_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
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
            implements_element_checker,
            return_type,
            this_type,
            generics,
            implements_infos,
            import_map,
            module_entity_type_map,
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
    implements_element_checker: &ImplementsElementChecker,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    moduled_name_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
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
            implements_element_checker,
            return_type,
            this_type,
            generics,
            implements_infos,
            import_map,
            module_entity_type_map,
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
    implements_element_checker: &ImplementsElementChecker,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    moduled_name_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
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
            implements_element_checker,
            return_type,
            this_type,
            generics,
            implements_infos,
            import_map,
            module_entity_type_map,
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
    implements_element_checker: &ImplementsElementChecker,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    moduled_name_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
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
            implements_element_checker,
            return_type,
            this_type,
            generics,
            implements_infos,
            import_map,
            module_entity_type_map,
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
    implements_element_checker: &ImplementsElementChecker,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    moduled_name_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
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
            implements_element_checker,
            return_type,
            this_type,
            generics,
            implements_infos,
            import_map,
            module_entity_type_map,
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
    implements_element_checker: &ImplementsElementChecker,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    moduled_name_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
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
            implements_element_checker,
            return_type,
            this_type,
            generics,
            implements_infos,
            import_map,
            module_entity_type_map,
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
    implements_element_checker: &ImplementsElementChecker,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    moduled_name_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
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
                implements_element_checker,
                return_type,
                this_type,
                generics,
                implements_infos,
                import_map,
                module_entity_type_map,
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
    implements_element_checker: &ImplementsElementChecker,
    return_type: &Moduled<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    moduled_name_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
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
            generics: generics_info,
            function_call,
            span: _,
        } => {
            let mut last_type_variable_id = match left {
                SimplePrimary::Tuple { expressions, span } => todo!(),
                SimplePrimary::Literal(literal) => {
                    match name_resolved_map.get(&EntityID::from(literal)) {
                        Some(resolved) => match resolved.define.kind {
                            DefineKind::Import => {
                                match import_map
                                    .get(&resolved.define.entity_id)
                                    .unwrap()
                                    .first()
                                    .unwrap_or(&ImportElement::Unknown)
                                {
                                    ImportElement::ModuleAlias { path } => {
                                        let path_name =
                                            path.iter().cloned().collect::<Vec<_>>().join("::");

                                        get_module_import_type(
                                            ast,
                                            literal,
                                            path_name,
                                            &mut chain_start_position,
                                            type_environment,
                                            implements_element_checker,
                                            moduled_name_type_map,
                                            package_resource_set,
                                            module_path,
                                            errors,
                                        )
                                    }
                                    ImportElement::ModuleElement { path, element: _ } => {
                                        let module_name =
                                            path.iter().cloned().collect::<Vec<_>>().join("::");
                                        let module_type = moduled_name_type_map
                                            .get(&module_name)
                                            .unwrap()
                                            .get(literal.value)
                                            .unwrap();

                                        let instance = type_environment
                                            .create_instance(module_type, user_type_set);

                                        Spanned::new(
                                            type_environment.create_and_set_entity_type(
                                                literal.into(),
                                                Moduled::new(
                                                    instance,
                                                    module_path.clone(),
                                                    literal.span.clone(),
                                                ),
                                            ),
                                            literal.span.clone(),
                                        )
                                    }
                                    ImportElement::Unknown => Spanned::new(
                                        type_environment.create_and_set_entity_type(
                                            literal.into(),
                                            Moduled::new(
                                                Type::Unknown,
                                                module_path.clone(),
                                                literal.span.clone(),
                                            ),
                                        ),
                                        literal.span.clone(),
                                    ),
                                }
                            }
                            DefineKind::Function => {
                                let function_type = module_entity_type_map
                                    .get(&resolved.define.entity_id)
                                    .unwrap();

                                let function_type =
                                    type_environment.create_instance(function_type, user_type_set);

                                type_environment
                                    .create_type_variable_id_with_type(
                                        function_type
                                            .moduled(module_path.clone(), literal.span.clone()),
                                    )
                                    .with_span(literal.span.clone())
                            }
                            DefineKind::Variable => Spanned::new(
                                type_environment
                                    .get_or_allocate_variable_for_entity(resolved.define.entity_id),
                                literal.span.clone(),
                            ),
                            DefineKind::UserType => todo!(),
                            DefineKind::Generics => todo!(),
                        },
                        None => match package_resource_set.get(literal.value) {
                            Some(_) => get_module_import_type(
                                ast,
                                literal,
                                literal.value.to_string(),
                                &mut chain_start_position,
                                type_environment,
                                implements_element_checker,
                                moduled_name_type_map,
                                package_resource_set,
                                module_path,
                                errors,
                            ),
                            None => Spanned::new(
                                type_environment.create_type_variable_id_with_set(),
                                literal.span.clone(),
                            ),
                        },
                    }
                }
                SimplePrimary::StringLiteral(literal) => todo!(),
                SimplePrimary::NumericLiteral(literal) => Spanned::new(
                    type_environment.create_and_set_entity_type(
                        EntityID::from(literal),
                        Moduled::new(
                            Type::IntegerLiteral,
                            module_path.clone(),
                            literal.span.clone(),
                        ),
                    ),
                    literal.span.clone(),
                ),
                SimplePrimary::Null(range) => todo!(),
                SimplePrimary::True(range) => todo!(),
                SimplePrimary::False(range) => todo!(),
                SimplePrimary::This(range) => todo!(),
                SimplePrimary::LargeThis(range) => todo!(),
            };

            if let Some(function_call) = function_call {
                let mut arguments = function_call
                    .arguments
                    .iter()
                    .map(|expression| {
                        infer_type_for_expression(
                            expression,
                            as_expression,
                            type_environment,
                            implements_element_checker,
                            return_type,
                            this_type,
                            generics,
                            implements_infos,
                            import_map,
                            module_entity_type_map,
                            moduled_name_type_map,
                            name_resolved_map,
                            user_type_set,
                            module_path,
                            package_resource_set,
                            errors,
                        )
                        .with_span(expression.span())
                    })
                    .collect::<Vec<_>>();

                let last_type =
                    type_environment.get_type_variable_type(last_type_variable_id.value);

                match last_type {
                    Some(last_type) => match &last_type.value {
                        Type::Function {
                            function_info,
                            generics,
                        } => {
                            if function_info.arguments.len() != arguments.len() {
                                let function_arguments_span_start = function_info
                                    .arguments
                                    .first()
                                    .map(|argument| argument.span.start)
                                    .unwrap_or(function_info.span.start);
                                let function_arguments_span_end = function_info
                                    .arguments
                                    .last()
                                    .map(|argument| argument.span.end)
                                    .unwrap_or(function_info.span.end);

                                let error = TypeError {
                                    kind: TypeErrorKind::FunctionArgumentCountMismatch {
                                        expected: function_info.arguments.len().moduled(
                                            function_info.module_path.clone(),
                                            function_arguments_span_start
                                                ..function_arguments_span_end,
                                        ),
                                        found: arguments.len().moduled(
                                            module_path.clone(),
                                            function_call.span.clone(),
                                        ),
                                    },
                                    span: function_call.span.clone(),
                                    module_path: module_path.clone(),
                                };
                                errors.push(error);
                            }

                            for (origin_argument, argument) in
                                function_info.arguments.iter().zip(arguments.into_iter())
                            {
                                type_environment.unify_type(
                                    origin_argument.value.clone().moduled(
                                        function_info.module_path.clone(),
                                        origin_argument.span.clone(),
                                    ),
                                    Type::TypeVariable(argument.value)
                                        .moduled(module_path.clone(), argument.span),
                                    user_type_set,
                                    errors,
                                );
                            }

                            let mut where_bounds = Vec::new();

                            for generic in function_info.generics.iter() {
                                where_bounds.push(WhereClauseInfo {
                                    target: Type::Generic(generic.clone())
                                        .with_span(generic.name.span.clone()),
                                    bounds: generic.bounds.read().unwrap().clone(),
                                });
                            }
                            where_bounds.extend(function_info.where_clause.clone());

                            let old_generics = &function_info.generics;
                            let new_generics = generics;

                            for where_bound in where_bounds.iter() {
                                let target_type = where_bound
                                    .target
                                    .value
                                    .replace_generics(old_generics, new_generics)
                                    .moduled(
                                        function_info.module_path.clone(),
                                        where_bound.target.span.clone(),
                                    );

                                for bound in where_bound.bounds.iter() {
                                    let bound_type = bound
                                        .value
                                        .replace_generics(old_generics, new_generics)
                                        .moduled(
                                            function_info.module_path.clone(),
                                            bound.span.clone(),
                                        );

                                    match implements_infos.is_implements(
                                        target_type.clone(),
                                        bound_type.clone(),
                                        None,
                                        user_type_set,
                                        type_environment,
                                    ) {
                                        ImplementsCheckResult::NoImplementsFound => {
                                            let error = TypeError {
                                                kind: TypeErrorKind::NotSatisfied {
                                                    target: target_type.clone().map(|ty| {
                                                        ty.to_display_string(
                                                            user_type_set,
                                                            Some(type_environment),
                                                        )
                                                    }),
                                                    origin_target: target_type.clone().map(|ty| {
                                                        ty.to_display_string(
                                                            user_type_set,
                                                            Some(type_environment),
                                                        )
                                                    }),
                                                    origin_bound: bound_type.clone().map(|ty| {
                                                        ty.to_display_string(
                                                            user_type_set,
                                                            Some(type_environment),
                                                        )
                                                    }),
                                                },
                                                span: function_call.span.clone(),
                                                module_path: module_path.clone(),
                                            };
                                            errors.push(error);
                                        }
                                        ImplementsCheckResult::Conflicts { conflicts } => {
                                            let error = TypeError {
                                                kind:
                                                    TypeErrorKind::ConflictedImplementInTypeInfer {
                                                        conflicts: conflicts
                                                            .into_iter()
                                                            .map(|ty| {
                                                                ty.map(|ty| {
                                                                    ty.to_display_string(
                                                                        user_type_set,
                                                                        Some(type_environment),
                                                                    )
                                                                })
                                                            })
                                                            .collect(),
                                                    },
                                                span: function_call.span.clone(),
                                                module_path: module_path.clone(),
                                            };
                                            errors.push(error);
                                        }
                                        ImplementsCheckResult::Success {
                                            new_concrete,
                                            new_interface,
                                            new_generics: _,
                                        } => {
                                            if !type_environment.test_unify_type(
                                                target_type.clone(),
                                                new_concrete,
                                                user_type_set,
                                            ) {
                                                unreachable!(
                                                    "function call bounds unify(concrete) must not fail!"
                                                );
                                            }
                                            if !type_environment.test_unify_type(
                                                new_interface.unwrap(),
                                                bound_type,
                                                user_type_set,
                                            ) {
                                                unreachable!(
                                                    "function call bounds unify(interface) must not fail!"
                                                );
                                            }
                                        }
                                    }
                                }
                            }

                            last_type_variable_id = type_environment
                                .create_type_variable_id_with_type(
                                    function_info
                                        .return_type
                                        .value
                                        .clone()
                                        .moduled(module_path.clone(), function_call.span.clone()),
                                )
                                .with_span(function_call.span.clone())
                        }
                        _ => {
                            let error = TypeError {
                                kind: TypeErrorKind::NotFunctionType {
                                    not_function_type_from: Type::Unknown
                                        .to_display_string(user_type_set, Some(type_environment))
                                        .with_span(last_type_variable_id.span.clone()),
                                },
                                span: function_call.span.clone(),
                                module_path: module_path.clone(),
                            };
                            errors.push(error);

                            last_type_variable_id = type_environment
                                .create_type_variable_id_with_type(
                                    Type::Unknown
                                        .moduled(module_path.clone(), function_call.span.clone()),
                                )
                                .with_span(function_call.span.clone())
                        }
                    },
                    None => {
                        let error = TypeError {
                            kind: TypeErrorKind::NotFunctionType {
                                not_function_type_from: Type::Unknown
                                    .to_display_string(user_type_set, Some(type_environment))
                                    .with_span(last_type_variable_id.span.clone()),
                            },
                            span: function_call.span.clone(),
                            module_path: module_path.clone(),
                        };
                        errors.push(error);

                        last_type_variable_id = type_environment
                            .create_type_variable_id_with_type(
                                Type::Unknown
                                    .moduled(module_path.clone(), function_call.span.clone()),
                            )
                            .with_span(function_call.span.clone());
                    }
                }
            }

            last_type_variable_id
        }
        PrimaryLeftExpr::NewObject { new_object } => {
            fn infer_type_for_new_object_expression(
                ast: &NewObjectExpression,
                object_type: Spanned<Type>,
                type_environment: &mut TypeEnvironment,
                implements_element_checker: &ImplementsElementChecker,
                return_type: &Moduled<Type>,
                this_type: &Option<Type>,
                generics: &HashMap<EntityID, Arc<GenericType>>,
                implements_infos: &ImplementsInfoSet,
                import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
                module_entity_type_map: &HashMap<EntityID, Type>,
                moduled_name_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
                name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
                user_type_set: &GlobalUserTypeSet,
                module_path: &ModulePath,
                package_resource_set: &PackageResourceSet,
                errors: &mut std::vec::Vec<TypeError>,
            ) -> Spanned<TypeVariableID> {
                match object_type.value.resolve_type_alias(user_type_set) {
                    Type::UserType {
                        user_type_info: user_type_id,
                        generics: _,
                    } => {
                        let user_type_info = user_type_set.get(user_type_id);
                        let user_type_info = user_type_info.read().unwrap();

                        let old_generics = &user_type_info.generics;
                        let new_generics = (0..old_generics.len())
                            .map(|_| {
                                Type::TypeVariable(
                                    type_environment.create_type_variable_id_with_set(),
                                )
                            })
                            .collect::<std::vec::Vec<_>>();

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
                                implements_element_checker,
                                return_type,
                                this_type,
                                generics,
                                implements_infos,
                                import_map,
                                module_entity_type_map,
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

                        let type_variable_id =
                            type_environment.create_type_variable_id_with_type(Moduled::new(
                                Type::UserType {
                                    user_type_info: user_type_id,
                                    generics: Arc::new(new_generics),
                                },
                                module_path.clone(),
                                ast.span.clone(),
                            ));

                        Spanned::new(type_variable_id, ast.span.clone())
                    }
                    _ => {
                        let error = TypeError {
                            kind: TypeErrorKind::NotUserTypeInNewObjectExpr,
                            span: object_type.span.clone(),
                            module_path: module_path.clone(),
                        };
                        errors.push(error);

                        let type_variable_id = type_environment.create_type_variable_id_with_type(
                            Moduled::new(Type::Unknown, module_path.clone(), ast.span.clone()),
                        );

                        Spanned::new(type_variable_id, ast.span.clone())
                    }
                }
            }

            match new_object.path.first() {
                Some(literal) => match name_resolved_map.get(&EntityID::from(literal)) {
                    Some(resolved) => match resolved.define.kind {
                        DefineKind::Import => {
                            match new_object.path.len() {
                                1 => match import_map
                                    .get(&resolved.define.entity_id)
                                    .unwrap()
                                    .first()
                                    .unwrap_or(&ImportElement::Unknown)
                                {
                                    ImportElement::ModuleElement { path, element: _ } => {
                                        let module_path_name =
                                            path.iter().cloned().collect::<Vec<_>>().join("::");
                                        let ty = moduled_name_type_map
                                            .get(&module_path_name)
                                            .unwrap()
                                            .get(new_object.path.first().unwrap().value)
                                            .unwrap();

                                        infer_type_for_new_object_expression(
                                            new_object,
                                            Spanned::new(ty.clone(), literal.span.clone()),
                                            type_environment,
                                            implements_element_checker,
                                            return_type,
                                            this_type,
                                            generics,
                                            implements_infos,
                                            import_map,
                                            module_entity_type_map,
                                            moduled_name_type_map,
                                            name_resolved_map,
                                            user_type_set,
                                            module_path,
                                            package_resource_set,
                                            errors,
                                        )
                                    }
                                    _ => {
                                        let error = TypeError {
                                            kind: TypeErrorKind::UnknownModuleElementType,
                                            span: literal.span.clone(),
                                            module_path: module_path.clone(),
                                        };
                                        errors.push(error);

                                        Spanned::new(
                                            type_environment.create_type_variable_id_with_type(
                                                Moduled::new(
                                                    Type::Unknown,
                                                    module_path.clone(),
                                                    literal.span.clone(),
                                                ),
                                            ),
                                            literal.span.clone(),
                                        )
                                    }
                                },
                                _ => {
                                    match import_map
                                        .get(&resolved.define.entity_id)
                                        .unwrap()
                                        .first()
                                        .unwrap_or(&ImportElement::Unknown)
                                    {
                                        ImportElement::ModuleAlias { path } => {
                                            let module_path_name = path
                                                .iter()
                                                .cloned()
                                                .chain(
                                                    new_object.path[..new_object.path.len() - 1]
                                                        .iter()
                                                        .map(|path| path.value.to_string()),
                                                )
                                                .collect::<Vec<_>>()
                                                .join("::");

                                            match moduled_name_type_map.get(&module_path_name) {
                                                Some(name_type_map) => {
                                                    match name_type_map
                                                        .get(new_object.path.last().unwrap().value)
                                                    {
                                                        Some(ty) => {
                                                            infer_type_for_new_object_expression(
                                                                new_object,
                                                                Spanned::new(
                                                                    ty.clone(),
                                                                    new_object
                                                                        .path
                                                                        .last()
                                                                        .unwrap()
                                                                        .span
                                                                        .clone(),
                                                                ),
                                                                type_environment,
                                                                implements_element_checker,
                                                                return_type,
                                                                this_type,
                                                                generics,
                                                                implements_infos,
                                                                import_map,
                                                                module_entity_type_map,
                                                                moduled_name_type_map,
                                                                name_resolved_map,
                                                                user_type_set,
                                                                module_path,
                                                                package_resource_set,
                                                                errors,
                                                            )
                                                        }
                                                        None => {
                                                            let error = TypeError {
                                                        kind: TypeErrorKind::UnknownModuleElementType,
                                                        span: new_object.path.last().unwrap().span.clone(),
                                                        module_path: module_path.clone(),
                                                    };
                                                            errors.push(error);

                                                            Spanned::new(type_environment
                                                            .create_type_variable_id_with_type(
                                                                Moduled::new(
                                                                    Type::Unknown,
                                                                    module_path.clone(),
                                                                    new_object
                                                                        .path
                                                                        .last()
                                                                        .unwrap()
                                                                        .span
                                                                        .clone(),
                                                                ),
                                                            ), new_object.path.last().unwrap().span.clone())
                                                        }
                                                    }
                                                }
                                                None => Spanned::new(
                                                    type_environment
                                                        .create_type_variable_id_with_type(
                                                            Moduled::new(
                                                                Type::Unknown,
                                                                module_path.clone(),
                                                                new_object.span.clone(),
                                                            ),
                                                        ),
                                                    new_object.span.clone(),
                                                ),
                                            }
                                        }
                                        _ => {
                                            let error = TypeError {
                                                kind: TypeErrorKind::UnknownModuleElementType,
                                                span: new_object.path.last().unwrap().span.clone(),
                                                module_path: module_path.clone(),
                                            };
                                            errors.push(error);

                                            Spanned::new(
                                                type_environment.create_type_variable_id_with_type(
                                                    Moduled::new(
                                                        Type::Unknown,
                                                        module_path.clone(),
                                                        new_object
                                                            .path
                                                            .last()
                                                            .unwrap()
                                                            .span
                                                            .clone(),
                                                    ),
                                                ),
                                                new_object.path.last().unwrap().span.clone(),
                                            )
                                        }
                                    }
                                }
                            }
                        }
                        DefineKind::UserType => match new_object.path.len() {
                            1 => {
                                let ty = module_entity_type_map
                                    .get(&resolved.define.entity_id)
                                    .unwrap();

                                infer_type_for_new_object_expression(
                                    new_object,
                                    Spanned::new(
                                        ty.clone(),
                                        new_object.path.first().unwrap().span.clone(),
                                    ),
                                    type_environment,
                                    implements_element_checker,
                                    return_type,
                                    this_type,
                                    generics,
                                    implements_infos,
                                    import_map,
                                    module_entity_type_map,
                                    moduled_name_type_map,
                                    name_resolved_map,
                                    user_type_set,
                                    module_path,
                                    package_resource_set,
                                    errors,
                                )
                            }
                            _ => {
                                let first_path = new_object.path.first().unwrap();
                                let last_path = new_object.path.last().unwrap();

                                let error = TypeError {
                                    kind: TypeErrorKind::ExtraUserTypePath,
                                    span: first_path.span.start..last_path.span.end,
                                    module_path: module_path.clone(),
                                };
                                errors.push(error);

                                Spanned::new(
                                    type_environment.create_type_variable_id_with_type(
                                        Moduled::new(
                                            Type::Unknown,
                                            module_path.clone(),
                                            new_object.path.last().unwrap().span.clone(),
                                        ),
                                    ),
                                    new_object.span.clone(),
                                )
                            }
                        },
                        _ => {
                            let error = TypeError {
                                kind: TypeErrorKind::NotUserTypeInNewObjectExpr,
                                span: literal.span.clone(),
                                module_path: module_path.clone(),
                            };
                            errors.push(error);

                            Spanned::new(
                                type_environment.create_type_variable_id_with_type(Moduled::new(
                                    Type::Unknown,
                                    module_path.clone(),
                                    new_object.path.last().unwrap().span.clone(),
                                )),
                                new_object.span.clone(),
                            )
                        }
                    },
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
                                        implements_element_checker,
                                        return_type,
                                        this_type,
                                        generics,
                                        implements_infos,
                                        import_map,
                                        module_entity_type_map,
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

                                        Spanned::new(
                                            type_environment.create_type_variable_id_with_type(
                                                Moduled::new(
                                                    Type::Unknown,
                                                    module_path.clone(),
                                                    new_object.span.clone(),
                                                ),
                                            ),
                                            new_object.span.clone(),
                                        )
                                    }
                                }
                            }
                            None => Spanned::new(
                                type_environment.create_type_variable_id_with_type(Moduled::new(
                                    Type::Unknown,
                                    module_path.clone(),
                                    new_object.span.clone(),
                                )),
                                new_object.span.clone(),
                            ),
                        }
                    }
                },
                None => Spanned::new(
                    type_environment.create_type_variable_id_with_type(Moduled::new(
                        Type::Unknown,
                        module_path.clone(),
                        new_object.span.clone(),
                    )),
                    new_object.span.clone(),
                ),
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

        if let Some(second) = &chain.second {
            let function_arguments = match &second.function_call {
                Some(function_call) => {
                    let mut function_arguments = Vec::new();

                    for argument_expression in function_call.arguments.iter() {
                        let expression_variable_id = infer_type_for_expression(
                            argument_expression,
                            as_expression,
                            type_environment,
                            implements_element_checker,
                            return_type,
                            this_type,
                            generics,
                            implements_infos,
                            import_map,
                            module_entity_type_map,
                            moduled_name_type_map,
                            name_resolved_map,
                            user_type_set,
                            module_path,
                            package_resource_set,
                            errors,
                        );

                        let expression_type = Type::TypeVariable(expression_variable_id)
                            .moduled(module_path.clone(), argument_expression.span());

                        function_arguments.push(expression_type);
                    }

                    function_arguments
                }
                None => Vec::new(),
            };

            let function_type_hints = match &second.function_call {
                Some(_) => Some((second.literal.value, function_arguments.as_slice())),
                None => None,
            };

            let element_type = type_environment.get_user_type_element(
                last_type_variable_id,
                &second.literal,
                function_type_hints,
                implements_infos,
                user_type_set,
                errors,
            );

            last_type_variable_id = Spanned::new(element_type, second.literal.span.clone());

            match &second.generics {
                Some(generics_info) => {
                    let ty = type_environment.get_type_variable_type(last_type_variable_id.value);

                    match ty {
                        Some(ty) => match &ty.value {
                            Type::Function {
                                function_info: _,
                                generics: old_generics,
                            }
                            | Type::UserType {
                                user_type_info: _,
                                generics: old_generics,
                            } => {
                                if old_generics.len() != generics_info.types.len() {
                                    let error = TypeError {
                                        kind: TypeErrorKind::InvalidGenericsCount {
                                            expected: old_generics.len(),
                                            found: generics_info.types.len(),
                                            defined: ().moduled(
                                                module_path.clone(),
                                                last_type_variable_id.span.clone(),
                                            ),
                                        },
                                        span: generics_info.span.clone(),
                                        module_path: module_path.clone(),
                                    };
                                    errors.push(error);
                                }

                                for (old_generic, new_generic) in
                                    old_generics.iter().zip(generics_info.types.iter())
                                {
                                    let new_generic_type = get_type(
                                        new_generic,
                                        this_type,
                                        generics,
                                        import_map,
                                        module_entity_type_map,
                                        moduled_name_type_map,
                                        name_resolved_map,
                                        user_type_set,
                                        module_path,
                                        errors,
                                        &mut Some((implements_infos, type_environment)),
                                    );

                                    type_environment.unify_type(
                                        old_generic.clone().moduled(
                                            module_path.clone(),
                                            last_type_variable_id.span.clone(),
                                        ),
                                        new_generic_type
                                            .moduled(module_path.clone(), new_generic.span.clone()),
                                        user_type_set,
                                        errors,
                                    );
                                }
                            }
                            _ => {
                                let error = TypeError {
                                    kind: TypeErrorKind::NotFunctionOrUserType {
                                        invalid_type_from: ty
                                            .value
                                            .to_display_string(
                                                user_type_set,
                                                Some(type_environment),
                                            )
                                            .with_span(last_type_variable_id.span.clone()),
                                    },
                                    span: generics_info.span.clone(),
                                    module_path: module_path.clone(),
                                };
                                errors.push(error);
                            }
                        },
                        None => {
                            let error = TypeError {
                                kind: TypeErrorKind::UnknownTypeAtThisPoint,
                                span: generics_info.span.clone(),
                                module_path: module_path.clone(),
                            };
                            errors.push(error);
                        }
                    }
                }
                None => {}
            }

            if let Some(ty) = type_environment.get_type_variable_type(last_type_variable_id.value) {
                match &ty.value {
                    Type::UserType {
                        user_type_info,
                        generics: new_generics,
                    } => {
                        let mut where_bounds = Vec::new();

                        let user_type_info = user_type_set.get(*user_type_info);
                        let user_type_info = user_type_info.read().unwrap();

                        let old_generics = &user_type_info.generics;

                        for generic in old_generics.iter() {
                            where_bounds.push(WhereClauseInfo {
                                target: Type::Generic(generic.clone())
                                    .with_span(generic.name.span.clone()),
                                bounds: generic.bounds.read().unwrap().clone(),
                            });
                        }
                        where_bounds.extend(user_type_info.where_clause.clone());

                        for where_bound in where_bounds {
                            let target_type = where_bound
                                .target
                                .value
                                .replace_generics(old_generics, new_generics);

                            for bound in where_bound.bounds {
                                let bound_type =
                                    bound.value.replace_generics(old_generics, new_generics);

                                match implements_infos.is_implements(
                                    target_type.clone().moduled(
                                        user_type_info.module_path.clone(),
                                        where_bound.target.span.clone(),
                                    ),
                                    bound_type.moduled(
                                        user_type_info.module_path.clone(),
                                        bound.span.clone(),
                                    ),
                                    None,
                                    user_type_set,
                                    type_environment,
                                ) {
                                    ImplementsCheckResult::NoImplementsFound => {
                                        let error = TypeError {
                                            kind: TypeErrorKind::NotSatisfied {
                                                target: target_type
                                                    .to_display_string(
                                                        user_type_set,
                                                        Some(type_environment),
                                                    )
                                                    .moduled(
                                                        module_path.clone(),
                                                        last_type_variable_id.span.clone(),
                                                    ),
                                                origin_target: where_bound
                                                    .target
                                                    .value
                                                    .to_display_string(
                                                        user_type_set,
                                                        Some(type_environment),
                                                    )
                                                    .moduled(
                                                        user_type_info.module_path.clone(),
                                                        where_bound.target.span.clone(),
                                                    ),
                                                origin_bound: bound
                                                    .value
                                                    .to_display_string(
                                                        user_type_set,
                                                        Some(type_environment),
                                                    )
                                                    .moduled(
                                                        user_type_info.module_path.clone(),
                                                        bound.span.clone(),
                                                    ),
                                            },
                                            span: last_type_variable_id.span.clone(),
                                            module_path: module_path.clone(),
                                        };
                                        errors.push(error);
                                    }
                                    ImplementsCheckResult::Conflicts { conflicts } => {
                                        let error = TypeError {
                                            kind: TypeErrorKind::ConflictedImplementInTypeInfer {
                                                conflicts: conflicts
                                                    .into_iter()
                                                    .map(|ty| {
                                                        ty.map(|ty| {
                                                            ty.to_display_string(
                                                                user_type_set,
                                                                Some(type_environment),
                                                            )
                                                        })
                                                    })
                                                    .collect(),
                                            },
                                            span: last_type_variable_id.span.clone(),
                                            module_path: module_path.clone(),
                                        };
                                        errors.push(error);
                                    }
                                    ImplementsCheckResult::Success {
                                        new_concrete,
                                        new_interface: _,
                                        new_generics: _,
                                    } => {
                                        if !type_environment.test_unify_type(
                                            ty.clone(),
                                            new_concrete,
                                            user_type_set,
                                        ) {
                                            unreachable!(
                                                ":<generics> bounds check unify must not fail!"
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                    Type::Function {
                        function_info,
                        generics: new_generics,
                    } => {
                        let mut where_bounds = Vec::new();

                        let old_generics = &function_info.generics;

                        for generic in old_generics.iter() {
                            where_bounds.push(WhereClauseInfo {
                                target: Type::Generic(generic.clone())
                                    .with_span(generic.name.span.clone()),
                                bounds: generic.bounds.read().unwrap().clone(),
                            });
                        }
                        where_bounds.extend(function_info.where_clause.clone());

                        for where_bound in where_bounds {
                            let target_type = where_bound
                                .target
                                .value
                                .replace_generics(old_generics, new_generics);

                            for bound in where_bound.bounds {
                                let bound_type =
                                    bound.value.replace_generics(old_generics, new_generics);

                                match implements_infos.is_implements(
                                    target_type.clone().moduled(
                                        function_info.module_path.clone(),
                                        where_bound.target.span.clone(),
                                    ),
                                    bound_type.moduled(
                                        function_info.module_path.clone(),
                                        bound.span.clone(),
                                    ),
                                    None,
                                    user_type_set,
                                    type_environment,
                                ) {
                                    ImplementsCheckResult::NoImplementsFound => {
                                        let error = TypeError {
                                            kind: TypeErrorKind::NotSatisfied {
                                                target: target_type
                                                    .to_display_string(
                                                        user_type_set,
                                                        Some(type_environment),
                                                    )
                                                    .moduled(
                                                        module_path.clone(),
                                                        last_type_variable_id.span.clone(),
                                                    ),
                                                origin_target: where_bound
                                                    .target
                                                    .value
                                                    .to_display_string(
                                                        user_type_set,
                                                        Some(type_environment),
                                                    )
                                                    .moduled(
                                                        function_info.module_path.clone(),
                                                        where_bound.target.span.clone(),
                                                    ),
                                                origin_bound: bound
                                                    .value
                                                    .to_display_string(
                                                        user_type_set,
                                                        Some(type_environment),
                                                    )
                                                    .moduled(
                                                        function_info.module_path.clone(),
                                                        bound.span.clone(),
                                                    ),
                                            },
                                            span: last_type_variable_id.span.clone(),
                                            module_path: module_path.clone(),
                                        };
                                        errors.push(error);
                                    }
                                    ImplementsCheckResult::Conflicts { conflicts } => {
                                        let error = TypeError {
                                            kind: TypeErrorKind::ConflictedImplementInTypeInfer {
                                                conflicts: conflicts
                                                    .into_iter()
                                                    .map(|ty| {
                                                        ty.map(|ty| {
                                                            ty.to_display_string(
                                                                user_type_set,
                                                                Some(type_environment),
                                                            )
                                                        })
                                                    })
                                                    .collect(),
                                            },
                                            span: last_type_variable_id.span.clone(),
                                            module_path: module_path.clone(),
                                        };
                                        errors.push(error);
                                    }
                                    ImplementsCheckResult::Success {
                                        new_concrete,
                                        new_interface: _,
                                        new_generics: _,
                                    } => {
                                        if !type_environment.test_unify_type(
                                            ty.clone(),
                                            new_concrete,
                                            user_type_set,
                                        ) {
                                            unreachable!(
                                                ":<generics> bounds check unify must not fail!"
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            if let Some(function_call) = &second.function_call {
                let function_type =
                    type_environment.get_type_variable_type(last_type_variable_id.value);

                last_type_variable_id = match function_type {
                    Some(ty) => match &ty.value {
                        Type::Function {
                            function_info,
                            generics: new_generics,
                        } => {
                            let old_generics = &function_info.generics;
                            let new_generics = &new_generics;

                            if function_info.arguments.len() != function_call.arguments.len() {
                                let arguments_define_span_start = function_info
                                    .arguments
                                    .iter()
                                    .map(|argument| argument.span.start)
                                    .min()
                                    .unwrap_or(function_info.span.start);
                                let arguments_define_span_end = function_info
                                    .arguments
                                    .iter()
                                    .map(|argument| argument.span.end)
                                    .max()
                                    .unwrap_or(function_info.span.end);

                                let error = TypeError {
                                    kind: TypeErrorKind::FunctionArgumentCountMismatch {
                                        expected: function_info.arguments.len().moduled(
                                            function_info.module_path.clone(),
                                            arguments_define_span_start..arguments_define_span_end,
                                        ),
                                        found: function_call.arguments.len().moduled(
                                            module_path.clone(),
                                            function_call.span.clone(),
                                        ),
                                    },
                                    span: function_call.span.clone(),
                                    module_path: module_path.clone(),
                                };
                                errors.push(error);
                            }

                            for (origin_argument_type, argument_type_variable) in
                                function_info.arguments.iter().zip(function_arguments)
                            {
                                let argument_type = origin_argument_type
                                    .value
                                    .replace_generics(old_generics, new_generics);

                                let argument_type = argument_type.moduled(
                                    function_info.module_path.clone(),
                                    origin_argument_type.span.clone(),
                                );

                                type_environment.unify_type(
                                    argument_type,
                                    argument_type_variable,
                                    user_type_set,
                                    errors,
                                );
                            }

                            let return_type = function_info
                                .return_type
                                .value
                                .replace_generics(old_generics, new_generics);

                            type_environment
                                .create_type_variable_id_with_type(return_type.moduled(
                                    function_info.module_path.clone(),
                                    function_info.return_type.span.clone(),
                                ))
                                .with_span(function_call.span.clone())
                        }
                        _ => {
                            let error = TypeError {
                                kind: TypeErrorKind::NotFunctionType {
                                    not_function_type_from: ty
                                        .value
                                        .to_display_string(user_type_set, Some(type_environment))
                                        .with_span(second.literal.span.clone()),
                                },
                                span: function_call.span.clone(),
                                module_path: module_path.clone(),
                            };
                            errors.push(error);

                            type_environment
                                .create_type_variable_id_with_set()
                                .with_span(function_call.span.clone())
                        }
                    },
                    None => {
                        let error = TypeError {
                            kind: TypeErrorKind::UnknownTypeAtThisPoint,
                            span: function_call.span.clone(),
                            module_path: module_path.clone(),
                        };
                        errors.push(error);

                        type_environment
                            .create_type_variable_id_with_set()
                            .with_span(function_call.span.clone())
                    }
                }
            }
        }
    }

    last_type_variable_id.value
}

fn get_module_import_type(
    ast: &Primary,
    first_literal: &Literal,
    mut path_name: String,
    chain_start_position: &mut usize,
    type_environment: &mut TypeEnvironment,
    implements_element_checker: &ImplementsElementChecker,
    moduled_name_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
    package_resource_set: &PackageResourceSet,
    module_path: &ModulePath,
    errors: &mut std::vec::Vec<TypeError>,
) -> Spanned<TypeVariableID> {
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

        last_literal = &second.literal;
        path_name = new_path;
    }

    Spanned::new(
        type_environment.create_and_set_entity_type(
            last_literal.into(),
            Moduled::new(import_type, module_path.clone(), last_literal.span.clone()),
        ),
        last_literal.span.clone(),
    )
}
