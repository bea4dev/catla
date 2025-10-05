use std::{mem::swap, sync::Arc};

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
use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_import::ImportElement;
use catla_name_resolver::ResolvedInfo;
use catla_parser::ast::{
    AddOrSubExpression, AndExpression, Define, EntityID, EqualsExpression, Expression, Factor,
    LessOrGreaterExpression, MulOrDivExpression, OrExpression, Primary, PrimaryLeftExpr, Program,
    SimplePrimary, Spanned, Statement, VariableBinding,
};
use catla_util::module_path::ModulePath;
use hashbrown::{DefaultHashBuilder, HashMap, HashSet};

use crate::{
    error::{TypeError, TypeErrorKind},
    types::{GenericType, GlobalUserTypeID, GlobalUserTypeSet, ImplementsInfoSet, Type},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVariableID(usize);

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
    fn new(module_path: &ModulePath, allocator: &'type_env_alloc Bump) -> Self {
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
        ty: Spanned<Type>,
    ) -> TypeVariableID {
        let type_variable_id = self.get_or_allocate_variable_for_entity(entity_id);
        let type_variable_set_id = self.var_set_map.get(&type_variable_id).unwrap();
        let type_variable_set = &mut self.type_variable_set_components[type_variable_set_id.0];

        type_variable_set.ty = Some(ty);

        type_variable_id
    }

    fn get_entity_type(&self, entity_id: EntityID) -> Option<Spanned<Type>> {
        let type_variable_id = self.entity_var_map.get(&entity_id).unwrap();
        let type_variable_set_id = self.var_set_map.get(type_variable_id).unwrap();
        let type_variable_set = &self.type_variable_set_components[type_variable_set_id.0];

        type_variable_set.ty.as_ref().cloned()
    }

    pub(crate) fn get_type_variable_type(
        &self,
        type_variable_id: TypeVariableID,
    ) -> Option<Spanned<Type>> {
        let type_variable_set_id = self.var_set_map.get(&type_variable_id).unwrap();
        let type_variable_set = &self.type_variable_set_components[type_variable_set_id.0];

        type_variable_set.ty.as_ref().cloned()
    }

    fn create_type_variable_id_without_set(&mut self) -> TypeVariableID {
        let old_id = self.type_variable_id_counter;
        self.type_variable_id_counter += 1;
        TypeVariableID(old_id)
    }

    fn create_type_variable_id_with_set(&mut self) -> TypeVariableID {
        let type_variable_id = self.create_type_variable_id_without_set();
        self.create_variable_set(type_variable_id);
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
    fn unify(
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

    fn unify_type(
        &mut self,
        left: Spanned<Type>,
        right: Spanned<Type>,
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
                    Spanned::new(
                        left.value.resolve_type_alias(user_type_set),
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
                    Spanned::new(
                        right.value.resolve_type_alias(user_type_set),
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
                                    Spanned::new(left_generic.clone(), left.span.clone()),
                                    Spanned::new(right_generic.clone(), right.span.clone()),
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
                                    Spanned::new(left_generic.clone(), left.span.clone()),
                                    Spanned::new(right_generic.clone(), right.span.clone()),
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
                        Spanned::new(left_base_type.as_ref().clone(), left.span.clone()),
                        Spanned::new(right_base_type.as_ref().clone(), right.span.clone()),
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
                                Spanned::new(left_item.clone(), left.span.clone()),
                                Spanned::new(right_item.clone(), right.span.clone()),
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
            _ => left == right,
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

    fn resolve_type(&self, ty: &Type, user_type_set: &GlobalUserTypeSet) -> Type {
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

                Type::Function {
                    function_info: function_info.clone(),
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
    ty: Option<Spanned<Type>>,
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
    moduled_name_user_type_map: &HashMap<String, HashMap<String, GlobalUserTypeID>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut std::vec::Vec<TypeError>,
) -> HashMap<EntityID, Spanned<Type>> {
    let allocator = Bump::new();
    let mut type_environment = TypeEnvironment::new(module_path, &allocator);

    infer_type_for_program(
        ast,
        false,
        &mut type_environment,
        &Spanned::new(Type::Unit, ast.span.clone()),
        &None,
        generics,
        implements_infos,
        import_map,
        entity_user_type_map,
        moduled_name_user_type_map,
        name_resolved_map,
        user_type_set,
        module_path,
        errors,
    );

    type_environment.export_entity_type(user_type_set)
}

fn infer_type_for_program(
    ast: &Program,
    as_expression: bool,
    type_environment: &mut TypeEnvironment,
    return_type: &Spanned<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_user_type_map: &HashMap<String, HashMap<String, GlobalUserTypeID>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
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
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
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
                        moduled_name_user_type_map,
                        name_resolved_map,
                        user_type_set,
                        module_path,
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
                                Ok(binding) => {
                                    infer_type_for_variable_binding(binding, type_environment)
                                }
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
                                    moduled_name_user_type_map,
                                    name_resolved_map,
                                    user_type_set,
                                    module_path,
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
) -> TypeVariableID {
    match ast {
        VariableBinding::Literal(literal) => {
            type_environment.get_or_allocate_variable_for_entity(EntityID::from(literal))
        }
        VariableBinding::Binding { bindings, span } => {
            let tuple_items = bindings
                .iter()
                .map(|binding| infer_type_for_variable_binding(binding, type_environment))
                .map(|id| Type::TypeVariable(id))
                .collect();
            let tuple_type = Type::Tuple(Arc::new(tuple_items));

            type_environment.create_and_set_entity_type(
                EntityID::from(ast),
                Spanned::new(tuple_type, span.clone()),
            )
        }
    }
}

fn infer_type_for_expression(
    ast: &Expression,
    as_expression: bool,
    type_environment: &mut TypeEnvironment,
    return_type: &Spanned<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_user_type_map: &HashMap<String, HashMap<String, GlobalUserTypeID>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
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
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                ),
                None => type_environment.create_and_set_entity_type(
                    EntityID::from(ast),
                    Spanned::new(Type::Unit, return_expression.span.clone()),
                ),
            };

            let return_expression_type = Type::TypeVariable(type_variable_id);

            type_environment.unify_type(
                return_type.clone(),
                Spanned::new(return_expression_type, return_expression.span.clone()),
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
            moduled_name_user_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            errors,
        ),
    }
}

fn infer_type_for_or_expression(
    ast: &OrExpression,
    as_expression: bool,
    type_environment: &mut TypeEnvironment,
    return_type: &Spanned<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_user_type_map: &HashMap<String, HashMap<String, GlobalUserTypeID>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
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
            moduled_name_user_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
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
    return_type: &Spanned<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_user_type_map: &HashMap<String, HashMap<String, GlobalUserTypeID>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
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
            moduled_name_user_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
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
    return_type: &Spanned<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_user_type_map: &HashMap<String, HashMap<String, GlobalUserTypeID>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
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
            moduled_name_user_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
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
    return_type: &Spanned<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_user_type_map: &HashMap<String, HashMap<String, GlobalUserTypeID>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
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
            moduled_name_user_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
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
    return_type: &Spanned<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_user_type_map: &HashMap<String, HashMap<String, GlobalUserTypeID>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
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
            moduled_name_user_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
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
    return_type: &Spanned<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_user_type_map: &HashMap<String, HashMap<String, GlobalUserTypeID>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
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
            moduled_name_user_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
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
    return_type: &Spanned<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_user_type_map: &HashMap<String, HashMap<String, GlobalUserTypeID>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
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
                moduled_name_user_type_map,
                name_resolved_map,
                user_type_set,
                module_path,
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
    return_type: &Spanned<Type>,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    implements_infos: &ImplementsInfoSet,
    import_map: &HashMap<EntityID, ImportElement>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    moduled_name_user_type_map: &HashMap<String, HashMap<String, GlobalUserTypeID>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut std::vec::Vec<TypeError>,
) -> TypeVariableID {
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
                    Some(resolved) => type_environment
                        .get_or_allocate_variable_for_entity(resolved.define.entity_id),
                    None => todo!(),
                }
            }
            SimplePrimary::StringLiteral(literal) => todo!(),
            SimplePrimary::NumericLiteral(literal) => type_environment.create_and_set_entity_type(
                EntityID::from(literal),
                Spanned::new(Type::IntegerLiteral, literal.span.clone()),
            ),
            SimplePrimary::Null(range) => todo!(),
            SimplePrimary::True(range) => todo!(),
            SimplePrimary::False(range) => todo!(),
            SimplePrimary::This(range) => todo!(),
            SimplePrimary::LargeThis(range) => todo!(),
        },
        PrimaryLeftExpr::NewObject { new_object } => todo!(),
        PrimaryLeftExpr::NewArray { new_array } => todo!(),
        PrimaryLeftExpr::NewArrayInit { new_array_init } => todo!(),
        PrimaryLeftExpr::If { if_expression } => todo!(),
        PrimaryLeftExpr::Loop { loop_expression } => todo!(),
    };

    first_type_variable_id
}
