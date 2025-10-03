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
use catla_parser::ast::{EntityID, Expression, Program, Spanned, Statement};
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
                let type_variable_id = self.create_type_variable_id();
                self.create_variable_set(type_variable_id);

                self.entity_var_map.insert(entity_id, type_variable_id);

                type_variable_id
            }
        }
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

    fn create_type_variable_id(&mut self) -> TypeVariableID {
        let old_id = self.type_variable_id_counter;
        self.type_variable_id_counter += 1;
        TypeVariableID(old_id)
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

    fn unify_entity_id(&mut self, entity_id_0: EntityID, entity_id_1: EntityID) {}

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
        errors: &mut Vec<TypeError>,
    ) {
        // take right members
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
        errors: &mut Vec<TypeError>,
    ) {
        match (&left.value, &right.value) {
            (Type::TypeVariable(left_variable_id), Type::TypeVariable(right_variable_id)) => {
                self.unify(*left_variable_id, *right_variable_id, user_type_set, errors);
            }
            (Type::TypeVariable(left_variable_id), _) => {
                let left_type = self.get_type_variable_type(*left_variable_id);

                match left_type {
                    Some(left_type) => {
                        // TODO : numeric type inference

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
                        // TODO : numeric type inference

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

    fn export_entity_type(&self) -> HashMap<EntityID, Spanned<Type>> {
        let mut entity_type_map = HashMap::new();

        for (entity_id, type_variable_id) in self.entity_var_map.iter() {
            let type_variable_set_id = self.var_set_map.get(type_variable_id).unwrap();
            let type_variable_set = &self.type_variable_set_components[type_variable_set_id.0];

            if let Some(ty) = &type_variable_set.ty {
                entity_type_map.insert(*entity_id, ty.clone());
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
        Self {
            members: HashSet::new_in(allocator),
            ty: None,
        }
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
    errors: &mut Vec<TypeError>,
) -> HashMap<EntityID, Spanned<Type>> {
    let allocator = Bump::new();
    let mut type_environment = TypeEnvironment::new(module_path, &allocator);

    infer_type_for_program(
        ast,
        &mut type_environment,
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

    type_environment.export_entity_type()
}

fn infer_type_for_program(
    ast: &Program,
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
    errors: &mut Vec<TypeError>,
) {
    for statement in ast.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => {
                infer_type_for_expression(
                    &assignment.left,
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
                }
            }
            Statement::Swap(swap_statement) => todo!(),
            Statement::Import(import_statement) => todo!(),
            Statement::DefineWithAttribute(define_with_attribute) => todo!(),
            Statement::Drop(drop_statement) => todo!(),
            Statement::Expression(expression) => todo!(),
            Statement::Implements(implements) => todo!(),
        }
    }
}

fn infer_type_for_expression(
    ast: &Expression,
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
    errors: &mut Vec<TypeError>,
) {
    match ast {
        Expression::Return(return_expression) => todo!(),
        Expression::Closure(closure) => todo!(),
        Expression::Or(or_expression) => todo!(),
    }
}
