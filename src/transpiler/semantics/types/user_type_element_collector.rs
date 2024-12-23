use std::{
    mem,
    ops::DerefMut,
    sync::{Arc, RwLock},
};

use allocator_api2::vec;
use allocator_api2::vec::Vec;
use ariadne::Color;
use catla_parser::parser::{
    AddOrSubExpression, AndExpression, ArrayTypeInfo, BaseTypeInfo, CompareExpression, Expression,
    ExpressionEnum, Factor, FunctionCall, FunctionDefine, GenericsDefine, MappingOperator,
    MappingOperatorKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight,
    Program, SimplePrimary, Spanned, StatementAST, StatementAttributeKind, StatementAttributes,
    TupleTypeInfo, TypeAttributeEnum, TypeInfo, UserTypeKindEnum, VariableBinding, WhereClause,
};
use either::Either;
use fxhash::FxHashMap;

use crate::transpiler::{
    component::EntityID,
    context::TranspileModuleContext,
    error::SimpleError,
    name_resolver::{DefineKind, FoundDefineInfo},
    TranspileError, TranspileWarning,
};

use super::type_info::{
    Bound, FreezableMutex, FunctionDefineInfo, FunctionType, GenericType, ImplementsInfo,
    ImplementsInfoSet, ScopeThisType, Type, WhereBound, WithDefineInfo,
};

pub(crate) fn collect_module_element_types_program(
    ast: Program,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    mut owner_info: Option<(&Type, Option<&mut FxHashMap<String, WithDefineInfo<Type>>>)>,
    context: &TranspileModuleContext,
) {
    for statement in ast.statements.iter() {
        if let Ok(statement) = statement {
            if let StatementAST::TypeDefine(type_define) = statement {
                let user_type = if let Ok(name) = &type_define.name {
                    user_type_map.get(name.value).unwrap().clone()
                } else {
                    Type::Unknown
                };

                if let Ok(name) = &type_define.name {
                    if let Some(generics_define) = &type_define.generics_define {
                        let generic_types = get_generic_type(
                            generics_define,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            &ScopeThisType::new(Type::Unknown),
                            errors,
                            warnings,
                            context,
                        );
                        set_generics_bounds(&user_type, generic_types);
                    }

                    if let Some((_, implementation_element_map)) = owner_info.as_mut() {
                        if let Some(implementation_element_map) = implementation_element_map {
                            implementation_element_map.insert(
                                name.value.to_string(),
                                WithDefineInfo {
                                    value: user_type,
                                    module_name: context.module_name.clone(),
                                    span: name.span.clone(),
                                },
                            );
                        } else if let Type::UserType {
                            user_type_info,
                            generics: _,
                            generics_span: _,
                        } = &user_type
                        {
                            let mut element_types = user_type_info.element_types.lock().unwrap();
                            element_types.insert(
                                name.value.to_string(),
                                WithDefineInfo {
                                    value: user_type.clone(),
                                    module_name: context.module_name.clone(),
                                    span: name.span.clone(),
                                },
                            );
                        }
                    }
                }

                let ty = if let Ok(type_info) = &type_define.type_info {
                    Spanned::new(
                        get_type(
                            type_info,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            &ScopeThisType::new(Type::Unknown),
                            errors,
                            warnings,
                            context,
                        ),
                        type_info.get_span(),
                    )
                } else {
                    Spanned::new(Type::Unknown, type_define.span.clone())
                };

                if let Ok(name) = &type_define.name {
                    let defined_type = user_type_map.get(name.value).unwrap();
                    if let Type::UserType {
                        user_type_info,
                        generics: _,
                        generics_span: _,
                    } = defined_type
                    {
                        let mut element_types = user_type_info.element_types.lock().unwrap();
                        element_types.insert(
                            "type".to_string(),
                            WithDefineInfo {
                                value: ty.value,
                                module_name: context.module_name.clone(),
                                span: ty.span,
                            },
                        );
                    }
                }
            }
        }
    }

    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue,
        };

        if let Some((user_type, implementation_element_map)) = owner_info.as_mut() {
            let element = match statement {
                StatementAST::VariableDefine(field_define) => {
                    let type_info = match &field_define.type_tag {
                        Some(type_tag) => match &type_tag.type_info {
                            Ok(type_info) => get_type(
                                type_info,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                generics_map,
                                current_scope_this_type,
                                errors,
                                warnings,
                                context,
                            ),
                            _ => Type::Unknown,
                        },
                        _ => {
                            let error = SimpleError::new(
                                0028,
                                field_define.span.clone(),
                                vec![],
                                vec![(
                                    (context.module_name.clone(), field_define.span.clone()),
                                    Color::Red,
                                )],
                            );
                            errors.push(error);
                            Type::Unknown
                        }
                    };

                    module_entity_type_map.insert(EntityID::from(field_define), type_info.clone());

                    match &field_define.binding {
                        Ok(binding) => match &binding.binding {
                            Either::Left(literal) => Some((
                                literal.clone(),
                                type_info,
                                field_define
                                    .attributes
                                    .statement_attributes
                                    .iter()
                                    .cloned()
                                    .collect(),
                            )),
                            Either::Right(_) => {
                                let error = SimpleError::new(
                                    0080,
                                    binding.span.clone(),
                                    vec![],
                                    vec![(
                                        (context.module_name.clone(), binding.span.clone()),
                                        Color::Red,
                                    )],
                                );
                                errors.push(error);

                                None
                            }
                        },
                        _ => None,
                    }
                }
                StatementAST::FunctionDefine(method_define) => {
                    let name_and_type = get_function_type_and_name(
                        method_define,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        module_entity_type_map,
                        current_scope_this_type,
                        errors,
                        warnings,
                        Some(user_type),
                        context,
                    );

                    name_and_type.map(|(name, ty)| {
                        (name, ty, method_define.attributes.iter().cloned().collect())
                    })
                }
                _ => None,
            };

            if let Some((element_name, element_type, attributes)) = element {
                if let Some(implementation_element_map) = implementation_element_map {
                    implementation_element_map.insert(
                        element_name.value.to_string(),
                        WithDefineInfo {
                            value: element_type,
                            module_name: context.module_name.clone(),
                            span: element_name.span,
                        },
                    );
                } else if let Type::UserType {
                    user_type_info,
                    generics: _,
                    generics_span: _,
                } = user_type
                {
                    let mut element_map = user_type_info.element_types.lock().unwrap();
                    element_map.insert(
                        element_name.value.to_string(),
                        WithDefineInfo {
                            value: element_type,
                            module_name: context.module_name.clone(),
                            span: element_name.span,
                        },
                    );

                    let mut element_attributes = user_type_info.element_attributes.lock().unwrap();
                    element_attributes.insert(element_name.value.to_string(), attributes);
                }
            }
        }

        match statement {
            StatementAST::Assignment(assignment) => {
                collect_module_element_types_expression(
                    assignment.left_expr,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    module_entity_type_map,
                    implements_infos,
                    current_scope_this_type,
                    errors,
                    warnings,
                    context,
                );
                if let Ok(right_expr) = &assignment.right_expr {
                    collect_module_element_types_expression(
                        &right_expr,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        module_entity_type_map,
                        implements_infos,
                        current_scope_this_type,
                        errors,
                        warnings,
                        context,
                    );
                }
            }
            StatementAST::Exchange(exchange) => {
                collect_module_element_types_expression(
                    exchange.left_expr,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    module_entity_type_map,
                    implements_infos,
                    current_scope_this_type,
                    errors,
                    warnings,
                    context,
                );
                if let Ok(right_expr) = &exchange.right_expr {
                    collect_module_element_types_expression(
                        &right_expr,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        module_entity_type_map,
                        implements_infos,
                        current_scope_this_type,
                        errors,
                        warnings,
                        context,
                    );
                }
            }
            StatementAST::FunctionDefine(function_define) => {
                if owner_info.is_none() {
                    let function_type = get_function_type_and_name(
                        function_define,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        module_entity_type_map,
                        current_scope_this_type,
                        errors,
                        warnings,
                        None,
                        context,
                    );

                    if let Some(function_type) = function_type {
                        module_element_type_map
                            .insert(function_type.0.value.to_string(), function_type.1);
                    }
                }

                if let Some(semicolon_or_block) = &function_define.block_or_semicolon.value {
                    if let Either::Right(block) = semicolon_or_block {
                        collect_module_element_types_program(
                            block.program,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            module_entity_type_map,
                            implements_infos,
                            &current_scope_this_type.nest(),
                            errors,
                            warnings,
                            None,
                            context,
                        );
                    }
                }
            }
            StatementAST::UserTypeDefine(user_type_define) => {
                if let Ok(name) = &user_type_define.name {
                    let user_type = user_type_map.get(name.value).unwrap().clone();

                    let current_scope_this_type =
                        if user_type_define.kind.value == UserTypeKindEnum::Interface {
                            ScopeThisType::new(Type::This)
                        } else {
                            ScopeThisType::new(user_type.clone())
                        };

                    if let Some(generics_define) = &user_type_define.generics_define {
                        let generic_types = get_generic_type(
                            generics_define,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            &current_scope_this_type,
                            errors,
                            warnings,
                            context,
                        );

                        set_generics_bounds(&user_type, generic_types);
                    }

                    if let Type::UserType {
                        user_type_info,
                        generics: _,
                        generics_span: _,
                    } = &user_type
                    {
                        let where_bounds = get_where_bounds(
                            &user_type_define.where_clause,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            &current_scope_this_type,
                            errors,
                            warnings,
                            context,
                        );
                        let mut where_bounds_lock = user_type_info.where_bounds.lock().unwrap();
                        *where_bounds_lock.as_mut().left().unwrap() = where_bounds;
                    }

                    if let Some(super_type_info) = &user_type_define.super_type_info {
                        if user_type_define.kind.value == UserTypeKindEnum::Interface {
                            for type_info in super_type_info.type_infos.iter() {
                                let interface = get_type(
                                    type_info,
                                    user_type_map,
                                    import_element_map,
                                    name_resolved_map,
                                    module_user_type_map,
                                    module_element_type_map,
                                    generics_map,
                                    &current_scope_this_type,
                                    errors,
                                    warnings,
                                    context,
                                );
                                module_entity_type_map
                                    .insert(EntityID::from(type_info), interface.clone());

                                if let Type::UserType {
                                    user_type_info,
                                    generics: _,
                                    generics_span: _,
                                } = &user_type
                                {
                                    let mut super_type = user_type_info.super_type.lock().unwrap();
                                    super_type.as_mut().unwrap_left().push(interface);
                                }
                            }
                        } else {
                            let concrete = user_type_map
                                .get(name.value)
                                .unwrap()
                                .clone()
                                .init_generics();
                            let generics = if let Type::UserType {
                                user_type_info,
                                generics: _,
                                generics_span: _,
                            } = &concrete
                            {
                                Arc::new(user_type_info.generics_define.clone())
                            } else {
                                unreachable!()
                            };

                            for type_info in super_type_info.type_infos.iter() {
                                let interface = get_type(
                                    type_info,
                                    user_type_map,
                                    import_element_map,
                                    name_resolved_map,
                                    module_user_type_map,
                                    module_element_type_map,
                                    generics_map,
                                    &current_scope_this_type,
                                    errors,
                                    warnings,
                                    context,
                                );
                                module_entity_type_map
                                    .insert(EntityID::from(type_info), interface.clone());

                                let implements_info = ImplementsInfo {
                                    generics: generics.clone(),
                                    interface: Spanned::new(interface, type_info.get_span()),
                                    concrete: Spanned::new(concrete.clone(), name.span.clone()),
                                    module_name: context.module_name.clone(),
                                    where_bounds: Arc::new(Vec::new()),
                                    element_types: Arc::new(FxHashMap::default()),
                                    is_bounds_info: false,
                                };
                                implements_infos.insert(EntityID::from(type_info), implements_info);
                            }
                        }
                    }

                    if let Some(block) = &user_type_define.block.value {
                        let user_type = user_type_map.get(name.value).unwrap();

                        collect_module_element_types_program(
                            block.program,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            module_entity_type_map,
                            implements_infos,
                            &current_scope_this_type,
                            errors,
                            warnings,
                            Some((user_type, None)),
                            context,
                        );
                    }
                }
            }
            StatementAST::Implements(implements) => {
                let implements_info = if let Ok(interface_info) = &implements.interface {
                    if let Ok(target_type) = &implements.target_user_type {
                        let generics = implements
                            .generics_define
                            .as_ref()
                            .map(|define| {
                                get_generic_type(
                                    &define,
                                    user_type_map,
                                    import_element_map,
                                    name_resolved_map,
                                    module_user_type_map,
                                    module_element_type_map,
                                    generics_map,
                                    &ScopeThisType::new(Type::Unknown),
                                    errors,
                                    warnings,
                                    context,
                                )
                            })
                            .unwrap_or_else(|| vec![]);
                        let generics = Arc::new(generics);

                        let concrete = Spanned::new(
                            get_type(
                                target_type,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                generics_map,
                                &ScopeThisType::new(Type::Unknown),
                                errors,
                                warnings,
                                context,
                            ),
                            target_type.get_span(),
                        );
                        module_entity_type_map
                            .insert(EntityID::from(target_type), concrete.value.clone());

                        let current_scope_this_type = &ScopeThisType::new(concrete.value.clone());

                        let interface = Spanned::new(
                            get_type(
                                interface_info,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                generics_map,
                                current_scope_this_type,
                                errors,
                                warnings,
                                context,
                            ),
                            interface_info.get_span(),
                        );
                        module_entity_type_map
                            .insert(EntityID::from(interface_info), interface.value.clone());

                        let where_bounds = get_where_bounds(
                            &implements.where_clause,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            current_scope_this_type,
                            errors,
                            warnings,
                            context,
                        );

                        Some(ImplementsInfo {
                            generics,
                            interface,
                            concrete,
                            module_name: context.module_name.clone(),
                            where_bounds: Arc::new(where_bounds),
                            element_types: Arc::new(FxHashMap::default()),
                            is_bounds_info: false,
                        })
                    } else {
                        None
                    }
                } else {
                    None
                };

                if let Some(implements_info) = implements_info {
                    let current_scope_this_type =
                        &ScopeThisType::new(implements_info.concrete.value.clone());

                    let mut element_types = FxHashMap::default();
                    if let Some(block) = &implements.block.value {
                        collect_module_element_types_program(
                            block.program,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            module_entity_type_map,
                            implements_infos,
                            current_scope_this_type,
                            errors,
                            warnings,
                            Some((&implements_info.concrete.value, Some(&mut element_types))),
                            context,
                        );
                    }
                    let implements_info = ImplementsInfo {
                        generics: implements_info.generics,
                        interface: implements_info.interface,
                        concrete: implements_info.concrete,
                        module_name: implements_info.module_name,
                        where_bounds: implements_info.where_bounds,
                        element_types: Arc::new(element_types),
                        is_bounds_info: false,
                    };
                    implements_infos.insert(EntityID::from(implements), implements_info);
                } else {
                    if let Some(block) = &implements.block.value {
                        collect_module_element_types_program(
                            block.program,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            module_entity_type_map,
                            implements_infos,
                            &ScopeThisType::new(Type::This),
                            errors,
                            warnings,
                            None,
                            context,
                        );
                    }
                }
            }
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = drop_statement.expression {
                    collect_module_element_types_expression(
                        expression,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        module_entity_type_map,
                        implements_infos,
                        current_scope_this_type,
                        errors,
                        warnings,
                        context,
                    );
                }
            }
            StatementAST::Expression(expression) => {
                collect_module_element_types_expression(
                    &expression,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    module_entity_type_map,
                    implements_infos,
                    current_scope_this_type,
                    errors,
                    warnings,
                    context,
                );
            }
            StatementAST::VariableDefine(variable_define) => {
                let mut is_static = false;
                for attribute in variable_define.attributes.statement_attributes.iter() {
                    if attribute.value == StatementAttributeKind::Static {
                        is_static = true;
                        break;
                    }
                }

                if is_static {
                    let variable_type = match &variable_define.type_tag {
                        Some(type_tag) => match &type_tag.type_info {
                            Ok(type_info) => get_type(
                                type_info,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                generics_map,
                                current_scope_this_type,
                                errors,
                                warnings,
                                context,
                            ),
                            _ => Type::Unknown,
                        },
                        _ => {
                            let span = match &variable_define.binding {
                                Ok(name) => name.span.clone(),
                                _ => variable_define.span.clone(),
                            };
                            let error = SimpleError::new(
                                0029,
                                span.clone(),
                                vec![],
                                vec![((context.module_name.clone(), span), Color::Red)],
                            );
                            errors.push(error);

                            Type::Unknown
                        }
                    };

                    module_entity_type_map
                        .insert(EntityID::from(variable_define), variable_type.clone());

                    if let Ok(binding) = &variable_define.binding {
                        match &binding.binding {
                            Either::Left(literal) => {
                                module_element_type_map
                                    .insert(literal.value.to_string(), variable_type);
                            }
                            Either::Right(_) => {
                                let error = SimpleError::new(
                                    0080,
                                    binding.span.clone(),
                                    vec![],
                                    vec![(
                                        (context.module_name.clone(), binding.span.clone()),
                                        Color::Red,
                                    )],
                                );
                                errors.push(error);

                                insert_binding_unknown(binding, module_element_type_map);
                            }
                        }
                    }
                }

                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        collect_module_element_types_expression(
                            &expression,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            module_entity_type_map,
                            implements_infos,
                            current_scope_this_type,
                            errors,
                            warnings,
                            context,
                        );
                    }
                }
            }
            _ => {}
        }
    }
}

fn insert_binding_unknown(
    ast: &VariableBinding,
    module_element_type_map: &mut FxHashMap<String, Type>,
) {
    match &ast.binding {
        Either::Left(literal) => {
            module_element_type_map.insert(literal.value.to_string(), Type::Unknown);
        }
        Either::Right(bindings) => {
            for binding in bindings.iter() {
                insert_binding_unknown(binding, module_element_type_map);
            }
        }
    }
}

fn set_generics_bounds(user_type: &Type, generic_types: Vec<Arc<GenericType>>) {
    if let Type::UserType {
        user_type_info,
        generics: _,
        generics_span: _,
    } = user_type
    {
        let size = user_type_info.generics_define.len();
        if size == generic_types.len() {
            for i in 0..size {
                let generic_type_old = &user_type_info.generics_define[i];
                let generic_type_new = &generic_types[i];

                let mut bounds_old = generic_type_old.bounds.lock().unwrap();
                let bounds_new = generic_type_new.bounds.lock().unwrap();
                let mut bounds_new = bounds_new.clone();

                if generic_type_old.define_entity_id == generic_type_new.define_entity_id {
                    mem::swap(bounds_old.deref_mut(), &mut bounds_new);
                }
            }
        }
    }
}

fn get_function_type_and_name<'allocator>(
    ast: &'allocator FunctionDefine,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    current_user_type: Option<&Type>,
    context: &TranspileModuleContext,
) -> Option<(Spanned<&'allocator str>, Type)> {
    let (generics_define, generics_define_span) = match &ast.generics_define {
        Some(generics_define) => (
            get_generic_type(
                generics_define,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                current_scope_this_type,
                errors,
                warnings,
                context,
            ),
            Some(generics_define.span.clone()),
        ),
        _ => (Vec::new(), None),
    };

    let return_type = match &ast.type_tag {
        Some(type_tag) => match &type_tag.type_info {
            Ok(type_info) => {
                let ty = get_type(
                    type_info,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    current_scope_this_type,
                    errors,
                    warnings,
                    context,
                );
                Spanned::new(ty, type_info.get_span())
            }
            _ => Spanned::new(Type::Unknown, type_tag.span.clone()),
        },
        _ => Spanned::new(Type::Unit, ast.span.clone()),
    };

    let mut argument_types = Vec::new();

    if let Some(user_type) = current_user_type {
        if !ast.attributes.contains_kind(StatementAttributeKind::Static) {
            argument_types.push(user_type.clone());
        }
    }

    for argument in ast.args.arguments.iter() {
        let type_info = match &argument.type_tag.type_info {
            Ok(type_info) => get_type(
                type_info,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                current_scope_this_type,
                errors,
                warnings,
                context,
            ),
            _ => Type::Unknown,
        };
        argument_types.push(type_info);
    }

    let define_info = FunctionDefineInfo {
        module_name: context.module_name.clone(),
        entity_id: EntityID::from(ast),
        generics_define_span,
        arguments_span: ast.args.span.clone(),
        is_closure: false,
        origin_function: Arc::new(RwLock::new(None)),
        span: ast.span.clone(),
    };

    let where_bounds = get_where_bounds(
        &ast.where_clause,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        generics_map,
        current_scope_this_type,
        errors,
        warnings,
        context,
    );

    let function_info = Arc::new(FunctionType {
        is_extension: current_user_type.is_some()
            && !ast.attributes.contains_kind(StatementAttributeKind::Static),
        generics_define,
        argument_types,
        return_type,
        define_info,
        where_bounds: FreezableMutex::new(where_bounds),
    });

    let function_type = Type::Function {
        function_info,
        generics: Arc::new(Vec::new()),
    };

    module_entity_type_map.insert(EntityID::from(ast), function_type.clone());

    match &ast.name {
        Ok(name) => Some((name.clone(), function_type)),
        _ => None,
    }
}

pub(crate) fn get_where_bounds(
    ast: &Option<WhereClause>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) -> Vec<WhereBound> {
    if let Some(ast) = ast {
        let mut where_bounds = Vec::new();
        for element in ast.elements.iter() {
            let target_type = get_type(
                &element.target_type,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                current_scope_this_type,
                errors,
                warnings,
                context,
            );
            let mut bounds = Vec::new();
            for bound in element.bounds.iter() {
                let ty = get_type(
                    bound,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    current_scope_this_type,
                    errors,
                    warnings,
                    context,
                );
                // TODO - check sanity of bound types
                bounds.push(Arc::new(Bound {
                    module_name: context.module_name.clone(),
                    span: bound.get_span(),
                    ty,
                    entity_id: EntityID::from(bound),
                }));
            }
            where_bounds.push(WhereBound {
                target_type: Spanned::new(target_type, element.target_type.get_span()),
                bounds,
            });
        }
        where_bounds
    } else {
        Vec::new()
    }
}

fn collect_module_element_types_expression(
    ast: Expression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            collect_module_element_types_and_expression(
                &or_expression.left_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                module_entity_type_map,
                implements_infos,
                current_scope_this_type,
                errors,
                warnings,
                context,
            );
            for right_expr in or_expression.right_exprs.iter() {
                if let Ok(right_expr) = &right_expr.1 {
                    collect_module_element_types_and_expression(
                        right_expr,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        module_entity_type_map,
                        implements_infos,
                        current_scope_this_type,
                        errors,
                        warnings,
                        context,
                    );
                }
            }
        }
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                collect_module_element_types_expression(
                    expression,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    module_entity_type_map,
                    implements_infos,
                    current_scope_this_type,
                    errors,
                    warnings,
                    context,
                );
            }
        }
        ExpressionEnum::Closure(closure) => {
            if let Some(expression_or_block) = &closure.expression_or_block.value {
                match expression_or_block {
                    Either::Left(expression) => {
                        collect_module_element_types_expression(
                            &expression,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            module_entity_type_map,
                            implements_infos,
                            current_scope_this_type,
                            errors,
                            warnings,
                            context,
                        );
                    }
                    Either::Right(block) => {
                        collect_module_element_types_program(
                            block.program,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            module_entity_type_map,
                            implements_infos,
                            current_scope_this_type,
                            errors,
                            warnings,
                            None,
                            context,
                        );
                    }
                }
            }
        }
    }
}

fn collect_module_element_types_and_expression(
    ast: &AndExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_module_element_types_compare_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        generics_map,
        module_entity_type_map,
        implements_infos,
        current_scope_this_type,
        errors,
        warnings,
        context,
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_module_element_types_compare_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                module_entity_type_map,
                implements_infos,
                current_scope_this_type,
                errors,
                warnings,
                context,
            );
        }
    }
}

fn collect_module_element_types_compare_expression(
    ast: &CompareExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_module_element_types_add_or_sub_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        generics_map,
        module_entity_type_map,
        implements_infos,
        current_scope_this_type,
        errors,
        warnings,
        context,
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_module_element_types_add_or_sub_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                module_entity_type_map,
                implements_infos,
                current_scope_this_type,
                errors,
                warnings,
                context,
            );
        }
    }
}

fn collect_module_element_types_add_or_sub_expression(
    ast: &AddOrSubExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_module_element_types_mul_or_div_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        generics_map,
        module_entity_type_map,
        implements_infos,
        current_scope_this_type,
        errors,
        warnings,
        context,
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_module_element_types_mul_or_div_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                module_entity_type_map,
                implements_infos,
                current_scope_this_type,
                errors,
                warnings,
                context,
            );
        }
    }
}

fn collect_module_element_types_mul_or_div_expression(
    ast: &MulOrDivExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_module_element_types_factor(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        generics_map,
        module_entity_type_map,
        implements_infos,
        current_scope_this_type,
        errors,
        warnings,
        context,
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_module_element_types_factor(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                module_entity_type_map,
                implements_infos,
                current_scope_this_type,
                errors,
                warnings,
                context,
            );
        }
    }
}

fn collect_module_element_types_factor(
    ast: &Factor,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    if let Ok(primary) = &ast.primary {
        collect_module_element_types_primary(
            primary,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            generics_map,
            module_entity_type_map,
            implements_infos,
            current_scope_this_type,
            errors,
            warnings,
            context,
        );
    }
}

fn collect_module_element_types_primary(
    ast: &Primary,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_module_element_types_primary_left(
        &ast.left,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        generics_map,
        module_entity_type_map,
        implements_infos,
        current_scope_this_type,
        errors,
        warnings,
        context,
    );
    for primary_right in ast.chain.iter() {
        collect_module_element_types_primary_right(
            primary_right,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            generics_map,
            module_entity_type_map,
            implements_infos,
            current_scope_this_type,
            errors,
            warnings,
            context,
        );
    }
}

fn collect_module_element_types_primary_left(
    ast: &PrimaryLeft,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expressions {
                    expressions,
                    error_tokens: _,
                    span: _,
                } => {
                    for expression in expressions {
                        collect_module_element_types_expression(
                            &expression,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            module_entity_type_map,
                            implements_infos,
                            current_scope_this_type,
                            errors,
                            warnings,
                            context,
                        );
                    }
                }
                _ => {}
            }

            if let Some(function_call) = &simple.2 {
                collect_module_element_types_function_call(
                    function_call,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    module_entity_type_map,
                    implements_infos,
                    current_scope_this_type,
                    errors,
                    warnings,
                    context,
                );
            }
        }
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => {
            if let Ok(init_expression) = new_array_init_expression.init_expression {
                collect_module_element_types_expression(
                    init_expression,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    module_entity_type_map,
                    implements_infos,
                    current_scope_this_type,
                    errors,
                    warnings,
                    context,
                );
            }
            if let Ok(length_expression) = new_array_init_expression.length_expression {
                collect_module_element_types_expression(
                    length_expression,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    module_entity_type_map,
                    implements_infos,
                    current_scope_this_type,
                    errors,
                    warnings,
                    context,
                );
            }
        }
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => {
            for value_expression in new_array_expression.value_expressions.iter() {
                if let Ok(value_expression) = value_expression {
                    collect_module_element_types_expression(
                        value_expression,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        module_entity_type_map,
                        implements_infos,
                        current_scope_this_type,
                        errors,
                        warnings,
                        context,
                    );
                }
            }
        }
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Ok(field_assigns) = &new_expression.field_assigns {
                for field_assign in field_assigns.iter() {
                    if let Ok(expression) = &field_assign.expression {
                        collect_module_element_types_expression(
                            &expression,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            module_entity_type_map,
                            implements_infos,
                            current_scope_this_type,
                            errors,
                            warnings,
                            context,
                        );
                    }
                }
            }
        }
        PrimaryLeftExpr::IfExpression(if_expression) => {
            let first_statement = &if_expression.if_statement;
            if let Ok(condition) = &first_statement.condition {
                collect_module_element_types_expression(
                    &condition,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    module_entity_type_map,
                    implements_infos,
                    current_scope_this_type,
                    errors,
                    warnings,
                    context,
                );
            }
            if let Some(block) = &first_statement.block.value {
                collect_module_element_types_program(
                    block.program,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    module_entity_type_map,
                    implements_infos,
                    current_scope_this_type,
                    errors,
                    warnings,
                    None,
                    context,
                );
            }

            for else_if_or_block in if_expression.chain.iter() {
                if let Some(else_if_or_block) = &else_if_or_block.else_if_or_else.value {
                    match else_if_or_block {
                        Either::Left(if_statement) => {
                            if let Ok(condition) = &if_statement.condition {
                                collect_module_element_types_expression(
                                    &condition,
                                    user_type_map,
                                    import_element_map,
                                    name_resolved_map,
                                    module_user_type_map,
                                    module_element_type_map,
                                    generics_map,
                                    module_entity_type_map,
                                    implements_infos,
                                    current_scope_this_type,
                                    errors,
                                    warnings,
                                    context,
                                );
                            }
                            if let Some(block) = &if_statement.block.value {
                                collect_module_element_types_program(
                                    block.program,
                                    user_type_map,
                                    import_element_map,
                                    name_resolved_map,
                                    module_user_type_map,
                                    module_element_type_map,
                                    generics_map,
                                    module_entity_type_map,
                                    implements_infos,
                                    current_scope_this_type,
                                    errors,
                                    warnings,
                                    None,
                                    context,
                                );
                            }
                        }
                        Either::Right(block) => {
                            collect_module_element_types_program(
                                block.program,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                generics_map,
                                module_entity_type_map,
                                implements_infos,
                                current_scope_this_type,
                                errors,
                                warnings,
                                None,
                                context,
                            );
                        }
                    }
                }
            }
        }
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                collect_module_element_types_program(
                    block.program,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    module_entity_type_map,
                    implements_infos,
                    current_scope_this_type,
                    errors,
                    warnings,
                    None,
                    context,
                );
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_module_element_types_mapping_operator(
            mapping_operator,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            generics_map,
            module_entity_type_map,
            implements_infos,
            current_scope_this_type,
            errors,
            warnings,
            context,
        );
    }
}

fn collect_module_element_types_primary_right(
    ast: &PrimaryRight,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    if let Some(second_expr) = &ast.second_expr {
        if let Some(function_call) = &second_expr.2 {
            collect_module_element_types_function_call(
                function_call,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                module_entity_type_map,
                implements_infos,
                current_scope_this_type,
                errors,
                warnings,
                context,
            );
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_module_element_types_mapping_operator(
            mapping_operator,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            generics_map,
            module_entity_type_map,
            implements_infos,
            current_scope_this_type,
            errors,
            warnings,
            context,
        );
    }
}

fn collect_module_element_types_mapping_operator(
    ast: &MappingOperator,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    let block = match &ast.value {
        MappingOperatorKind::NullElvisBlock(block) => block,
        MappingOperatorKind::ResultElvisBlock(block) => block,
        _ => return,
    };

    if let Some(block) = &block.value {
        collect_module_element_types_program(
            block.program,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            generics_map,
            module_entity_type_map,
            implements_infos,
            current_scope_this_type,
            errors,
            warnings,
            None,
            context,
        );
    }
}

fn collect_module_element_types_function_call(
    ast: &FunctionCall,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    if let Ok(arg_exprs) = &ast.arg_exprs {
        for arg_expr in arg_exprs.iter() {
            collect_module_element_types_expression(
                &arg_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                module_entity_type_map,
                implements_infos,
                current_scope_this_type,
                errors,
                warnings,
                context,
            );
        }
    }
}

pub(crate) fn get_type(
    ast: &TypeInfo,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) -> Type {
    match ast {
        TypeInfo::BaseType(base_type_info) => get_base_type(
            base_type_info,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            generics_map,
            current_scope_this_type,
            errors,
            warnings,
            context,
        ),
        TypeInfo::ArrayType(array_type_info) => get_array_type(
            array_type_info,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            generics_map,
            current_scope_this_type,
            errors,
            warnings,
            context,
        ),
        TypeInfo::TupleType(tuple_type_info) => get_tuple_type(
            tuple_type_info,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            generics_map,
            current_scope_this_type,
            errors,
            warnings,
            context,
        ),
    }
}

pub(crate) fn get_tuple_type(
    ast: &TupleTypeInfo,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) -> Type {
    let mut types = Vec::new();

    for type_info in ast.types.iter() {
        let ty = get_type(
            type_info,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            generics_map,
            current_scope_this_type,
            errors,
            warnings,
            context,
        );
        types.push(ty);
    }

    Type::Tuple(Arc::new(types))
}

pub(crate) fn get_array_type(
    ast: &ArrayTypeInfo,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) -> Type {
    if let Ok(base_type_info) = ast.type_info {
        Type::Array(Arc::new(get_type(
            base_type_info,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            generics_map,
            current_scope_this_type,
            errors,
            warnings,
            context,
        )))
    } else {
        Type::Unknown
    }
}

pub(crate) fn get_base_type(
    ast: &BaseTypeInfo,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) -> Type {
    if ast.path.is_empty() {
        return Type::Unknown;
    }

    let mut type_info = if let Some(module_name) = import_element_map.get(&EntityID::from(ast)) {
        let type_map = module_user_type_map.get(&module_name.value).unwrap();
        match type_map.get(ast.path.last().unwrap().value) {
            Some(type_info) => type_info.clone(),
            _ => {
                let span = ast.path.last().unwrap().span.clone();
                let error = SimpleError::new(
                    0030,
                    span.clone(),
                    vec![
                        (module_name.value.clone(), Color::Yellow),
                        (ast.path.last().unwrap().value.to_string(), Color::Red),
                    ],
                    vec![((context.module_name.clone(), span), Color::Red)],
                );
                errors.push(error);
                return Type::Unknown;
            }
        }
    } else {
        match name_resolved_map.get(&EntityID::from(&ast.path[0])) {
            Some(resolved) => {
                let ty = match resolved.define_info.define_kind {
                    DefineKind::UserType => user_type_map.get(ast.path[0].value).unwrap().clone(),
                    DefineKind::Generics => Type::Generic(
                        generics_map
                            .get(&resolved.define_info.entity_id)
                            .unwrap()
                            .clone(),
                    ),
                    DefineKind::Import => {
                        match import_element_map.get(&resolved.define_info.entity_id) {
                            Some(module_name) => {
                                let user_type_map =
                                    module_user_type_map.get(&module_name.value).unwrap();
                                match user_type_map.get(ast.path[0].value) {
                                    Some(user_type) => user_type.clone(),
                                    None => Type::Unknown,
                                }
                            }
                            None => Type::Unknown,
                        }
                    }
                    _ => Type::Unknown,
                };

                if ty == Type::Unknown {
                    let text = resolved
                        .define_info
                        .define_kind
                        .get_name(&context.context.localized_text);
                    let span = ast.path[0].span.clone();

                    let error = SimpleError::new(
                        0033,
                        span.clone(),
                        vec![(text, Color::Red)],
                        vec![
                            ((context.module_name.clone(), span), Color::Red),
                            (
                                (
                                    context.module_name.clone(),
                                    resolved.define_info.span.clone(),
                                ),
                                Color::Yellow,
                            ),
                        ],
                    );
                    errors.push(error);
                }

                ty
            }
            _ => {
                if let Some(auto_import_module_name) = context
                    .context
                    .auto_import
                    .auto_import_elements
                    .get(ast.path[0].value)
                {
                    module_user_type_map
                        .get(auto_import_module_name)
                        .expect(
                            format!("Not found auto import module : {}", auto_import_module_name)
                                .as_str(),
                        )
                        .get(ast.path[0].value)
                        .expect(
                            format!("Not found auto import element : {}", ast.path[0].value)
                                .as_str(),
                        )
                        .clone()
                } else {
                    parse_primitive_type(ast.path[0].value, current_scope_this_type)
                }
            }
        }
    };

    if let Some(generics_ast) = &ast.generics {
        let mut generics_types = Vec::new();
        let mut generics_span = Vec::new();
        for element in generics_ast.elements.iter() {
            let ty = get_type(
                element,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                current_scope_this_type,
                errors,
                warnings,
                context,
            );
            generics_types.push(ty);
            generics_span.push(element.get_span());
        }

        type_info = match &type_info {
            Type::UserType {
                user_type_info: data_struct_info,
                generics: _,
                generics_span: _,
            } => Type::UserType {
                user_type_info: data_struct_info.clone(),
                generics: Arc::new(generics_types),
                generics_span: Some(Arc::new(generics_span)),
            },
            Type::Function {
                function_info,
                generics: _,
            } => Type::Function {
                function_info: function_info.clone(),
                generics: Arc::new(generics_types),
            },
            _ => {
                let span_0 = ast.path.last().unwrap().span.clone();
                let span_1 = generics_ast.span.clone();

                let error = SimpleError::new(
                    0031,
                    span_1.clone(),
                    vec![],
                    vec![
                        ((context.module_name.clone(), span_0), Color::Yellow),
                        ((context.module_name.clone(), span_1), Color::Red),
                    ],
                );
                errors.push(error);

                type_info.clone()
            }
        };
    }

    for attribute in ast.type_attributes.iter() {
        type_info = match &attribute.value {
            TypeAttributeEnum::Optional => Type::Option(Arc::new(type_info)),
            TypeAttributeEnum::Result(error_type) => {
                let error_type = match error_type {
                    Some(error_type) => {
                        if error_type.elements.len() == 1 {
                            get_type(
                                &error_type.elements[0],
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                generics_map,
                                current_scope_this_type,
                                errors,
                                warnings,
                                context,
                            )
                        } else {
                            let span_0_start = ast.path.last().unwrap().span.start;
                            let span_0_end = attribute.span.end;
                            let span_0 = span_0_start..span_0_end;
                            let span_1 = error_type.span.clone();

                            let error = SimpleError::new(
                                0032,
                                span_1.clone(),
                                vec![
                                    (1.to_string(), Color::Yellow),
                                    (error_type.elements.len().to_string(), Color::Red),
                                ],
                                vec![
                                    ((context.module_name.clone(), span_0), Color::Yellow),
                                    ((context.module_name.clone(), span_1), Color::Red),
                                ],
                            );
                            errors.push(error);
                            Type::Unknown
                        }
                    }
                    _ => Type::Unit, // TODO - default error class object
                };
                Type::Result {
                    value: Arc::new(type_info),
                    error: Arc::new(error_type),
                }
            }
        };
    }

    // dereference renamed type
    if type_info.is_renamed_type() {
        type_info
            .get_element_type_with_replaced_generic("type")
            .unwrap()
            .value
    } else {
        type_info
    }
}

pub(crate) fn parse_primitive_type(str: &str, current_scope_this_type: &ScopeThisType) -> Type {
    match str {
        "int" => Type::Int32,
        "int8" => Type::Int8,
        "int16" => Type::Int16,
        "int32" => Type::Int32,
        "int64" => Type::Int64,
        "uint" => Type::Uint32,
        "uint8" => Type::Uint8,
        "uint16" => Type::Uint16,
        "uint32" => Type::Uint32,
        "uint64" => Type::Uint64,
        "float" => Type::Float32,
        "float32" => Type::Float32,
        "float64" => Type::Float64,
        "bool" => Type::Bool,
        "unit" => Type::Unit,
        "This" => current_scope_this_type.ty.clone(),
        _ => Type::Unknown,
    }
}

fn get_generic_type<'allocator>(
    ast: &GenericsDefine,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    current_scope_this_type: &ScopeThisType,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) -> Vec<Arc<GenericType>> {
    let mut generics = Vec::new();

    for element in ast.elements.iter() {
        let entity_id = EntityID::from(element);

        let generic = Arc::new(GenericType {
            define_entity_id: entity_id,
            name: Arc::new(element.name.value.to_string()),
            bounds: FreezableMutex::new(Vec::new()),
            location: WithDefineInfo {
                value: (),
                module_name: context.module_name.clone(),
                span: element.span.clone(),
            },
        });

        generics_map.insert(entity_id, generic.clone());

        let mut bounds = Vec::new();

        for bound in element.bounds.iter() {
            let ty = get_type(
                bound,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                current_scope_this_type,
                errors,
                warnings,
                context,
            );
            // TODO - check sanity of bound types
            bounds.push(Arc::new(Bound {
                module_name: context.module_name.clone(),
                span: bound.get_span(),
                ty,
                entity_id: EntityID::from(bound),
            }));
        }

        {
            let mut bounds_ref = generic.bounds.lock().unwrap();
            *bounds_ref.as_mut().left().unwrap() = bounds;
        }

        generics.push(generic);
    }

    generics
}
