use std::{mem, ops::DerefMut, sync::Arc};

use ariadne::Color;
use catla_parser::parser::{AddOrSubExpression, AndExpression, CompareExpression, EQNEExpression, Expression, ExpressionEnum, Factor, FunctionCall, FunctionDefine, GenericsDefine, MappingOperator, MappingOperatorKind, MemoryManageAttributeKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, Spanned, StatementAST, StatementAttributeKind, TypeAttributeEnum, TypeInfo, WhereClause};
use either::Either;
use fxhash::FxHashMap;

use crate::transpiler::{component::EntityID, context::TranspileModuleContext, error::SimpleError, name_resolver::{DefineKind, FoundDefineInfo}, TranspileError, TranspileWarning};

use super::type_info::{Bound, FreezableMutex, FunctionDefineInfo, FunctionType, GenericType, ImplementsInfo, ImplementsInfoSet, Type, WhereBound};


pub(crate) fn collect_module_element_types_program(
    ast: Program,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    current_user_type_name: Option<&str>,
    context: &TranspileModuleContext
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        if let Some(user_type_name) = &current_user_type_name {
            let element = match statement {
                StatementAST::VariableDefine(field_define) => {
                    let type_info = match &field_define.type_tag {
                        Some(type_tag) => {
                            match &type_tag.type_info {
                                Ok(type_info) => {
                                    get_type(
                                        type_info,
                                        user_type_map,
                                        import_element_map,
                                        name_resolved_map,
                                        module_user_type_map,
                                        module_element_type_map,
                                        generics_map,
                                        errors,
                                        warnings,
                                        context
                                    )
                                },
                                _ => Type::Unknown
                            }
                        },
                        _ => {
                            let error = SimpleError::new(
                                0028,
                                field_define.span.clone(),
                                vec![],
                                vec![(field_define.span.clone(), Color::Red)]
                            );
                            errors.push(error);
                            Type::Unknown
                        }
                    };

                    module_entity_type_map.insert(EntityID::from(field_define), type_info.clone());

                    match &field_define.name {
                        Ok(name) => Some((name.value, type_info)),
                        _ => None
                    }
                },
                StatementAST::FunctionDefine(method_define) => {
                    get_function_type_and_name(
                        method_define,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        module_entity_type_map,
                        implements_infos,
                        errors,
                        warnings,
                        Some(*user_type_name),
                        context
                    )
                },
                _ => None
            };

            if let Some(element) = element {
                let user_type = user_type_map.get(*user_type_name).unwrap().clone();

                if let Type::UserType{ user_type_info: data_struct_info, generics: _ } = user_type {
                    let mut element_map = data_struct_info.element_types.lock().unwrap();
                    element_map.insert(element.0.to_string(), element.1);
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
                    errors,
                    warnings,
                    context
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
                        errors,
                        warnings,
                        context
                    );
                }
            },
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
                    errors,
                    warnings,
                    context
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
                        errors,
                        warnings,
                        context
                    );
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                if current_user_type_name.is_none() {
                    let function_type = get_function_type_and_name(
                        function_define,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        module_entity_type_map,
                        implements_infos,
                        errors,
                        warnings,
                        None,
                        context
                    );

                    if let Some(function_type) = function_type {
                        module_element_type_map.insert(function_type.0.to_string(), function_type.1);
                    }
                }

                if let Some(block) = &function_define.block.value {
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
                        errors,
                        warnings,
                        None,
                        context
                    );
                }
            },
            StatementAST::UserTypeDefine(data_struct_define) => {
                if let Ok(name) = &data_struct_define.name {
                    if let Some(generics_define) = &data_struct_define.generics_define {
                        let generic_types = get_generic_type(
                            generics_define,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            errors,
                            warnings,
                            context
                        );
                        
                        // set generic bounds type
                        let user_type = user_type_map.get(name.value).unwrap().clone();
                        if let Type::UserType{ user_type_info, generics: _ } = user_type {
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

                            let where_bounds = get_where_bounds(
                                &data_struct_define.where_clause,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                generics_map,
                                errors,
                                warnings,
                                context
                            );
                            let mut where_bounds_lock = user_type_info.where_bounds.lock().unwrap();
                            *where_bounds_lock.as_mut().left().unwrap() = where_bounds;
                        }
                    }

                    if let Some(super_type_info) = &data_struct_define.super_type_info {
                        let concrete = user_type_map.get(name.value).unwrap().clone();
                        let generics = if let Type::UserType { user_type_info, generics: _ } = &concrete {
                            Arc::new(user_type_info.generics_define.clone())
                        } else {
                            Arc::new(Vec::new())
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
                                errors,
                                warnings,
                                context
                            );

                            let implements_info = ImplementsInfo {
                                generics: generics.clone(),
                                interface,
                                concrete: concrete.clone()
                            };
                            implements_infos.add(implements_info);
                        }
                    }

                    if let Some(block) = &data_struct_define.block.value {
                        collect_module_element_types_program(
                            block.program,
                            user_type_map,
                            import_element_map,
                            name_resolved_map, module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            module_entity_type_map,
                            implements_infos,
                            errors,
                            warnings,
                            Some(name.value),
                            context
                        );
                    }
                }
            },
            StatementAST::Implements(implements) => {
                if let Ok(interface) = &implements.interface {
                    if let Ok(target_type) = &implements.target_user_type {
                        let generics = implements.generics_define.as_ref().map(|define| {
                            get_generic_type(
                                &define,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                generics_map,
                                errors,
                                warnings,
                                context
                            )
                        }).unwrap_or_else(|| { vec![] });
                        let generics = Arc::new(generics);

                        let interface = get_type(
                            interface,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            errors,
                            warnings,
                            context
                        );

                        let concrete = get_type(
                            target_type,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            errors,
                            warnings,
                            context
                        );

                        let implements_info = ImplementsInfo {
                            generics,
                            interface,
                            concrete,
                        };
                        implements_infos.add(implements_info);
                    }
                }

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
                        errors,
                        warnings,
                        current_user_type_name,
                        context
                    );
                }
            },
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
                        errors,
                        warnings,
                        context
                    );
                }
            },
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
                    errors,
                    warnings,
                    context
                );
            },
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
                        Some(type_tag) => {
                            match &type_tag.type_info {
                                Ok(type_info) => {
                                    get_type(
                                        type_info,
                                        user_type_map,
                                        import_element_map,
                                        name_resolved_map,
                                        module_user_type_map,
                                        module_element_type_map,
                                        generics_map,
                                        errors,
                                        warnings,
                                        context
                                    )
                                },
                                _ => Type::Unknown
                            }
                        },
                        _ => {
                            let span = match &variable_define.name {
                                Ok(name) => name.span.clone(),
                                _ => variable_define.span.clone()
                            };
                            let error = SimpleError::new(
                                0029,
                                span.clone(),
                                vec![],
                                vec![(span, Color::Red)]
                            );
                            errors.push(error);

                            Type::Unknown
                        }
                    };

                    module_entity_type_map.insert(EntityID::from(variable_define), variable_type.clone());
                    
                    if let Ok(name) = &variable_define.name {
                        module_element_type_map.insert(name.value.to_string(), variable_type);
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
                            errors,
                            warnings,
                            context
                        );
                    }
                }
            }
            _ => {}
        }
    }
}

fn get_function_type_and_name<'allocator>(
    ast: &'allocator FunctionDefine,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    current_user_type_name: Option<&str>,
    context: &TranspileModuleContext
) -> Option<(&'allocator str, Type)> {
    let (generics_define, generics_define_span) = match &ast.generics_define {
        Some(generics_define) => {
            (
                get_generic_type(
                    generics_define,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    errors,
                    warnings,
                    context
                ),
                Some(generics_define.span.clone())
            )
        },
        _ => (Vec::new(), None)
    };
    
    let return_type = match &ast.type_tag {
        Some(type_tag) => {
            match &type_tag.type_info {
                Ok(type_info) => {
                    let ty = get_type(
                        type_info,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        errors,
                        warnings,
                        context
                    );
                    Spanned::new(ty, type_info.span.clone())
                },
                _ => Spanned::new(Type::Unknown, type_tag.span.clone())
            }
        },
        _ => Spanned::new(Type::Unit, ast.span.clone())
    };

    let mut argument_types = Vec::new();

    if let Some(user_type_name) = &current_user_type_name {
        argument_types.push(user_type_map.get(*user_type_name).unwrap().clone());
    }

    for argument in ast.args.arguments.iter() {
        let type_info = match &argument.type_tag.type_info {
            Ok(type_info) => {
                get_type(
                    type_info,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    errors,
                    warnings,
                    context
                )
            },
            _ => Type::Unknown
        };
        argument_types.push(type_info);
    }

    if let Some(block) = &ast.block.value {
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
            errors,
            warnings,
            current_user_type_name,
            context
        );
    }

    let define_info = FunctionDefineInfo {
        module_name: context.module_name.clone(),
        generics_define_span,
        arguments_span: ast.args.span.clone(),
        span: ast.span.clone()
    };
    
    let where_bounds = get_where_bounds(
        &ast.where_clause,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        generics_map,
        errors,
        warnings,
        context
    );

    let function_info = Arc::new(FunctionType {
        is_extension: current_user_type_name.is_some(),
        generics_define,
        argument_types,
        return_type,
        define_info,
        where_bounds: FreezableMutex::new(where_bounds)
    });

    let function_type = Type::Function{ function_info, generics: Arc::new(Vec::new()) };

    module_entity_type_map.insert(EntityID::from(ast), function_type.clone());

    match &ast.name {
        Ok(name) => {
            let name = match name {
                Either::Left(name) => name.value,
                Either::Right(attribute) => {
                    match attribute.value {
                        MemoryManageAttributeKind::New => "new",
                        MemoryManageAttributeKind::Drop => "drop",
                        MemoryManageAttributeKind::Mutex => "mutex",
                    }
                },
            };
            Some((name, function_type))
        },
        _ => None
    }
}

fn get_where_bounds(
    ast: &Option<WhereClause>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
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
                errors,
                warnings,
                context
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
                    errors,
                    warnings,
                    context
                );
                // TODO - check sanity of bound types
                bounds.push(Arc::new(Bound {
                    module_name: context.module_name.clone(),
                    span: bound.span.clone(),
                    ty
                }));
            }
            where_bounds.push(WhereBound { target_type, bounds });
        }
        where_bounds
    } else {
        Vec::new()
    }
}

fn collect_module_element_types_expression(
    ast: Expression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
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
                errors,
                warnings,
                context
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
                        errors,
                        warnings,
                        context
                    );
                }
            }
        },
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
                    errors,
                    warnings,
                    context
                );
            }
        },
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
                            errors,
                            warnings,
                            context
                        );
                    },
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
                            errors,
                            warnings,
                            None,
                            context
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
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    collect_module_element_types_eqne_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        generics_map,
        module_entity_type_map,
        implements_infos,
        errors,
        warnings,
        context
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_module_element_types_eqne_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                module_entity_type_map,
                implements_infos,
                errors,
                warnings,
                context
            );
        }
    }
}

fn collect_module_element_types_eqne_expression(
    ast: &EQNEExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
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
        errors,
        warnings,
        context
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
                errors,
                warnings,
                context
            );
        }
    }
}

fn collect_module_element_types_compare_expression(
    ast: &CompareExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
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
        errors,
        warnings,
        context
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
                errors,
                warnings,
                context
            );
        }
    }
}

fn collect_module_element_types_add_or_sub_expression(
    ast: &AddOrSubExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
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
        errors,
        warnings,
        context
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
                errors,
                warnings,
                context
            );
        }
    }
}

fn collect_module_element_types_mul_or_div_expression(
    ast: &MulOrDivExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
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
        errors,
        warnings,
        context
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
                errors,
                warnings,
                context
            );
        }
    }
}

fn collect_module_element_types_factor(
    ast: &Factor,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
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
            errors,
            warnings,
            context
        );
    }
}

fn collect_module_element_types_primary(
    ast: &Primary,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
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
        errors,
        warnings,
        context
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
            errors,
            warnings,
            context
        );
    }
}

fn collect_module_element_types_primary_left(
    ast: &PrimaryLeft,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expression { expression, error_tokens: _, span: _ } => {
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
                            errors,
                            warnings,
                            context
                        );
                    }
                },
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
                    errors,
                    warnings,
                    context
                );
            }
        },
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
                            errors,
                            warnings,
                            context
                        );
                    }
                }
            }
        },
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
                    errors,
                    warnings,
                    context
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
                    errors,
                    warnings,
                    None,
                    context
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
                                    errors,
                                    warnings,
                                    context
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
                                    errors,
                                    warnings,
                                    None,
                                    context
                                );
                            }
                        },
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
                                errors,
                                warnings,
                                None,
                                context
                            );
                        }
                    }
                }
            }
        },
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
                    errors,
                    warnings,
                    None,
                    context
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
            errors,
            warnings,
            context
        );
    }
}

fn collect_module_element_types_primary_right(
    ast: &PrimaryRight,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
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
                errors,
                warnings,
                context
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
            errors,
            warnings,
            context
        );
    }
}

fn collect_module_element_types_mapping_operator(
    ast: &MappingOperator,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    let block = match &ast.value {
        MappingOperatorKind::NullElvisBlock(block) => block,
        MappingOperatorKind::ResultElvisBlock(block) => block,
        _ => return
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
            errors,
            warnings,
            None,
            context
        );
    }
}

fn collect_module_element_types_function_call(
    ast: &FunctionCall,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &mut FxHashMap<EntityID, Type>,
    implements_infos: &mut ImplementsInfoSet,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
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
                errors,
                warnings,
                context
            );
        }
    }
}

pub(crate) fn get_type(
    ast: &TypeInfo,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) -> Type {
    if ast.path.is_empty() {
        return Type::Unknown;
    }

    let mut type_info = if let Some(module_name) = import_element_map.get(&EntityID::from(ast)) {
        let type_map = module_user_type_map.get(module_name).unwrap();
        match type_map.get(ast.path.last().unwrap().value) {
            Some(type_info) => type_info.clone(),
            _ => {
                let span = ast.path.last().unwrap().span.clone();
                let error = SimpleError::new(
                    0030,
                    span.clone(),
                    vec![module_name.clone(), ast.path.last().unwrap().value.to_string()],
                    vec![(span, Color::Red)]
                );
                errors.push(error);
                return Type::Unknown
            }
        }
    } else {
        match name_resolved_map.get(&EntityID::from(&ast.path[0])) {
            Some(resolved) => {
                let ty = match resolved.define_info.define_kind {
                    DefineKind::UserType => user_type_map.get(ast.path[0].value).unwrap().clone(),
                    DefineKind::Generics => {
                        Type::Generic(generics_map.get(&EntityID::from(resolved.define_info.entity_id)).unwrap().clone())
                    },
                    DefineKind::Import => {
                        let module_name = import_element_map.get(&EntityID::from(resolved.define_info.entity_id)).unwrap();
                        let user_type_map = module_user_type_map.get(module_name).unwrap();
                        match user_type_map.get(ast.path[0].value) {
                            Some(user_type) => user_type.clone(),
                            None => Type::Unknown
                        }
                    },
                    _ => Type::Unknown
                };
                
                if ty == Type::Unknown {
                    let text = resolved.define_info.define_kind.get_name(&context.context.localized_text);
                    let span = ast.path[0].span.clone();

                    let error = SimpleError::new(
                        0033,
                        span.clone(),
                        vec![text],
                        vec![(span, Color::Red), (resolved.define_info.span.clone(), Color::Yellow)]
                    );
                    errors.push(error);
                }

                ty
            },
            _ => {
                match ast.path[0].value {
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
                    _ => Type::Unknown
                }
            }
        }
    };

    if let Some(generics_ast) = &ast.generics {
        let mut generics_types = Vec::new();
        for element in generics_ast.elements.iter() {
            let ty = get_type(
                element,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                errors,
                warnings,
                context
            );
            generics_types.push(ty);
        }

        type_info = match &type_info {
            Type::UserType { user_type_info: data_struct_info, generics: _ } => {
                Type::UserType { user_type_info: data_struct_info.clone(), generics: Arc::new(generics_types) }
            },
            Type::Function { function_info, generics: _ } => {
                Type::Function { function_info: function_info.clone(), generics: Arc::new(generics_types) }
            },
            _ => {
                let span_0 = ast.path.last().unwrap().span.clone();
                let span_1 = generics_ast.span.clone();

                let error = SimpleError::new(
                    0031,
                    span_1.clone(),
                    vec![],
                    vec![(span_0, Color::Yellow), (span_1, Color::Red)]
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
                                errors,
                                warnings,
                                context
                            )
                        } else {
                            let span_0_start = ast.path.last().unwrap().span.start;
                            let span_0_end = attribute.span.end;
                            let span_0 = span_0_start..span_0_end;
                            let span_1 = error_type.span.clone();

                            let error = SimpleError::new(
                                0032,
                                span_1.clone(),
                                vec![1.to_string(), error_type.elements.len().to_string()],
                                vec![(span_0, Color::Yellow), (span_1, Color::Red)]
                            );
                            errors.push(error);
                            Type::Unknown
                        }
                    },
                    _ => Type::Unit // TODO - default error class object
                };
                Type::Result { value: Arc::new(type_info), error: Arc::new(error_type) }
            }
        };
    }

    type_info
}

fn get_generic_type<'allocator>(
    ast: &GenericsDefine,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) -> Vec<Arc<GenericType>> {
    let mut generics = Vec::new();

    for element in ast.elements.iter() {
        let entity_id = EntityID::from(element);
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
                errors,
                warnings,
                context
            );
            // TODO - check sanity of bound types
            bounds.push(Arc::new(Bound {
                module_name: context.module_name.clone(),
                span: bound.span.clone(),
                ty
            }));
        }

        let generic = Arc::new(GenericType {
            define_entity_id: entity_id,
            name: element.name.value.to_string(),
            bounds: FreezableMutex::new(bounds),
        });

        generics_map.insert(entity_id, generic.clone());

        generics.push(generic);
    }
    
    generics
}