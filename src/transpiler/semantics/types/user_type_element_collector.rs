use std::{collections::HashMap, mem, ops::DerefMut, sync::{Arc, Mutex}};

use catla_parser::parser::{AddOrSubExpression, AndExpression, CompareExpression, EQNEExpression, Expression, ExpressionEnum, Factor, FunctionCall, FunctionDefine, GenericsDefine, GenericsElement, MappingOperator, MappingOperatorKind, MemoryManageAttributeKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, Spanned, StatementAST, StatementAttributeKind, TypeInfo};
use either::Either;
use fxhash::FxHashMap;

use crate::transpiler::{component::EntityID, context::TranspileModuleContext, name_resolver::FoundDefineInfo};

use super::type_info::{self, DataStructInfo, FunctionType, GenericType, Type};


pub(crate) fn collect_module_element_types_program(
    ast: Program,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
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
                                        context
                                    )
                                },
                                _ => Type::Unknown
                            }
                        },
                        _ => {
                            // TODO - error
                            Type::Unknown
                        }
                    };

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
                        Some(*user_type_name),
                        context
                    )
                },
                _ => None
            };

            if let Some(element) = element {
                let user_type = user_type_map.get(*user_type_name).unwrap().clone();

                if let Type::DataStruct(user_type) = user_type {
                    let mut element_map = user_type.element_types.lock().unwrap();
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
                        current_user_type_name,
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
                        None,
                        context
                    );
                }
            },
            StatementAST::DataStructDefine(data_struct_define) => {
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
                            current_user_type_name,
                            context
                        );
                        
                        // set generic bounds type
                        let user_type = user_type_map.get(name.value).unwrap().clone();
                        if let Type::DataStruct(user_type) = user_type {
                            let size = user_type.generics_define.len();
                            if size == generic_types.len() {
                                for i in 0..size {
                                    let generic_type_old = &user_type.generics_define[i];
                                    let generic_type_new = &generic_types[i];

                                    let mut bounds_old = generic_type_old.bounds.lock().unwrap();
                                    let mut bounds_new = generic_type_new.bounds.lock().unwrap();

                                    if generic_type_old.define_entity_id == generic_type_new.define_entity_id {
                                        mem::swap(bounds_old.deref_mut(), bounds_new.deref_mut());
                                    }
                                }
                            }
                        }
                    }

                    collect_module_element_types_program(
                        ast,
                        user_type_map,
                        import_element_map,
                        name_resolved_map, module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        Some(name.value),
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
                                        context
                                    )
                                },
                                _ => Type::Unknown
                            }
                        },
                        _ => Type::Unknown
                    };
                    
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
    current_user_type_name: Option<&str>,
    context: &TranspileModuleContext
) -> Option<(&'allocator str, Type)> {
    let generics_define = match &ast.generics_define {
        Some(generics_define) => {
            get_generic_type(
                generics_define,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                current_user_type_name,
                context
            )
        },
        _ => Vec::new()
    };
    
    let return_type = match &ast.type_tag {
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
                        context
                    )
                },
                _ => Type::Unknown
            }
        },
        _ => Type::Unknown
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
                    context
                )
            },
            _ => Type::Unknown
        };
        argument_types.push(type_info);
    }

    let function_type = Arc::new(FunctionType {
        is_extention: current_user_type_name.is_some(),
        generics_define,
        argument_types,
        return_type
    });

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
            Some((name, Type::Function(function_type)))
        },
        _ => None
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
    context: &TranspileModuleContext
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expression { expression, error_tokens: _ } => {
                    if let Ok(expression) = expression {
                        collect_module_element_types_expression(
                            &expression,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            generics_map,
                            context
                        );
                    }
                },
                _ => {}
            }

            if let Some(function_call) = &simple.1 {
                collect_module_element_types_function_call(
                    function_call,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    context
                );
            }
        },
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Ok(function_call) = &new_expression.function_call {
                collect_module_element_types_function_call(
                    function_call,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    context
                );
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
    context: &TranspileModuleContext
) {
    if let Some(second_expr) = &ast.second_expr {
        if let Some(function_call) = &second_expr.1 {
            collect_module_element_types_function_call(
                function_call,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
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
                context
            );
        }
    }
}

fn get_type(
    ast: &TypeInfo,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    context: &TranspileModuleContext
) -> Type {
    todo!()
}

fn get_generic_type<'allocator>(
    ast: &GenericsDefine,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &mut FxHashMap<String, Type>,
    generics_map: &mut FxHashMap<EntityID, Arc<GenericType>>,
    current_user_type_name: Option<&str>,
    context: &TranspileModuleContext
) -> Vec<Arc<GenericType>> {
    // TODO - register to generics_map
    todo!()
}