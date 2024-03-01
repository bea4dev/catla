use std::{collections::HashMap, sync::{Arc, Mutex}};

use catla_parser::parser::{AddOrSubExpression, AndExpression, CompareExpression, EQNEExpression, Expression, ExpressionEnum, Factor, FunctionCall, FunctionDefine, MappingOperator, MappingOperatorKind, MemoryManageAttributeKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, Spanned, StatementAST, TypeInfo};
use either::Either;
use fxhash::FxHashMap;

use crate::transpiler::{component::EntityID, context::TranspileModuleContext, name_resolver::FoundDefineInfo};

use super::type_info::{DataStructInfo, FunctionType, Type};


pub fn collect_user_type_element_program(
    ast: Program,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    current_user_type_name: Option<&str>,
    context: &TranspileModuleContext
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        match current_user_type_name {
            Some(user_type_name) => {
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
                            Some(user_type_name),
                            context
                        )
                    },
                    _ => None
                };

                if let Some(element) = element {
                    let user_type = user_type_map.get(user_type_name).unwrap().clone();

                    if let Type::DataStruct(user_type) = user_type {
                        let mut element_map = user_type.element_types.lock().unwrap();
                        element_map.insert(element.0.to_string(), element.1);
                    }
                }
            },
            _ => {
                
            }
        }

        match statement {
            StatementAST::Assignment(assignment) => {
                collect_user_type_element_expression(assignment.left_expr, user_type_map, import_element_map, name_resolved_map, context);
                if let Ok(right_expr) = &assignment.right_expr {
                    collect_user_type_element_expression(&right_expr, user_type_map, import_element_map, name_resolved_map, context);
                }
            },
            StatementAST::Exchange(exchange) => {
                collect_user_type_element_expression(exchange.left_expr, user_type_map, import_element_map, name_resolved_map, context);
                if let Ok(right_expr) = &exchange.right_expr {
                    collect_user_type_element_expression(&right_expr, user_type_map, import_element_map, name_resolved_map, context);
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                if let Some(block) = &function_define.block.value {
                    collect_user_type_element_program(block.program, user_type_map, import_element_map, name_resolved_map, None, context);
                }
            },
            StatementAST::DataStructDefine(data_struct_define) => {
                if let Ok(name) = &data_struct_define.name {
                    collect_user_type_element_program(
                        ast,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        Some(name.value),
                        context
                    );
                }
            },
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = drop_statement.expression {
                    collect_user_type_element_expression(expression, user_type_map, import_element_map, name_resolved_map, context);
                }
            },
            StatementAST::Expression(expression) => {
                collect_user_type_element_expression(&expression, user_type_map, import_element_map, name_resolved_map, context);
            },
            StatementAST::VariableDefine(variable_define) => {
                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        collect_user_type_element_expression(&expression, user_type_map, import_element_map, name_resolved_map, context);
                    }
                }
            }
            _ => {}
        }
    }
}

fn get_function_type_and_name<'allocator>(
    ast: &'allocator FunctionDefine,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    current_user_type_name: Option<&str>,
    context: &TranspileModuleContext
) -> Option<(&'allocator str, Type)> {
    let return_type = match &ast.type_tag {
        Some(type_tag) => {
            match &type_tag.type_info {
                Ok(type_info) => {
                    get_type(
                        type_info,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
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
                    context
                )
            },
            _ => Type::Unknown
        };
        argument_types.push(type_info);
    }

    let function_type = Arc::new(FunctionType {
        is_extention: current_user_type_name.is_some(),
        generics_define: Vec::new(),
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

fn collect_user_type_element_expression(
    ast: Expression,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    context: &TranspileModuleContext
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            collect_user_type_element_and_expression(&or_expression.left_expr, user_type_map, import_element_map, name_resolved_map, context);
            for right_expr in or_expression.right_exprs.iter() {
                if let Ok(right_expr) = &right_expr.1 {
                    collect_user_type_element_and_expression(right_expr, user_type_map, import_element_map, name_resolved_map, context);
                }
            }
        },
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                collect_user_type_element_expression(expression, user_type_map, import_element_map, name_resolved_map, context);
            }
        },
        ExpressionEnum::Closure(closure) => {
            if let Some(expression_or_block) = &closure.expression_or_block.value {
                match expression_or_block {
                    Either::Left(expression) => {
                        collect_user_type_element_expression(&expression, user_type_map, import_element_map, name_resolved_map, context);
                    },
                    Either::Right(block) => {
                        collect_user_type_element_program(block.program, user_type_map, import_element_map, name_resolved_map, context);
                    },
                }
            }
        }
    }
}

fn collect_user_type_element_and_expression(
    ast: &AndExpression,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    context: &TranspileModuleContext
) {
    collect_user_type_element_eqne_expression(&ast.left_expr, user_type_map, import_element_map, name_resolved_map, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_user_type_element_eqne_expression(right_expr, user_type_map, import_element_map, name_resolved_map, context);
        }
    }
}

fn collect_user_type_element_eqne_expression(
    ast: &EQNEExpression,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    context: &TranspileModuleContext
) {
    collect_user_type_element_compare_expression(&ast.left_expr, user_type_map, import_element_map, name_resolved_map, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_user_type_element_compare_expression(right_expr, user_type_map, import_element_map, name_resolved_map, context);
        }
    }
}

fn collect_user_type_element_compare_expression(
    ast: &CompareExpression,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    context: &TranspileModuleContext
) {
    collect_user_type_element_add_or_sub_expression(&ast.left_expr, user_type_map, import_element_map, name_resolved_map, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_user_type_element_add_or_sub_expression(right_expr, user_type_map, import_element_map, name_resolved_map, context);
        }
    }
}

fn collect_user_type_element_add_or_sub_expression(
    ast: &AddOrSubExpression,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    context: &TranspileModuleContext
) {
    collect_user_type_element_mul_or_div_expression(&ast.left_expr, user_type_map, import_element_map, name_resolved_map, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_user_type_element_mul_or_div_expression(right_expr, user_type_map, import_element_map, name_resolved_map, context);
        }
    }
}

fn collect_user_type_element_mul_or_div_expression(
    ast: &MulOrDivExpression,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    context: &TranspileModuleContext
) {
    collect_user_type_element_factor(&ast.left_expr, user_type_map, import_element_map, name_resolved_map, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_user_type_element_factor(right_expr, user_type_map, import_element_map, name_resolved_map, context);
        }
    }
}

fn collect_user_type_element_factor(
    ast: &Factor,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    context: &TranspileModuleContext
) {
    if let Ok(primary) = &ast.primary {
        collect_user_type_element_primary(primary, user_type_map, import_element_map, name_resolved_map, context);
    }
}

fn collect_user_type_element_primary(
    ast: &Primary,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    context: &TranspileModuleContext
) {
    collect_user_type_element_primary_left(&ast.left, user_type_map, import_element_map, name_resolved_map, context);
    for primary_right in ast.chain.iter() {
        collect_user_type_element_primary_right(primary_right, user_type_map, import_element_map, name_resolved_map, context);
    }
}

fn collect_user_type_element_primary_left(
    ast: &PrimaryLeft,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    context: &TranspileModuleContext
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expression { expression, error_tokens: _ } => {
                    if let Ok(expression) = expression {
                        collect_user_type_element_expression(&expression, user_type_map, import_element_map, name_resolved_map, context);
                    }
                },
                _ => {}
            }

            if let Some(function_call) = &simple.1 {
                collect_user_type_element_function_call(function_call, user_type_map, import_element_map, name_resolved_map, context);
            }
        },
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Ok(function_call) = &new_expression.function_call {
                collect_user_type_element_function_call(function_call, user_type_map, import_element_map, name_resolved_map, context);
            }
        },
        PrimaryLeftExpr::IfExpression(if_expression) => {
            let first_statement = &if_expression.if_statement;
            if let Ok(condition) = &first_statement.condition {
                collect_user_type_element_expression(&condition, user_type_map, import_element_map, name_resolved_map, context);
            }
            if let Some(block) = &first_statement.block.value {
                collect_user_type_element_program(block.program, user_type_map, import_element_map, name_resolved_map, context);
            }

            for else_if_or_block in if_expression.chain.iter() {
                if let Some(else_if_or_block) = &else_if_or_block.else_if_or_else.value {
                    match else_if_or_block {
                        Either::Left(if_statement) => {
                            if let Ok(condition) = &if_statement.condition {
                                collect_user_type_element_expression(&condition, user_type_map, import_element_map, name_resolved_map, context);
                            }
                            if let Some(block) = &if_statement.block.value {
                                collect_user_type_element_program(block.program, user_type_map, import_element_map, name_resolved_map, context);
                            }
                        },
                        Either::Right(block) => {
                            collect_user_type_element_program(block.program, user_type_map, import_element_map, name_resolved_map, context);
                        },
                    }
                }
            }
        },
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                collect_user_type_element_program(block.program, user_type_map, import_element_map, name_resolved_map, context);
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_user_type_element_mapping_operator(mapping_operator, user_type_map, import_element_map, name_resolved_map, context);
    }
}

fn collect_user_type_element_primary_right(
    ast: &PrimaryRight,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    context: &TranspileModuleContext
) {
    if let Some(second_expr) = &ast.second_expr {
        if let Some(function_call) = &second_expr.1 {
            collect_user_type_element_function_call(function_call, user_type_map, import_element_map, name_resolved_map, context);
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_user_type_element_mapping_operator(mapping_operator, user_type_map, import_element_map, name_resolved_map, context);
    }
}

fn collect_user_type_element_mapping_operator(
    ast: &MappingOperator,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    context: &TranspileModuleContext
) {
    let block = match &ast.value {
        MappingOperatorKind::NullElvisBlock(block) => block,
        MappingOperatorKind::ResultElvisBlock(block) => block,
        _ => return
    };

    if let Some(block) = &block.value {
        collect_user_type_element_program(block.program, user_type_map, import_element_map, name_resolved_map, context);
    }
}

fn collect_user_type_element_function_call(
    ast: &FunctionCall,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    context: &TranspileModuleContext
) {
    if let Ok(arg_exprs) = &ast.arg_exprs {
        for arg_expr in arg_exprs.iter() {
            collect_user_type_element_expression(&arg_expr, user_type_map, import_element_map, name_resolved_map, context);
        }
    }
}

fn get_type(
    ast: &TypeInfo,
    user_type_map: &HashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    context: &TranspileModuleContext
) -> Type {
    todo!()
}