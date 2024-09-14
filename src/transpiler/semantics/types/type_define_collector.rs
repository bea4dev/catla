use std::sync::{Arc, Mutex};

use catla_parser::parser::{AddOrSubExpression, AndExpression, CompareExpression, Expression, ExpressionEnum, Factor, FunctionCall, MappingOperator, MappingOperatorKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, Spanned, StatementAST, UserTypeKindEnum};
use either::Either;
use fxhash::FxHashMap;

use crate::transpiler::{component::EntityID, context::TranspileModuleContext};

use super::type_info::{UserTypeInfo, FreezableMutex, GenericType, Type, WithDefineInfo};


pub(crate) fn collect_user_type_program(
    ast: Program,
    user_type_map: &mut FxHashMap<String, Type>,
    context: &TranspileModuleContext
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                collect_user_type_expression(assignment.left_expr, user_type_map, context);
                if let Ok(right_expr) = &assignment.right_expr {
                    collect_user_type_expression(&right_expr, user_type_map, context);
                }
            },
            StatementAST::Exchange(exchange) => {
                collect_user_type_expression(exchange.left_expr, user_type_map, context);
                if let Ok(right_expr) = &exchange.right_expr {
                    collect_user_type_expression(&right_expr, user_type_map, context);
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                if let Some(semicolon_or_block) = &function_define.block_or_semicolon.value {
                    if let Either::Right(block) = semicolon_or_block {
                        collect_user_type_program(block.program, user_type_map, context);
                    }
                }
            },
            StatementAST::UserTypeDefine(user_type_define) => {
                if let Ok(name) = &user_type_define.name {
                    let name = Spanned::new(name.value.to_string(), name.span.clone());
                    let type_name = name.value.clone();

                    let mut generics_define = Vec::new();
                    let mut generics_define_span = None;
                    if let Some(generics) = &user_type_define.generics_define {
                        for element in generics.elements.iter() {
                            let generic_type = Arc::new(GenericType {
                                define_entity_id: EntityID::from(element),
                                name: Arc::new(element.name.value.to_string()),
                                bounds: FreezableMutex::new(Vec::new()),
                                location: WithDefineInfo { value: (), module_name: context.module_name.clone(), span: element.span.clone() }
                            });
                            generics_define.push(generic_type);
                        }
                        generics_define_span = Some(generics.span.clone());
                    }

                    let user_type_info = UserTypeInfo {
                        module_name: context.module_name.clone(),
                        name,
                        define_span: user_type_define.span.clone(),
                        kind: user_type_define.kind.value.clone(),
                        generics_define,
                        generics_define_span,
                        element_types: Mutex::new(FxHashMap::default()),
                        element_attributes: Default::default(),
                        where_bounds: Default::default(),
                        super_type: Default::default()
                    };

                    if let Some(block) = &user_type_define.block.value {
                        collect_user_type_program(
                            block.program,
                            user_type_map,
                            context
                        );
                    }

                    let ty = Type::UserType {
                        user_type_info: Arc::new(user_type_info),
                        generics: Arc::new(Vec::new()),
                        generics_span: None
                    };
                    user_type_map.insert(type_name, ty);
                }
            },
            StatementAST::TypeDefine(type_define) => {
                if let Ok(name) = &type_define.name {
                    let name = Spanned::new(name.value.to_string(), name.span.clone());
                    let type_name = name.value.clone();

                    let mut generics_define = Vec::new();
                    let mut generics_define_span = None;
                    if let Some(generics) = &type_define.generics_define {
                        for element in generics.elements.iter() {
                            let generic_type = Arc::new(GenericType {
                                define_entity_id: EntityID::from(element),
                                name: Arc::new(element.name.value.to_string()),
                                bounds: FreezableMutex::new(Vec::new()),
                                location: WithDefineInfo { value: (), module_name: context.module_name.clone(), span: element.span.clone() }
                            });
                            generics_define.push(generic_type);
                        }
                        generics_define_span = Some(generics.span.clone());
                    }

                    let data_struct_info = UserTypeInfo {
                        module_name: context.module_name.clone(),
                        name,
                        define_span: type_define.span.clone(),
                        kind: UserTypeKindEnum::Struct,
                        generics_define,
                        generics_define_span,
                        element_types: Mutex::new(FxHashMap::default()),
                        element_attributes: Default::default(),
                        where_bounds: Default::default(),
                        super_type: FreezableMutex::new(Vec::new())
                    };

                    let ty = Type::UserType {
                        user_type_info: Arc::new(data_struct_info),
                        generics: Arc::new(Vec::new()),
                        generics_span: None
                    };
                    user_type_map.insert(type_name, ty);
                }
            },
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = drop_statement.expression {
                    collect_user_type_expression(expression, user_type_map, context);
                }
            },
            StatementAST::Expression(expression) => {
                collect_user_type_expression(&expression, user_type_map, context);
            },
            StatementAST::VariableDefine(variable_define) => {
                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        collect_user_type_expression(&expression, user_type_map, context);
                    }
                }
            }
            _ => {}
        }
    }
}

fn collect_user_type_expression(
    ast: Expression,
    user_type_map: &mut FxHashMap<String, Type>,
    context: &TranspileModuleContext
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            collect_user_type_and_expression(&or_expression.left_expr, user_type_map, context);
            for right_expr in or_expression.right_exprs.iter() {
                if let Ok(right_expr) = &right_expr.1 {
                    collect_user_type_and_expression(right_expr, user_type_map, context);
                }
            }
        },
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                collect_user_type_expression(expression, user_type_map, context);
            }
        },
        ExpressionEnum::Closure(closure) => {
            if let Some(expression_or_block) = &closure.expression_or_block.value {
                match expression_or_block {
                    Either::Left(expression) => {
                        collect_user_type_expression(&expression, user_type_map, context);
                    },
                    Either::Right(block) => {
                        collect_user_type_program(block.program, user_type_map, context);
                    },
                }
            }
        }
    }
}

fn collect_user_type_and_expression(
    ast: &AndExpression,
    user_type_map: &mut FxHashMap<String, Type>,
    context: &TranspileModuleContext
) {
    collect_user_type_compare_expression(&ast.left_expr, user_type_map, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_user_type_compare_expression(right_expr, user_type_map, context);
        }
    }
}

fn collect_user_type_compare_expression(
    ast: &CompareExpression,
    user_type_map: &mut FxHashMap<String, Type>,
    context: &TranspileModuleContext
) {
    collect_user_type_add_or_sub_expression(&ast.left_expr, user_type_map, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_user_type_add_or_sub_expression(right_expr, user_type_map, context);
        }
    }
}

fn collect_user_type_add_or_sub_expression(
    ast: &AddOrSubExpression,
    user_type_map: &mut FxHashMap<String, Type>,
    context: &TranspileModuleContext
) {
    collect_user_type_mul_or_div_expression(&ast.left_expr, user_type_map, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_user_type_mul_or_div_expression(right_expr, user_type_map, context);
        }
    }
}

fn collect_user_type_mul_or_div_expression(
    ast: &MulOrDivExpression,
    user_type_map: &mut FxHashMap<String, Type>,
    context: &TranspileModuleContext
) {
    collect_user_type_factor(&ast.left_expr, user_type_map, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_user_type_factor(right_expr, user_type_map, context);
        }
    }
}

fn collect_user_type_factor(
    ast: &Factor,
    user_type_map: &mut FxHashMap<String, Type>,
    context: &TranspileModuleContext
) {
    if let Ok(primary) = &ast.primary {
        collect_user_type_primary(primary, user_type_map, context);
    }
}

fn collect_user_type_primary(
    ast: &Primary,
    user_type_map: &mut FxHashMap<String, Type>,
    context: &TranspileModuleContext
) {
    collect_user_type_primary_left(&ast.left, user_type_map, context);
    for primary_right in ast.chain.iter() {
        collect_user_type_primary_right(primary_right, user_type_map, context);
    }
}

fn collect_user_type_primary_left(
    ast: &PrimaryLeft,
    user_type_map: &mut FxHashMap<String, Type>,
    context: &TranspileModuleContext
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expressions { expressions: expression, error_tokens: _, span: _ } => {
                    if let Ok(expression) = expression {
                        collect_user_type_expression(&expression, user_type_map, context);
                    }
                },
                _ => {}
            }

            if let Some(function_call) = &simple.2 {
                collect_user_type_function_call(function_call, user_type_map, context);
            }
        },
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => {
            if let Ok(init_expression) = new_array_init_expression.init_expression {
                collect_user_type_expression(init_expression, user_type_map, context);
            }
            if let Ok(length_expression) = new_array_init_expression.length_expression {
                collect_user_type_expression(length_expression, user_type_map, context);
            }
        },
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => {
            for value_expression in new_array_expression.value_expressions.iter() {
                if let Ok(value_expression) = value_expression {
                    collect_user_type_expression(value_expression, user_type_map, context);
                }
            }
        },
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Ok(field_assigns) = &new_expression.field_assigns {
                for field_assign in field_assigns.iter() {
                    if let Ok(expression) = &field_assign.expression {
                        collect_user_type_expression(&expression, user_type_map, context);
                    }
                }
            }
        },
        PrimaryLeftExpr::IfExpression(if_expression) => {
            let first_statement = &if_expression.if_statement;
            if let Ok(condition) = &first_statement.condition {
                collect_user_type_expression(&condition, user_type_map, context);
            }
            if let Some(block) = &first_statement.block.value {
                collect_user_type_program(block.program, user_type_map, context);
            }

            for else_if_or_block in if_expression.chain.iter() {
                if let Some(else_if_or_block) = &else_if_or_block.else_if_or_else.value {
                    match else_if_or_block {
                        Either::Left(if_statement) => {
                            if let Ok(condition) = &if_statement.condition {
                                collect_user_type_expression(&condition, user_type_map, context);
                            }
                            if let Some(block) = &if_statement.block.value {
                                collect_user_type_program(block.program, user_type_map, context);
                            }
                        },
                        Either::Right(block) => {
                            collect_user_type_program(block.program, user_type_map, context);
                        },
                    }
                }
            }
        },
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                collect_user_type_program(block.program, user_type_map, context);
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_user_type_mapping_operator(mapping_operator, user_type_map, context);
    }
}

fn collect_user_type_primary_right(
    ast: &PrimaryRight,
    user_type_map: &mut FxHashMap<String, Type>,
    context: &TranspileModuleContext
) {
    if let Some(second_expr) = &ast.second_expr {
        if let Some(function_call) = &second_expr.2 {
            collect_user_type_function_call(function_call, user_type_map, context);
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_user_type_mapping_operator(mapping_operator, user_type_map, context);
    }
}

fn collect_user_type_mapping_operator(
    ast: &MappingOperator,
    user_type_map: &mut FxHashMap<String, Type>,
    context: &TranspileModuleContext
) {
    let block = match &ast.value {
        MappingOperatorKind::NullElvisBlock(block) => block,
        MappingOperatorKind::ResultElvisBlock(block) => block,
        _ => return
    };

    if let Some(block) = &block.value {
        collect_user_type_program(block.program, user_type_map, context);
    }
}

fn collect_user_type_function_call(
    ast: &FunctionCall,
    user_type_map: &mut FxHashMap<String, Type>,
    context: &TranspileModuleContext
) {
    if let Ok(arg_exprs) = &ast.arg_exprs {
        for arg_expr in arg_exprs.iter() {
            collect_user_type_expression(&arg_expr, user_type_map, context);
        }
    }
}
