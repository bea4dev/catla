use std::{collections::HashMap, sync::{Arc, Mutex}};

use catla_parser::parser::{AddOrSubExpression, AndExpression, CompareExpression, EQNEExpression, Expression, ExpressionEnum, Factor, FunctionCall, MappingOperator, MappingOperatorKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, Spanned, StatementAST};
use either::Either;

use crate::transpiler::context::TranspileModuleContext;

use super::type_info::{DataStructInfo, Type};


pub fn collect_user_type_program(
    ast: Program,
    module_element_types: &mut HashMap<String, Type>,
    context: &TranspileModuleContext
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                collect_user_type_expression(assignment.left_expr, module_element_types, context);
                if let Ok(right_expr) = &assignment.right_expr {
                    collect_user_type_expression(&right_expr, module_element_types, context);
                }
            },
            StatementAST::Exchange(exchange) => {
                collect_user_type_expression(exchange.left_expr, module_element_types, context);
                if let Ok(right_expr) = &exchange.right_expr {
                    collect_user_type_expression(&right_expr, module_element_types, context);
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                if let Some(block) = &function_define.block.value {
                    collect_user_type_program(block.program, module_element_types, context);
                }
            },
            StatementAST::DataStructDefine(data_struct_define) => {
                if let Ok(name) = &data_struct_define.name {
                    let name = Spanned::new(name.value.to_string(), name.span.clone());
                    let type_name = name.value.clone();
                    let data_struct_info = DataStructInfo {
                        module_name: context.module_name.clone(),
                        name,
                        define_span: data_struct_define.span.clone(),
                        kind: data_struct_define.kind.value.clone(),
                        generics_define: Vec::new(),
                        element_types: Mutex::new(HashMap::new()),
                    };

                    if let Some(block) = &data_struct_define.block.value {
                        collect_user_type_program(
                            block.program,
                            module_element_types,
                            context
                        );
                    }

                    module_element_types.insert(type_name, Type::DataStruct(Some(Arc::new(data_struct_info))));
                }
            },
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = drop_statement.expression {
                    collect_user_type_expression(expression, module_element_types, context);
                }
            },
            StatementAST::Expression(expression) => {
                collect_user_type_expression(&expression, module_element_types, context);
            },
            StatementAST::VariableDefine(variable_define) => {
                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        collect_user_type_expression(&expression, module_element_types, context);
                    }
                }
            }
            _ => {}
        }
    }
}

fn collect_user_type_expression(
    ast: Expression,
    module_element_types: &mut HashMap<String, Type>,
    context: &TranspileModuleContext
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            collect_user_type_and_expression(&or_expression.left_expr, module_element_types, context);
            for right_expr in or_expression.right_exprs.iter() {
                if let Ok(right_expr) = &right_expr.1 {
                    collect_user_type_and_expression(right_expr, module_element_types, context);
                }
            }
        },
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                collect_user_type_expression(expression, module_element_types, context);
            }
        },
        ExpressionEnum::Closure(closure) => {
            if let Some(expression_or_block) = &closure.expression_or_block.value {
                match expression_or_block {
                    Either::Left(expression) => {
                        collect_user_type_expression(&expression, module_element_types, context);
                    },
                    Either::Right(block) => {
                        collect_user_type_program(block.program, module_element_types, context);
                    },
                }
            }
        }
    }
}

fn collect_user_type_and_expression(
    ast: &AndExpression,
    module_element_types: &mut HashMap<String, Type>,
    context: &TranspileModuleContext
) {
    collect_user_type_eqne_expression(&ast.left_expr, module_element_types, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_user_type_eqne_expression(right_expr, module_element_types, context);
        }
    }
}

fn collect_user_type_eqne_expression(
    ast: &EQNEExpression,
    module_element_types: &mut HashMap<String, Type>,
    context: &TranspileModuleContext
) {
    collect_user_type_compare_expression(&ast.left_expr, module_element_types, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_user_type_compare_expression(right_expr, module_element_types, context);
        }
    }
}

fn collect_user_type_compare_expression(
    ast: &CompareExpression,
    module_element_types: &mut HashMap<String, Type>,
    context: &TranspileModuleContext
) {
    collect_user_type_add_or_sub_expression(&ast.left_expr, module_element_types, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_user_type_add_or_sub_expression(right_expr, module_element_types, context);
        }
    }
}

fn collect_user_type_add_or_sub_expression(
    ast: &AddOrSubExpression,
    module_element_types: &mut HashMap<String, Type>,
    context: &TranspileModuleContext
) {
    collect_user_type_mul_or_div_expression(&ast.left_expr, module_element_types, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_user_type_mul_or_div_expression(right_expr, module_element_types, context);
        }
    }
}

fn collect_user_type_mul_or_div_expression(
    ast: &MulOrDivExpression,
    module_element_types: &mut HashMap<String, Type>,
    context: &TranspileModuleContext
) {
    collect_user_type_factor(&ast.left_expr, module_element_types, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_user_type_factor(right_expr, module_element_types, context);
        }
    }
}

fn collect_user_type_factor(
    ast: &Factor,
    module_element_types: &mut HashMap<String, Type>,
    context: &TranspileModuleContext
) {
    if let Ok(primary) = &ast.primary {
        collect_user_type_primary(primary, module_element_types, context);
    }
}

fn collect_user_type_primary(
    ast: &Primary,
    module_element_types: &mut HashMap<String, Type>,
    context: &TranspileModuleContext
) {
    collect_user_type_primary_left(&ast.left, module_element_types, context);
    for primary_right in ast.chain.iter() {
        collect_user_type_primary_right(primary_right, module_element_types, context);
    }
}

fn collect_user_type_primary_left(
    ast: &PrimaryLeft,
    module_element_types: &mut HashMap<String, Type>,
    context: &TranspileModuleContext
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expression { expression, error_tokens: _ } => {
                    if let Ok(expression) = expression {
                        collect_user_type_expression(&expression, module_element_types, context);
                    }
                },
                _ => {}
            }

            if let Some(function_call) = &simple.1 {
                collect_user_type_function_call(function_call, module_element_types, context);
            }
        },
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Ok(function_call) = &new_expression.function_call {
                collect_user_type_function_call(function_call, module_element_types, context);
            }
        },
        PrimaryLeftExpr::IfExpression(if_expression) => {
            let first_statement = &if_expression.if_statement;
            if let Ok(condition) = &first_statement.condition {
                collect_user_type_expression(&condition, module_element_types, context);
            }
            if let Some(block) = &first_statement.block.value {
                collect_user_type_program(block.program, module_element_types, context);
            }

            for else_if_or_block in if_expression.chain.iter() {
                if let Some(else_if_or_block) = &else_if_or_block.else_if_or_else.value {
                    match else_if_or_block {
                        Either::Left(if_statement) => {
                            if let Ok(condition) = &if_statement.condition {
                                collect_user_type_expression(&condition, module_element_types, context);
                            }
                            if let Some(block) = &if_statement.block.value {
                                collect_user_type_program(block.program, module_element_types, context);
                            }
                        },
                        Either::Right(block) => {
                            collect_user_type_program(block.program, module_element_types, context);
                        },
                    }
                }
            }
        },
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                collect_user_type_program(block.program, module_element_types, context);
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_user_type_mapping_operator(mapping_operator, module_element_types, context);
    }
}

fn collect_user_type_primary_right(
    ast: &PrimaryRight,
    module_element_types: &mut HashMap<String, Type>,
    context: &TranspileModuleContext
) {
    if let Some(second_expr) = &ast.second_expr {
        if let Some(function_call) = &second_expr.1 {
            collect_user_type_function_call(function_call, module_element_types, context);
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_user_type_mapping_operator(mapping_operator, module_element_types, context);
    }
}

fn collect_user_type_mapping_operator(
    ast: &MappingOperator,
    module_element_types: &mut HashMap<String, Type>,
    context: &TranspileModuleContext
) {
    let block = match &ast.value {
        MappingOperatorKind::NullElvisBlock(block) => block,
        MappingOperatorKind::ResultElvisBlock(block) => block,
        _ => return
    };

    if let Some(block) = &block.value {
        collect_user_type_program(block.program, module_element_types, context);
    }
}

fn collect_user_type_function_call(
    ast: &FunctionCall,
    module_element_types: &mut HashMap<String, Type>,
    context: &TranspileModuleContext
) {
    if let Ok(arg_exprs) = &ast.arg_exprs {
        for arg_expr in arg_exprs.iter() {
            collect_user_type_expression(&arg_expr, module_element_types, context);
        }
    }
}