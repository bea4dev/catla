use std::{collections::HashMap, sync::{Arc, Mutex}};

use ariadne::Color;
use catla_parser::parser::{expression, AddOrSubExpression, AndExpression, CompareExpression, EQNEExpression, Expression, ExpressionEnum, Factor, FunctionCall, Import, MappingOperator, MappingOperatorKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, Spanned, StatementAST};
use either::Either;

use crate::transpiler::{context::TranspileModuleContext, error::SimpleError, parse_error::collect_parse_error_program, resource::FILE_EXTENSION, transpile_module, TranspileError, TranspileWarning};

use super::type_info::{DataStructInfo, Type};


pub fn collect_import_module_program(
    ast: Program,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                collect_import_module_expression(assignment.left_expr, errors, warnings, context);
                if let Ok(right_expr) = &assignment.right_expr {
                    collect_import_module_expression(&right_expr, errors, warnings, context);
                }
            },
            StatementAST::Exchange(exchange) => {
                collect_import_module_expression(exchange.left_expr, errors, warnings, context);
                if let Ok(right_expr) = &exchange.right_expr {
                    collect_import_module_expression(&right_expr, errors, warnings, context);
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                if let Some(block) = &function_define.block.value {
                    collect_import_module_program(block.program, errors, warnings, context);
                }
            },
            StatementAST::DataStructDefine(data_struct_define) => {
                if let Some(block) = &data_struct_define.block.value {
                    collect_parse_error_program(block.program, errors, warnings, context);
                }
            },
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = drop_statement.expression {
                    collect_import_module_expression(expression, errors, warnings, context);
                }
            },
            StatementAST::Expression(expression) => {
                collect_import_module_expression(&expression, errors, warnings, context);
            },
            StatementAST::VariableDefine(variable_define) => {
                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        collect_import_module_expression(&expression, errors, warnings, context);
                    }
                }
            },
            StatementAST::Import(import) => {
                collect_import_module_import(import, errors, warnings, context);
            },
            _ => {}
        }
    }
}

fn collect_import_module_import(
    ast: &Import,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    let context = &context.context;

    let mut module_path_name = String::new();
    for path in ast.import_path.iter() {
        module_path_name += path.value;
        module_path_name += "/";
    }
    
    for element in ast.elements.elements.iter() {
        let module_name = module_path_name.clone() + element.value + FILE_EXTENSION;

        if !context.source_code_provider.exsists_source_code(&module_name) {
            let error = SimpleError::new(
                0027,
                element.span.clone(),
                vec![module_name],
                vec![(element.span.clone(), Color::Red)]
            );
            errors.push(error);
            continue
        }

        let context_temp = context.clone();
        context.future_runtime.spawn(async move {
            transpile_module(module_name, context_temp).await;
        });
    }
}

fn collect_import_module_expression(
    ast: Expression,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            collect_import_module_and_expression(&or_expression.left_expr, errors, warnings, context);
            for right_expr in or_expression.right_exprs.iter() {
                if let Ok(right_expr) = &right_expr.1 {
                    collect_import_module_and_expression(right_expr, errors, warnings, context);
                }
            }
        },
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                collect_import_module_expression(expression, errors, warnings, context);
            }
        },
        ExpressionEnum::Closure(closure) => {
            if let Some(expression_or_block) = &closure.expression_or_block.value {
                match expression_or_block {
                    Either::Left(expression) => {
                        collect_import_module_expression(&expression, errors, warnings, context);
                    },
                    Either::Right(block) => {
                        collect_import_module_program(block.program, errors, warnings, context);
                    },
                }
            }
        }
    }
}

fn collect_import_module_and_expression(
    ast: &AndExpression,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    collect_import_module_eqne_expression(&ast.left_expr, errors, warnings, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_import_module_eqne_expression(right_expr, errors, warnings, context);
        }
    }
}

fn collect_import_module_eqne_expression(
    ast: &EQNEExpression,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    collect_import_module_compare_expression(&ast.left_expr, errors, warnings, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_import_module_compare_expression(right_expr, errors, warnings, context);
        }
    }
}

fn collect_import_module_compare_expression(
    ast: &CompareExpression,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    collect_import_module_add_or_sub_expression(&ast.left_expr, errors, warnings, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_import_module_add_or_sub_expression(right_expr, errors, warnings, context);
        }
    }
}

fn collect_import_module_add_or_sub_expression(
    ast: &AddOrSubExpression,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    collect_import_module_mul_or_div_expression(&ast.left_expr, errors, warnings, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_import_module_mul_or_div_expression(right_expr, errors, warnings, context);
        }
    }
}

fn collect_import_module_mul_or_div_expression(
    ast: &MulOrDivExpression,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    collect_import_module_factor(&ast.left_expr, errors, warnings, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_import_module_factor(right_expr, errors, warnings, context);
        }
    }
}

fn collect_import_module_factor(
    ast: &Factor,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    if let Ok(primary) = &ast.primary {
        collect_import_module_primary(primary, errors, warnings, context);
    }
}

fn collect_import_module_primary(
    ast: &Primary,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    collect_import_module_primary_left(&ast.left, errors, warnings, context);
    for primary_right in ast.chain.iter() {
        collect_import_module_primary_right(primary_right, errors, warnings, context);
    }
}

fn collect_import_module_primary_left(
    ast: &PrimaryLeft,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expression { expression, error_tokens: _ } => {
                    if let Ok(expression) = expression {
                        collect_import_module_expression(&expression, errors, warnings, context);
                    }
                },
                _ => {}
            }

            if let Some(function_call) = &simple.1 {
                collect_import_module_function_call(function_call, errors, warnings, context);
            }
        },
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Ok(function_call) = &new_expression.function_call {
                collect_import_module_function_call(function_call, errors, warnings, context);
            }
        },
        PrimaryLeftExpr::IfExpression(if_expression) => {
            let first_statement = &if_expression.if_statement;
            if let Ok(condition) = &first_statement.condition {
                collect_import_module_expression(&condition, errors, warnings, context);
            }
            if let Some(block) = &first_statement.block.value {
                collect_import_module_program(block.program, errors, warnings, context);
            }

            for else_if_or_block in if_expression.chain.iter() {
                if let Some(else_if_or_block) = &else_if_or_block.else_if_or_else.value {
                    match else_if_or_block {
                        Either::Left(if_statement) => {
                            if let Ok(condition) = &if_statement.condition {
                                collect_import_module_expression(&condition, errors, warnings, context);
                            }
                            if let Some(block) = &if_statement.block.value {
                                collect_import_module_program(block.program, errors, warnings, context);
                            }
                        },
                        Either::Right(block) => {
                            collect_import_module_program(block.program, errors, warnings, context);
                        },
                    }
                }
            }
        },
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                collect_import_module_program(block.program, errors, warnings, context);
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_import_module_mapping_operator(mapping_operator, errors, warnings, context);
    }
}

fn collect_import_module_primary_right(
    ast: &PrimaryRight,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    if let Some(second_expr) = &ast.second_expr {
        if let Some(function_call) = &second_expr.1 {
            collect_import_module_function_call(function_call, errors, warnings, context);
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_import_module_mapping_operator(mapping_operator, errors, warnings, context);
    }
}

fn collect_import_module_mapping_operator(
    ast: &MappingOperator,
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
        collect_import_module_program(block.program, errors, warnings, context);
    }
}

fn collect_import_module_function_call(
    ast: &FunctionCall,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    if let Ok(arg_exprs) = &ast.arg_exprs {
        for arg_expr in arg_exprs.iter() {
            collect_import_module_expression(&arg_expr, errors, warnings, context);
        }
    }
}