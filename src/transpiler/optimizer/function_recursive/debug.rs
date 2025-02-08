use std::ops::{Deref, Range};

use ariadne::{Color, Label, ReportBuilder};
use catla_parser::parser::{
    AddOrSubExpression, AndExpression, CompareExpression, Expression, ExpressionEnum, Factor,
    FunctionCall, MappingOperator, MappingOperatorKind, MulOrDivExpression, OrExpression, Primary,
    PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, StatementAST, AST,
};
use either::Either;

use crate::transpiler::{component::EntityID, context::TranspileModuleContext};

struct FunctionRecursiveResult {
    span: Range<usize>,
    is_recursive: bool,
}

impl FunctionRecursiveResult {
    fn new<T: AST>(ast: &T, span: Range<usize>, context: &TranspileModuleContext) -> Self {
        Self {
            span,
            is_recursive: context.context.function_recursive_info.is_recursive(
                &context.module_name,
                &EntityID::from(ast),
                &context.context,
            ),
        }
    }
}

pub(crate) fn print_recursive_functions(
    ast: Program,
    builder: &mut ReportBuilder<'_, (String, Range<usize>)>,
    context: &TranspileModuleContext,
) {
    let mut results = Vec::new();

    collect_recursive_result_program(ast, &mut results, context);

    if results.is_empty() {
        return;
    }

    for result in results {
        let color = if result.is_recursive {
            Color::Yellow
        } else {
            Color::Cyan
        };

        builder.add_label(
            Label::new((context.module_name.deref().clone(), result.span))
                .with_color(color)
                .with_message(format!("is_recursive : {}", result.is_recursive)),
        );
    }
}

fn collect_recursive_result_program(
    ast: Program,
    results: &mut Vec<FunctionRecursiveResult>,
    context: &TranspileModuleContext,
) {
    for statement in ast.statements.iter() {
        let Ok(statement) = statement else { continue };

        match statement {
            StatementAST::Assignment(assignment) => {
                collect_recursive_result_expression(assignment.left_expr, results, context);

                if let Ok(expression) = assignment.right_expr {
                    collect_recursive_result_expression(expression, results, context);
                }
            }
            StatementAST::Exchange(exchange) => {
                collect_recursive_result_expression(exchange.left_expr, results, context);

                if let Ok(expression) = exchange.right_expr {
                    collect_recursive_result_expression(expression, results, context);
                }
            }
            StatementAST::Import(_) => {}
            StatementAST::StatementAttributes(_) => todo!(),
            StatementAST::VariableDefine(variable_define) => {
                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        collect_recursive_result_expression(*expression, results, context);
                    }
                }
            }
            StatementAST::FunctionDefine(function_define) => {
                if let Ok(name) = &function_define.name {
                    let result =
                        FunctionRecursiveResult::new(function_define, name.span.clone(), context);
                    results.push(result);
                }

                if let Some(block_or_semicolon) = &function_define.block_or_semicolon.value {
                    if let Either::Right(block) = block_or_semicolon {
                        collect_recursive_result_program(block.program, results, context);
                    }
                }
            }
            StatementAST::UserTypeDefine(user_type_define) => {
                if let Some(block) = &user_type_define.block.value {
                    collect_recursive_result_program(block.program, results, context);
                }
            }
            StatementAST::TypeDefine(_) => {}
            StatementAST::Implements(implements) => {
                if let Some(block) = &implements.block.value {
                    collect_recursive_result_program(block.program, results, context);
                }
            }
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = drop_statement.expression {
                    collect_recursive_result_expression(expression, results, context);
                }
            }
            StatementAST::Expression(expression) => {
                collect_recursive_result_expression(*expression, results, context);
            }
            StatementAST::TranspilerTag(_) => {}
        }
    }
}

fn collect_recursive_result_expression(
    ast: Expression,
    results: &mut Vec<FunctionRecursiveResult>,
    context: &TranspileModuleContext,
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            collect_recursive_result_or_expression(or_expression, results, context);
        }
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                collect_recursive_result_expression(expression, results, context);
            }
        }
        ExpressionEnum::Closure(closure) => {
            if let Some(expression_or_block) = &closure.expression_or_block.value {
                match expression_or_block {
                    Either::Left(expression) => {
                        collect_recursive_result_expression(*expression, results, context);
                    }
                    Either::Right(block) => {
                        collect_recursive_result_program(block.program, results, context);
                    }
                }
            }
        }
    }
}

macro_rules! collect_recursive_result_op2 {
    ($function_name:ident, $ast_type:ident, $next_layer_function_name:ident) => {
        fn $function_name(
            ast: &$ast_type,
            results: &mut Vec<FunctionRecursiveResult>,
            context: &TranspileModuleContext,
        ) {
            $next_layer_function_name(&ast.left_expr, results, context);

            for (_, right_expr) in ast.right_exprs.iter() {
                let Ok(right_expr) = right_expr else { continue };

                $next_layer_function_name(right_expr, results, context);
            }
        }
    };
}

collect_recursive_result_op2!(
    collect_recursive_result_or_expression,
    OrExpression,
    collect_recursive_result_and_expression
);

collect_recursive_result_op2!(
    collect_recursive_result_and_expression,
    AndExpression,
    collect_recursive_result_compare_expression
);

collect_recursive_result_op2!(
    collect_recursive_result_compare_expression,
    CompareExpression,
    collect_recursive_result_add_or_sub_expression
);

collect_recursive_result_op2!(
    collect_recursive_result_add_or_sub_expression,
    AddOrSubExpression,
    collect_recursive_result_mul_or_div_expression
);

collect_recursive_result_op2!(
    collect_recursive_result_mul_or_div_expression,
    MulOrDivExpression,
    collect_recursive_result_factor
);

fn collect_recursive_result_factor(
    ast: &Factor,
    results: &mut Vec<FunctionRecursiveResult>,
    context: &TranspileModuleContext,
) {
    if let Ok(primary) = &ast.primary {
        collect_recursive_result_primary(primary, results, context);
    }
}

fn collect_recursive_result_primary(
    ast: &Primary,
    results: &mut Vec<FunctionRecursiveResult>,
    context: &TranspileModuleContext,
) {
    collect_recursive_result_primary_left(&ast.left, results, context);

    for primary_right in ast.chain.iter() {
        collect_recursive_result_primary_right(primary_right, results, context);
    }
}

fn collect_recursive_result_primary_left(
    ast: &PrimaryLeft,
    results: &mut Vec<FunctionRecursiveResult>,
    context: &TranspileModuleContext,
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple((simple_primary, _, function_call)) => {
            if let SimplePrimary::Expressions {
                expressions,
                error_tokens: _,
                span: _,
            } = simple_primary
            {
                for expression in expressions.iter() {
                    collect_recursive_result_expression(*expression, results, context);
                }
            }
            if let Some(function_call) = function_call {
                collect_recursive_result_function_call(function_call, results, context);
            }
        }
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => {
            if let Ok(expression) = new_array_init_expression.init_expression {
                collect_recursive_result_expression(expression, results, context);
            }
            if let Ok(expression) = new_array_init_expression.length_expression {
                collect_recursive_result_expression(expression, results, context);
            }
        }
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => {
            for expression in new_array_expression.value_expressions.iter() {
                if let Ok(expression) = expression {
                    collect_recursive_result_expression(*expression, results, context);
                }
            }
        }
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Ok(field_assigns) = &new_expression.field_assigns {
                for field_assign in field_assigns.iter() {
                    if let Ok(expression) = field_assign.expression {
                        collect_recursive_result_expression(expression, results, context);
                    }
                }
            }
        }
        PrimaryLeftExpr::IfExpression(if_expression) => {
            if let Ok(expression) = if_expression.if_statement.condition {
                collect_recursive_result_expression(expression, results, context);
            }
            if let Some(block) = &if_expression.if_statement.block.value {
                collect_recursive_result_program(block.program, results, context);
            }

            for chain in if_expression.chain.iter() {
                if let Some(else_if_or_else) = &chain.else_if_or_else.value {
                    match else_if_or_else {
                        Either::Left(if_statement) => {
                            if let Ok(expression) = if_statement.condition {
                                collect_recursive_result_expression(expression, results, context);
                            }
                            if let Some(block) = &if_statement.block.value {
                                collect_recursive_result_program(block.program, results, context);
                            }
                        }
                        Either::Right(block) => {
                            collect_recursive_result_program(block.program, results, context);
                        }
                    }
                }
            }
        }
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                collect_recursive_result_program(block.program, results, context);
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_recursive_result_mapping_operator(mapping_operator, results, context);
    }
}

fn collect_recursive_result_primary_right(
    ast: &PrimaryRight,
    results: &mut Vec<FunctionRecursiveResult>,
    context: &TranspileModuleContext,
) {
    if let Some((_, _, function_call)) = &ast.second_expr {
        if let Some(function_call) = function_call {
            collect_recursive_result_function_call(function_call, results, context);
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_recursive_result_mapping_operator(mapping_operator, results, context);
    }
}

fn collect_recursive_result_function_call(
    ast: &FunctionCall,
    results: &mut Vec<FunctionRecursiveResult>,
    context: &TranspileModuleContext,
) {
    if let Ok(args) = &ast.arg_exprs {
        for expression in args.iter() {
            collect_recursive_result_expression(*expression, results, context);
        }
    }
}

fn collect_recursive_result_mapping_operator(
    ast: &MappingOperator,
    results: &mut Vec<FunctionRecursiveResult>,
    context: &TranspileModuleContext,
) {
    let block = match &ast.value {
        MappingOperatorKind::NullPropagation => return,
        MappingOperatorKind::NullUnwrap => return,
        MappingOperatorKind::NullElvisBlock(recovered) => &recovered.value,
        MappingOperatorKind::ResultPropagation => return,
        MappingOperatorKind::ResultUnwrap => return,
        MappingOperatorKind::ResultElvisBlock(recovered) => &recovered.value,
    };

    if let Some(block) = block {
        collect_recursive_result_program(block.program, results, context);
    }
}
