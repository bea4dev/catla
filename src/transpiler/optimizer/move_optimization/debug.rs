use std::ops::{Deref, Range};

use ariadne::{Color, Label, ReportBuilder};
use catla_parser::parser::{
    AddOrSubExpression, AndExpression, CompareExpression, Expression, ExpressionEnum, Factor,
    FunctionCall, MappingOperator, MappingOperatorKind, MulOrDivExpression, OrExpression, Primary,
    PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, Spanned, StatementAST,
    VariableBinding,
};
use either::Either;

use crate::transpiler::{component::EntityID, context::TranspileModuleContext};

use super::variable_user_collector::VariableUserInfo;

struct VariableInfo {
    can_move: bool,
    span: Range<usize>,
}

pub(crate) fn print_variable_user_info(
    ast: Program,
    variable_move_info: &VariableUserInfo,
    builder: &mut ReportBuilder<'_, (String, Range<usize>)>,
    has_element: &mut bool,
    context: &TranspileModuleContext,
) {
    let mut info = Vec::new();

    collect_variable_info_program(ast, variable_move_info, &mut info);

    if info.is_empty() {
        return;
    }
    *has_element = true;

    for info in info {
        let color = if info.can_move {
            Color::Cyan
        } else {
            Color::Yellow
        };

        builder.add_label(
            Label::new((context.module_name.deref().clone(), info.span))
                .with_color(color)
                .with_message(format!("move : {}", info.can_move)),
        );
    }
}

fn collect_variable_info_program(
    ast: Program,
    variable_move_info: &VariableUserInfo,
    info: &mut Vec<VariableInfo>,
) {
    for statement in ast.statements.iter() {
        let Ok(statement) = statement else { continue };

        match statement {
            StatementAST::Assignment(assignment) => {
                collect_variable_info_expression(assignment.left_expr, variable_move_info, info);

                if let Ok(expression) = assignment.right_expr {
                    collect_variable_info_expression(expression, variable_move_info, info);
                }
            }
            StatementAST::Exchange(exchange) => {
                collect_variable_info_expression(exchange.left_expr, variable_move_info, info);

                if let Ok(expression) = exchange.right_expr {
                    collect_variable_info_expression(expression, variable_move_info, info);
                }
            }
            StatementAST::Import(_) => {}
            StatementAST::StatementAttributes(_) => {}
            StatementAST::VariableDefine(variable_define) => {
                if let Ok(variable_binding) = &variable_define.binding {
                    collect_variable_info_variable_binding(
                        variable_binding,
                        variable_move_info,
                        info,
                    );
                }

                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        collect_variable_info_expression(*expression, variable_move_info, info);
                    }
                }
            }
            StatementAST::FunctionDefine(function_define) => {
                for argument in function_define.args.arguments.iter() {
                    collect_variable_info_variable_binding(
                        &argument.binding,
                        variable_move_info,
                        info,
                    );
                }

                if let Some(semicolon_or_block) = &function_define.block_or_semicolon.value {
                    if let Either::Right(block) = semicolon_or_block {
                        collect_variable_info_program(block.program, variable_move_info, info);
                    }
                }
            }
            StatementAST::UserTypeDefine(user_type_define) => {
                if let Some(block) = &user_type_define.block.value {
                    collect_variable_info_program(block.program, variable_move_info, info);
                }
            }
            StatementAST::TypeDefine(_) => {}
            StatementAST::Implements(implements) => {
                if let Some(block) = &implements.block.value {
                    collect_variable_info_program(block.program, variable_move_info, info);
                }
            }
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = drop_statement.expression {
                    collect_variable_info_expression(expression, variable_move_info, info);
                }
            }
            StatementAST::Expression(expression) => {
                collect_variable_info_expression(*expression, variable_move_info, info);
            }
            StatementAST::TranspilerTag(_) => {}
        }
    }
}

fn collect_variable_info_variable_binding(
    ast: &VariableBinding,
    variable_move_info: &VariableUserInfo,
    info: &mut Vec<VariableInfo>,
) {
    match &ast.binding {
        Either::Left(literal) => {
            collect_variable_info_variable_literal(literal, variable_move_info, info);
        }
        Either::Right(bindings) => {
            for binding in bindings.iter() {
                collect_variable_info_variable_binding(binding, variable_move_info, info);
            }
        }
    }
}

fn collect_variable_info_variable_literal(
    literal: &Spanned<&str>,
    variable_move_info: &VariableUserInfo,
    info: &mut Vec<VariableInfo>,
) {
    let can_move = variable_move_info.can_move_variable(EntityID::from(literal));

    info.push(VariableInfo {
        can_move,
        span: literal.span.clone(),
    });
}

fn collect_variable_info_expression(
    ast: Expression,
    variable_move_info: &VariableUserInfo,
    info: &mut Vec<VariableInfo>,
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            collect_variable_info_or_expression(or_expression, variable_move_info, info);
        }
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                collect_variable_info_expression(expression, variable_move_info, info);
            }
        }
        ExpressionEnum::Closure(closure) => match &closure.arguments.arguments {
            Either::Left(literal) => {
                collect_variable_info_variable_literal(literal, variable_move_info, info);
            }
            Either::Right(arguments) => {
                for argument in arguments.iter() {
                    match argument {
                        Either::Left(argument) => {
                            collect_variable_info_variable_binding(
                                &argument.binding,
                                variable_move_info,
                                info,
                            );
                        }
                        Either::Right(literal) => {
                            collect_variable_info_variable_literal(
                                literal,
                                variable_move_info,
                                info,
                            );
                        }
                    }
                }
            }
        },
    }
}

macro_rules! collect_variable_info_op2 {
    ($function_name:ident, $ast_type:ident, $next_layer_function_name:ident) => {
        fn $function_name(
            ast: &$ast_type,
            variable_move_info: &VariableUserInfo,
            info: &mut Vec<VariableInfo>,
        ) {
            $next_layer_function_name(&ast.left_expr, variable_move_info, info);

            for (_, right_expr) in ast.right_exprs.iter() {
                if let Ok(right_expr) = right_expr {
                    $next_layer_function_name(right_expr, variable_move_info, info);
                }
            }
        }
    };
}

collect_variable_info_op2!(
    collect_variable_info_or_expression,
    OrExpression,
    collect_variable_info_and_expression
);

collect_variable_info_op2!(
    collect_variable_info_and_expression,
    AndExpression,
    collect_variable_info_compare_expression
);

collect_variable_info_op2!(
    collect_variable_info_compare_expression,
    CompareExpression,
    collect_variable_info_add_or_sub_expression
);

collect_variable_info_op2!(
    collect_variable_info_add_or_sub_expression,
    AddOrSubExpression,
    collect_variable_info_mul_or_div_expression
);

collect_variable_info_op2!(
    collect_variable_info_mul_or_div_expression,
    MulOrDivExpression,
    collect_variable_info_factor
);

fn collect_variable_info_factor(
    ast: &Factor,
    variable_move_info: &VariableUserInfo,
    info: &mut Vec<VariableInfo>,
) {
    if let Ok(primary) = &ast.primary {
        collect_variable_info_primary(primary, variable_move_info, info);
    }
}

fn collect_variable_info_primary(
    ast: &Primary,
    variable_move_info: &VariableUserInfo,
    info: &mut Vec<VariableInfo>,
) {
    collect_variable_info_primary_left(&ast.left, variable_move_info, info);

    for primary_right in ast.chain.iter() {
        collect_variable_info_primary_right(primary_right, variable_move_info, info);
    }
}

fn collect_variable_info_primary_left(
    ast: &PrimaryLeft,
    variable_move_info: &VariableUserInfo,
    info: &mut Vec<VariableInfo>,
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
                    collect_variable_info_expression(*expression, variable_move_info, info);
                }
            }

            if let Some(function_call) = function_call {
                collect_variable_info_function_call(function_call, variable_move_info, info);
            }
        }
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => {
            if let Ok(expression) = new_array_init_expression.init_expression {
                collect_variable_info_expression(expression, variable_move_info, info);
            }

            if let Ok(expression) = new_array_init_expression.length_expression {
                collect_variable_info_expression(expression, variable_move_info, info);
            }
        }
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => {
            for expression in new_array_expression.value_expressions.iter() {
                if let Ok(expression) = expression {
                    collect_variable_info_expression(*expression, variable_move_info, info);
                }
            }
        }
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Ok(field_assigns) = &new_expression.field_assigns {
                for field_assign in field_assigns.iter() {
                    if let Ok(expression) = field_assign.expression {
                        collect_variable_info_expression(expression, variable_move_info, info);
                    }
                }
            }
        }
        PrimaryLeftExpr::IfExpression(if_expression) => {
            if let Ok(expression) = if_expression.if_statement.condition {
                collect_variable_info_expression(expression, variable_move_info, info);
            }
            if let Some(block) = &if_expression.if_statement.block.value {
                collect_variable_info_program(block.program, variable_move_info, info);
            }

            for chain in if_expression.chain.iter() {
                if let Some(else_if_or_else) = &chain.else_if_or_else.value {
                    match else_if_or_else {
                        Either::Left(if_statement) => {
                            if let Ok(expression) = if_statement.condition {
                                collect_variable_info_expression(
                                    expression,
                                    variable_move_info,
                                    info,
                                );
                            }

                            if let Some(block) = &if_statement.block.value {
                                collect_variable_info_program(
                                    block.program,
                                    variable_move_info,
                                    info,
                                );
                            }
                        }
                        Either::Right(block) => {
                            collect_variable_info_program(block.program, variable_move_info, info);
                        }
                    }
                }
            }
        }
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                collect_variable_info_program(block.program, variable_move_info, info);
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_variable_info_mapping_operator(mapping_operator, variable_move_info, info);
    }
}

fn collect_variable_info_primary_right(
    ast: &PrimaryRight,
    variable_move_info: &VariableUserInfo,
    info: &mut Vec<VariableInfo>,
) {
    if let Some((_, _, function_call)) = &ast.second_expr {
        if let Some(function_call) = function_call {
            collect_variable_info_function_call(function_call, variable_move_info, info);
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_variable_info_mapping_operator(mapping_operator, variable_move_info, info);
    }
}

fn collect_variable_info_function_call(
    ast: &FunctionCall,
    variable_move_info: &VariableUserInfo,
    info: &mut Vec<VariableInfo>,
) {
    if let Ok(args) = &ast.arg_exprs {
        for expression in args.iter() {
            collect_variable_info_expression(*expression, variable_move_info, info);
        }
    }
}

fn collect_variable_info_mapping_operator(
    ast: &MappingOperator,
    variable_move_info: &VariableUserInfo,
    info: &mut Vec<VariableInfo>,
) {
    let block = match &ast.value {
        MappingOperatorKind::NullElvisBlock(recovered) => &recovered.value,
        MappingOperatorKind::ResultElvisBlock(recovered) => &recovered.value,
        _ => return,
    };

    if let Some(block) = block {
        collect_variable_info_program(block.program, variable_move_info, info);
    }
}
