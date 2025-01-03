use std::ops::Range;

use allocator_api2::vec;
use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_parser::{
    lexer::Token,
    parser::{
        ASTParseError, AddOrSubExpression, AndExpression, ArrayTypeInfo, BaseTypeInfo, Block,
        CompareExpression, Expression, ExpressionEnum, Factor, FieldAssign, FunctionCall, Generics,
        GenericsDefine, IfStatement, MappingOperatorKind, MulOrDivExpression, ParseResult, Primary,
        PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, Recovered, SimplePrimary,
        StatementAST, TranspilerTag, TupleTypeInfo, TypeAttributeEnum, TypeInfo, TypeTag,
        WhereClause, WhereElement,
    },
};
use either::Either::{self, Left, Right};

use self::{
    misc::{unexpected_token_error, Expected, UnexpectedTokens},
    statement::{not_separated_statement_error_1, statement_attributes_without_define},
};

use super::{advice::Advice, context::TranspileModuleContext, TranspileError, TranspileWarning};

pub mod misc;
pub mod statement;

pub fn collect_parse_error_program(
    ast: Program,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    let mut statement_errors = Vec::new();
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            Err(error) => {
                statement_errors.push(error);
                continue;
            }
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                collect_parse_error_expression(&assignment.left_expr, errors, warnings, context);
                collect_parse_error_with_parse_result(
                    &assignment.right_expr,
                    collect_parse_error_expression,
                    Expected::Expression,
                    0005,
                    errors,
                    warnings,
                    context,
                );
            }
            StatementAST::Exchange(exchange) => {
                collect_parse_error_expression(&exchange.left_expr, errors, warnings, context);
                collect_parse_error_with_parse_result(
                    &exchange.right_expr,
                    collect_parse_error_expression,
                    Expected::Expression,
                    0006,
                    errors,
                    warnings,
                    context,
                );
            }
            StatementAST::Import(import) => {
                collect_error_tokens(
                    &import.elements.error_tokens,
                    Expected::ImportElements,
                    0007,
                    errors,
                    context,
                );
                collect_parse_error_only_parse_result_error(
                    &import.elements.brace_right,
                    Expected::BraceRight,
                    0007,
                    errors,
                    context,
                );
            }
            StatementAST::StatementAttributes(statement_attributes) => {
                errors.push(statement_attributes_without_define(statement_attributes));
            }
            StatementAST::VariableDefine(variable_define) => {
                collect_parse_error_only_parse_result_error(
                    &variable_define.binding,
                    Expected::VariableName,
                    0008,
                    errors,
                    context,
                );
                if let Some(expression) = &variable_define.expression {
                    collect_parse_error_with_parse_result(
                        expression,
                        collect_parse_error_expression,
                        Expected::Expression,
                        0008,
                        errors,
                        warnings,
                        context,
                    );
                }
            }
            StatementAST::FunctionDefine(function_define) => {
                if let Some(generics_define) = &function_define.generics_define {
                    collect_parse_error_generics_define(generics_define, errors, warnings, context);
                }
                collect_parse_error_only_parse_result_error(
                    &function_define.name,
                    Expected::FunctionName,
                    0009,
                    errors,
                    context,
                );

                let arguments = &function_define.args;
                collect_parse_error_only_parse_result_error(
                    &arguments.paren_left,
                    Expected::ParenthesisLeft,
                    0009,
                    errors,
                    context,
                );
                if let Some(this_mutability) = &arguments.this_mutability {
                    collect_parse_error_only_parse_result_error(
                        &this_mutability.this_span,
                        Expected::This,
                        0054,
                        errors,
                        context,
                    );
                }
                for argument in arguments.arguments.iter() {
                    collect_parse_error_type_tag(&argument.type_tag, errors, warnings, context);
                }

                collect_error_tokens(
                    &arguments.error_tokens,
                    Expected::FunctionArgument,
                    0009,
                    errors,
                    context,
                );

                collect_parse_error_only_parse_result_error(
                    &arguments.paren_right,
                    Expected::ParenthesisRight,
                    0009,
                    errors,
                    context,
                );

                if let Some(type_tag) = &function_define.type_tag {
                    collect_parse_error_type_tag(type_tag, errors, warnings, context);
                }

                if let Some(where_clause) = &function_define.where_clause {
                    collect_parse_error_where_clause(where_clause, errors, warnings, context);
                }

                collect_parse_error_with_recovered(
                    &function_define.block_or_semicolon,
                    collect_parse_error_block_or_semicolon,
                    Expected::Block,
                    0009,
                    errors,
                    warnings,
                    context,
                );
            }
            StatementAST::UserTypeDefine(data_struct_define) => {
                collect_parse_error_only_parse_result_error(
                    &data_struct_define.name,
                    Expected::UserTypeName,
                    0010,
                    errors,
                    context,
                );

                if let Some(generics_define) = &data_struct_define.generics_define {
                    collect_parse_error_generics_define(generics_define, errors, warnings, context);
                }

                if let Some(super_type_info) = &data_struct_define.super_type_info {
                    for type_info in super_type_info.type_infos.iter() {
                        collect_parse_error_type_info(type_info, errors, warnings, context);
                    }
                    collect_error_tokens(
                        &super_type_info.error_tokens,
                        Expected::SuperTypeInfo,
                        0010,
                        errors,
                        context,
                    );
                }

                if let Some(where_clause) = &data_struct_define.where_clause {
                    collect_parse_error_where_clause(where_clause, errors, warnings, context);
                }

                collect_error_tokens(
                    &data_struct_define.error_tokens,
                    Expected::Unnecessary,
                    0010,
                    errors,
                    context,
                );

                collect_parse_error_with_recovered(
                    &data_struct_define.block,
                    collect_parse_error_block,
                    Expected::Block,
                    0010,
                    errors,
                    warnings,
                    context,
                );
            }
            StatementAST::TypeDefine(type_define) => {
                collect_parse_error_only_parse_result_error(
                    &type_define.name,
                    Expected::UserTypeName,
                    0053,
                    errors,
                    context,
                );

                if let Some(generics_define) = &type_define.generics_define {
                    collect_parse_error_generics_define(generics_define, errors, warnings, context);
                }

                collect_parse_error_with_parse_result(
                    &type_define.type_info,
                    collect_parse_error_type_info,
                    Expected::TypeInfo,
                    0053,
                    errors,
                    warnings,
                    context,
                );

                for error_tokens in type_define.error_tokens.iter() {
                    collect_error_tokens(
                        error_tokens,
                        Expected::Unnecessary,
                        0053,
                        errors,
                        context,
                    );
                }
            }
            StatementAST::Implements(implements) => {
                if let Some(generics_define) = &implements.generics_define {
                    collect_parse_error_generics_define(generics_define, errors, warnings, context);
                }

                collect_parse_error_with_parse_result(
                    &implements.interface,
                    collect_parse_error_type_info,
                    Expected::TypeInfo,
                    0045,
                    errors,
                    warnings,
                    context,
                );
                collect_parse_error_with_parse_result(
                    &implements.target_user_type,
                    collect_parse_error_type_info,
                    Expected::TypeInfo,
                    0045,
                    errors,
                    warnings,
                    context,
                );
                if let Some(where_clause) = &implements.where_clause {
                    collect_parse_error_where_clause(where_clause, errors, warnings, context);
                }
                collect_parse_error_with_recovered(
                    &implements.block,
                    collect_parse_error_block,
                    Expected::Block,
                    0045,
                    errors,
                    warnings,
                    context,
                );
            }
            StatementAST::DropStatement(drop_statement) => {
                collect_parse_error_with_parse_result(
                    &drop_statement.expression,
                    collect_parse_error_expression,
                    Expected::Expression,
                    0011,
                    errors,
                    warnings,
                    context,
                );
            }
            StatementAST::Expression(expression) => {
                collect_parse_error_expression(expression, errors, warnings, context);
            }
            StatementAST::TranspilerTag(transpiler_tag) => {
                collect_parse_error_transpiler_tag(transpiler_tag, errors, context);
            }
        }
    }

    for token in ast.not_separated_stmts.iter() {
        errors.push(not_separated_statement_error_1(token, context));
    }

    errors.extend(unexpected_token_error(
        &statement_errors,
        Expected::Statement,
        0002,
        context,
    ));
}

fn collect_parse_error_transpiler_tag(
    ast: &TranspilerTag,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    collect_parse_error_only_parse_result_error(
        &ast.literal,
        Expected::TagName,
        0081,
        errors,
        context,
    );
    collect_error_tokens(
        &ast.error_tokens,
        Expected::BracketRight,
        0081,
        errors,
        context,
    );
    collect_parse_error_only_parse_result_error(
        &ast.bracket_right,
        Expected::BracketRight,
        0081,
        errors,
        context,
    );
}

fn collect_parse_error_block_or_semicolon(
    ast: &Either<Range<usize>, Block>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    if let Either::Right(block) = ast {
        collect_parse_error_block(block, errors, warnings, context);
    }
}

fn collect_parse_error_generics_define(
    ast: &GenericsDefine,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_error_tokens(
        &ast.error_tokens,
        Expected::Unnecessary,
        0022,
        errors,
        context,
    );
    collect_parse_error_only_parse_result_error(
        &ast.greater_than,
        Expected::GreaterThan,
        0022,
        errors,
        context,
    );

    for element in ast.elements.iter() {
        for bound_type_info in element.bounds.iter() {
            collect_parse_error_type_info(bound_type_info, errors, warnings, context);
        }
    }
}

fn collect_parse_error_where_clause(
    ast: &WhereClause,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    for element in ast.elements.iter() {
        collect_parse_error_where_element(element, errors, warnings, context);
    }
    for error_tokens in ast.error_tokens.iter() {
        collect_error_tokens(error_tokens, Expected::Unnecessary, 0049, errors, context);
    }
    collect_parse_error_only_parse_result_error(
        &ast.next_expected_token,
        Expected::BraceLeft,
        0049,
        errors,
        context,
    );
}

fn collect_parse_error_where_element(
    ast: &WhereElement,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_parse_error_type_info(&ast.target_type, errors, warnings, context);
    for bound in ast.bounds.iter() {
        collect_parse_error_type_info(bound, errors, warnings, context);
    }
}

fn collect_parse_error_block(
    ast: &Block,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_parse_error_program(&ast.program, errors, warnings, context);
    collect_parse_error_only_parse_result_error(
        &ast.brace_right,
        Expected::BraceRight,
        0012,
        errors,
        context,
    );
}

fn collect_parse_error_expression(
    ast: &Expression,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            collect_parse_error_and_expression(&or_expression.left_expr, errors, warnings, context);
            for expression in or_expression.right_exprs.iter() {
                collect_parse_error_with_parse_result(
                    &expression.1,
                    collect_parse_error_and_expression,
                    Expected::Expression,
                    0013,
                    errors,
                    warnings,
                    context,
                );
            }
        }
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = &return_expression.expression {
                collect_parse_error_expression(expression, errors, warnings, context);
            }
        }
        ExpressionEnum::Closure(closure) => {
            let arguments = &closure.arguments;
            if let Right(arguments) = &arguments.arguments {
                for argument in arguments {
                    if let Left(function_argument) = argument {
                        collect_parse_error_type_tag(
                            &function_argument.type_tag,
                            errors,
                            warnings,
                            context,
                        );
                    }
                }
            }
            collect_error_tokens(
                &arguments.error_tokens,
                Expected::ClosureArgument,
                0014,
                errors,
                context,
            );
            collect_parse_error_only_parse_result_error(
                &arguments.vertical_bar_right,
                Expected::VerticalBarRight,
                0014,
                errors,
                context,
            );

            let mut expected = Expected::ExpressionOrBlock;

            if closure.fat_arrow_span.is_ok() {
                collect_error_tokens(
                    &closure.error_tokens,
                    Expected::Unnecessary,
                    0014,
                    errors,
                    context,
                );
            } else {
                if closure.error_tokens.is_empty() {
                    let span_start = closure.arguments.span.end;
                    let span_end = context.source_code.code.len().min(span_start + 1);

                    if closure.expression_or_block.result.is_err()
                        || closure.expression_or_block.value.is_none()
                    {
                        expected = Expected::FatArrowAndBlock;
                    } else {
                        let mut error = TranspileError::new(UnexpectedTokens {
                            span: span_start..span_end,
                            error_code: 0014,
                            expected: Expected::FatArrow,
                        });
                        let advice = Advice::Add {
                            add: "=>".to_string(),
                            position: span_start,
                            message_override: None,
                        };
                        error.add_advice(context.module_name.clone(), advice);
                        errors.push(error);
                    }
                }
            }

            collect_parse_error_with_recovered(
                &closure.expression_or_block,
                collect_parse_error_expression_or_block,
                expected,
                0014,
                errors,
                warnings,
                context,
            );
        }
    }
}

fn collect_parse_error_expression_or_block(
    ast: &Either<Expression, Block>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    match ast {
        Left(expression) => collect_parse_error_expression(expression, errors, warnings, context),
        Right(block) => collect_parse_error_block(block, errors, warnings, context),
    }
}

fn collect_parse_error_and_expression(
    ast: &AndExpression,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_parse_error_compare_expression(&ast.left_expr, errors, warnings, context);
    for expression in ast.right_exprs.iter() {
        collect_parse_error_with_parse_result(
            &expression.1,
            collect_parse_error_compare_expression,
            Expected::Expression,
            0013,
            errors,
            warnings,
            context,
        );
    }
}

fn collect_parse_error_compare_expression(
    ast: &CompareExpression,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_parse_error_add_or_sub_expression(&ast.left_expr, errors, warnings, context);
    for expression in ast.right_exprs.iter() {
        collect_parse_error_with_parse_result(
            &expression.1,
            collect_parse_error_add_or_sub_expression,
            Expected::Expression,
            0013,
            errors,
            warnings,
            context,
        );
    }
}

fn collect_parse_error_add_or_sub_expression(
    ast: &AddOrSubExpression,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_parse_error_mul_or_div_expression(&ast.left_expr, errors, warnings, context);
    for expression in ast.right_exprs.iter() {
        collect_parse_error_with_parse_result(
            &expression.1,
            collect_parse_error_mul_or_div_expression,
            Expected::Expression,
            0013,
            errors,
            warnings,
            context,
        );
    }
}

fn collect_parse_error_mul_or_div_expression(
    ast: &MulOrDivExpression,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_parse_error_factor(&ast.left_expr, errors, warnings, context);
    for expression in ast.right_exprs.iter() {
        collect_parse_error_with_parse_result(
            &expression.1,
            collect_parse_error_factor,
            Expected::Expression,
            0013,
            errors,
            warnings,
            context,
        );
    }
}

fn collect_parse_error_factor(
    ast: &Factor,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_parse_error_with_parse_result(
        &ast.primary,
        collect_parse_error_primary,
        Expected::Expression,
        0013,
        errors,
        warnings,
        context,
    );
}

fn collect_parse_error_primary(
    ast: &Primary,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_parse_error_primary_left(&ast.left, errors, warnings, context);
    for chain in ast.chain.iter() {
        collect_parse_error_primary_right(chain, errors, warnings, context);
    }
}

fn collect_parse_error_primary_left(
    ast: &PrimaryLeft,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple_primary) => {
            if let SimplePrimary::Expressions {
                expressions,
                error_tokens,
                span: _,
            } = &simple_primary.0
            {
                for expression in expressions.iter() {
                    collect_parse_error_expression(expression, errors, warnings, context);
                }
                for error_tokens in error_tokens.iter() {
                    collect_error_tokens(
                        error_tokens,
                        Expected::Unnecessary,
                        0013,
                        errors,
                        context,
                    );
                }
            }
            if let Some(generics) = &simple_primary.1 {
                collect_parse_error_with_parse_result(
                    generics,
                    collect_parse_error_generics,
                    Expected::Generics,
                    0015,
                    errors,
                    warnings,
                    context,
                );
            }
            if let Some(function_call) = &simple_primary.2 {
                collect_parse_error_function_call(function_call, errors, warnings, context);
            }
        }
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => {
            collect_parse_error_with_parse_result(
                &new_array_init_expression.init_expression,
                collect_parse_error_expression,
                Expected::Expression,
                0064,
                errors,
                warnings,
                context,
            );
            collect_parse_error_only_parse_result_error(
                &new_array_init_expression.semicolon,
                Expected::Semicolon,
                0064,
                errors,
                context,
            );
            collect_parse_error_with_parse_result(
                &new_array_init_expression.length_expression,
                collect_parse_error_expression,
                Expected::Expression,
                0064,
                errors,
                warnings,
                context,
            );
            collect_parse_error_only_parse_result_error(
                &new_array_init_expression.bracket_right,
                Expected::BraceRight,
                0064,
                errors,
                context,
            );
            collect_error_tokens(
                &new_array_init_expression.error_tokens,
                Expected::Unnecessary,
                0064,
                errors,
                context,
            );
        }
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => {
            for value_expression in new_array_expression.value_expressions.iter() {
                collect_parse_error_with_parse_result(
                    value_expression,
                    collect_parse_error_expression,
                    Expected::Expression,
                    0064,
                    errors,
                    warnings,
                    context,
                );
            }
            for error_tokens in new_array_expression.error_tokens.iter() {
                collect_error_tokens(error_tokens, Expected::Expression, 0064, errors, context);
            }
        }
        PrimaryLeftExpr::NewExpression(new_expression) => {
            for error_tokens in new_expression.error_tokens.iter() {
                collect_error_tokens(error_tokens, Expected::Unnecessary, 0016, errors, context);
            }
            collect_parse_error_with_parse_result(
                &new_expression.field_assigns,
                collect_parse_error_field_assigns,
                Expected::FieldAssign,
                0016,
                errors,
                warnings,
                context,
            );
        }
        PrimaryLeftExpr::IfExpression(if_expression) => {
            collect_parse_error_if_statement(
                &if_expression.if_statement,
                errors,
                warnings,
                context,
            );
            for if_or_else in if_expression.chain.iter() {
                collect_parse_error_with_recovered(
                    &if_or_else.else_if_or_else,
                    collect_parse_error_if_statement_or_block,
                    Expected::IfStatementOrBlock,
                    0017,
                    errors,
                    warnings,
                    context,
                );
            }
        }
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            collect_parse_error_with_parse_result(
                &loop_expression.block,
                collect_parse_error_block,
                Expected::Block,
                0018,
                errors,
                warnings,
                context,
            );
        }
    }
}

fn collect_parse_error_field_assigns(
    ast: &Vec<FieldAssign, &Bump>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    for field_assign in ast.iter() {
        collect_parse_error_with_parse_result(
            &field_assign.expression,
            collect_parse_error_expression,
            Expected::Expression,
            0016,
            errors,
            warnings,
            context,
        );
    }
}

fn collect_parse_error_function_call(
    ast: &FunctionCall,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_error_tokens(
        &ast.error_tokens,
        Expected::Unnecessary,
        0015,
        errors,
        context,
    );
    collect_parse_error_with_parse_result(
        &ast.arg_exprs,
        collect_parse_error_argument_expr,
        Expected::ArgumentExpression,
        0015,
        errors,
        warnings,
        context,
    );
}

fn collect_parse_error_argument_expr(
    ast: &Vec<Expression, &Bump>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    for expression in ast.iter() {
        collect_parse_error_expression(expression, errors, warnings, context);
    }
}

fn collect_parse_error_if_statement(
    ast: &IfStatement,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_parse_error_with_parse_result(
        &ast.condition,
        collect_parse_error_expression,
        Expected::Expression,
        0017,
        errors,
        warnings,
        context,
    );
    collect_parse_error_with_recovered(
        &ast.block,
        collect_parse_error_block,
        Expected::Block,
        0017,
        errors,
        warnings,
        context,
    );
}

fn collect_parse_error_if_statement_or_block(
    ast: &Either<IfStatement, Block>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    match ast {
        Left(if_statement) => {
            collect_parse_error_if_statement(if_statement, errors, warnings, context)
        }
        Right(block) => collect_parse_error_block(block, errors, warnings, context),
    }
}

fn collect_parse_error_primary_right(
    ast: &PrimaryRight,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    if let Some(second_expr) = &ast.second_expr {
        if let Some(generics) = &second_expr.1 {
            collect_parse_error_with_parse_result(
                generics,
                collect_parse_error_generics,
                Expected::Generics,
                0015,
                errors,
                warnings,
                context,
            );
        }
        if let Some(function_call) = &second_expr.2 {
            collect_parse_error_function_call(function_call, errors, warnings, context);
        }
    }
    if let Some(mapping_operator) = &ast.mapping_operator {
        let block = match &mapping_operator.value {
            MappingOperatorKind::NullElvisBlock(block) => block,
            MappingOperatorKind::ResultElvisBlock(block) => block,
            _ => return,
        };
        collect_parse_error_with_recovered(
            block,
            collect_parse_error_block,
            Expected::Block,
            0019,
            errors,
            warnings,
            context,
        );
    }
}

fn collect_parse_error_type_tag(
    ast: &TypeTag,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    collect_parse_error_with_parse_result(
        &ast.type_info,
        collect_parse_error_type_info,
        Expected::TypeInfo,
        0020,
        errors,
        warnings,
        context,
    );
}

fn collect_parse_error_type_info(
    ast: &TypeInfo,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    match ast {
        TypeInfo::BaseType(base_type_info) => {
            collect_parse_error_base_type_info(base_type_info, errors, warnings, context);
        }
        TypeInfo::ArrayType(array_type_info) => {
            collect_parse_error_array_type_info(array_type_info, errors, warnings, context);
        }
        TypeInfo::TupleType(tuple_type_info) => {
            collect_parse_error_tuple_type_info(tuple_type_info, errors, warnings, context);
        }
    }
}

fn collect_parse_error_base_type_info(
    ast: &BaseTypeInfo,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    if let Some(generics) = &ast.generics {
        collect_parse_error_generics(generics, errors, warnings, context);
    }
    for attribute in ast.type_attributes.iter() {
        if let TypeAttributeEnum::Result(generics) = &attribute.value {
            if let Some(generics) = generics {
                collect_parse_error_generics(generics, errors, warnings, context);
            }
        }
    }
}

fn collect_parse_error_array_type_info(
    ast: &ArrayTypeInfo,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    match &ast.type_info {
        Ok(type_info) => {
            collect_parse_error_type_info(*type_info, errors, warnings, context);
        }
        Err(error) => errors.extend(unexpected_token_error(
            &vec![error],
            Expected::TypeInfo,
            0065,
            context,
        )),
    }
    collect_error_tokens(
        &ast.error_tokens,
        Expected::BraceRight,
        0065,
        errors,
        context,
    );
    collect_parse_error_only_parse_result_error(
        &ast.bracket_right,
        Expected::BraceRight,
        0065,
        errors,
        context,
    );
}

fn collect_parse_error_tuple_type_info(
    ast: &TupleTypeInfo,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    for type_info in ast.types.iter() {
        collect_parse_error_type_info(type_info, errors, warnings, context);
    }
    for error_tokens in ast.error_tokens.iter() {
        collect_error_tokens(error_tokens, Expected::TypeInfo, 0077, errors, context)
    }
}

fn collect_parse_error_generics(
    ast: &Generics,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    for element in ast.elements.iter() {
        collect_parse_error_type_info(element, errors, warnings, context);
    }
    collect_error_tokens(
        &ast.error_tokens,
        Expected::Unnecessary,
        0021,
        errors,
        context,
    );
}

fn collect_parse_error_with_parse_result<T>(
    ast: &ParseResult<T>,
    error_collector: fn(
        ast: &T,
        errors: &mut Vec<TranspileError>,
        warnings: &mut Vec<TranspileWarning>,
        context: &TranspileModuleContext,
    ),
    expected: Expected,
    error_code: usize,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    match ast {
        Ok(ast) => error_collector(ast, errors, warnings, context),
        Err(error) => {
            errors.extend(unexpected_token_error(
                &vec![error],
                expected,
                error_code,
                context,
            ));
        }
    }
}

fn collect_parse_error_with_recovered<T>(
    recovered: &Recovered<T>,
    error_collector: fn(
        ast: &T,
        errors: &mut Vec<TranspileError>,
        warnings: &mut Vec<TranspileWarning>,
        context: &TranspileModuleContext,
    ),
    expected: Expected,
    error_code: usize,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    let expected = if recovered.value.is_some() {
        Expected::Unnecessary
    } else {
        expected
    };
    if let Err(error) = &recovered.result {
        errors.extend(unexpected_token_error(
            &vec![error],
            expected,
            error_code,
            context,
        ));
    }

    if let Some(ast) = &recovered.value {
        error_collector(ast, errors, warnings, context);
    }
}

fn collect_parse_error_only_parse_result_error<T>(
    result: &ParseResult<T>,
    expected: Expected,
    error_code: usize,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    match result {
        Err(error) => errors.extend(unexpected_token_error(
            &vec![error],
            expected,
            error_code,
            context,
        )),
        _ => {}
    }
}

fn collect_error_tokens(
    error_tokens: &Vec<Token, &Bump>,
    expected: Expected,
    error_code: usize,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    if !error_tokens.is_empty() {
        let error = ASTParseError::UnexpectedToken(error_tokens.clone());
        errors.extend(unexpected_token_error(
            &vec![&error],
            expected,
            error_code,
            context,
        ));
    }
}
