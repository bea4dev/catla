use catla_parser::{parser::{ASTParseError, AddOrSubExpression, AndExpression, Block, CompareExpression, EQNEExpression, Expression, ExpressionEnum, Factor, FunctionCall, Generics, IfStatement, MappingOperatorKind, MulOrDivExpression, ParseResult, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, Recovered, SimplePrimary, StatementAST, TypeAttributeEnum, TypeInfo, TypeTag}, lexer::Token};
use either::Either::{Right, Left, self};

use self::{statement::{not_separated_statement_error_1, statement_attributes_without_define}, misc::{Expected, unexpected_token_error, UnexpectedTokens}};

use super::{TranspileError, TranspileWarning};

pub mod statement;
pub mod misc;



pub fn collect_parse_error_program(ast: Program, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
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
                collect_parse_error_expression(&assignment.left_expr, errors, warnings);
                collect_parse_error_with_parse_result(
                    &assignment.right_expr,
                    collect_parse_error_expression,
                    Expected::Expression,
                    0005,
                    errors,
                    warnings
                );
            },
            StatementAST::Exchange(exchange) => {
                collect_parse_error_expression(&exchange.left_expr, errors, warnings);
                collect_parse_error_with_parse_result(
                    &exchange.right_expr,
                    collect_parse_error_expression,
                    Expected::Expression,
                    0006,
                    errors,
                    warnings
                );
            },
            StatementAST::Import(import) => {
                collect_error_tokens(&import.elements.error_tokens, Expected::ImportElements, 0007, errors);
                collect_parse_error_only_parse_result_error(&import.elements.brace_right, Expected::BraceRight, 0007, errors);
            },
            StatementAST::StatementAttributes(statement_attributes) => {
                errors.push(statement_attributes_without_define(statement_attributes));
            },
            StatementAST::VariableDefine(variable_define) => {
                collect_parse_error_only_parse_result_error(&variable_define.name, Expected::VariableName, 0008, errors);
                if let Some(expression) = &variable_define.expression {
                    collect_parse_error_with_parse_result(expression, collect_parse_error_expression, Expected::Expression, 0008, errors, warnings);
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                if let Some(generics) = &function_define.generics {
                    collect_parse_error_generics(generics, errors, warnings);
                }
                collect_parse_error_only_parse_result_error(&function_define.name, Expected::FunctionName, 0009, errors);
                
                let arguments = &function_define.args;
                collect_parse_error_only_parse_result_error(&arguments.paren_left, Expected::ParenthesisLeft, 0009, errors);
                for argument in arguments.arguments.iter() {
                    collect_parse_error_type_tag(&argument.type_tag, errors, warnings);
                }
                
                collect_error_tokens(&arguments.error_tokens, Expected::FunctionArgument, 0009, errors);

                collect_parse_error_only_parse_result_error(&arguments.paren_right, Expected::ParenthesisRight, 0009, errors);

                if let Some(type_tag) = &function_define.type_tag {
                    collect_parse_error_type_tag(type_tag, errors, warnings);
                }

                collect_parse_error_with_recovered(&function_define.block, collect_parse_error_block, Expected::Block, 0009, errors, warnings);
            },
            StatementAST::DataStructDefine(data_struct_define) => {
                collect_parse_error_only_parse_result_error(&data_struct_define.name, Expected::DataStructName, 0010, errors);

                if let Some(generics) = &data_struct_define.generics {
                    collect_parse_error_generics(generics, errors, warnings);
                }

                if let Some(extends) = &data_struct_define.extends {
                    collect_parse_error_with_parse_result(&extends.type_info, collect_parse_error_type_info, Expected::ExtendsType, 0010, errors, warnings);
                }

                if let Some(implements) = &data_struct_define.implements {
                    for type_info in implements.type_infos.iter() {
                        collect_parse_error_type_info(type_info, errors, warnings);
                    }
                    collect_error_tokens(&implements.error_tokens, Expected::ImplementsType, 0010, errors);
                }

                collect_error_tokens(&data_struct_define.error_tokens, Expected::Unnecessary, 0010, errors);

                collect_parse_error_with_recovered(&data_struct_define.block, collect_parse_error_block, Expected::Block, 0010, errors, warnings);
            },
            StatementAST::DropStatement(drop_statement) => {
                collect_parse_error_with_parse_result(&drop_statement.expression, collect_parse_error_expression, Expected::Expression, 0011, errors, warnings);
            },
            StatementAST::Expression(expression) => {
                collect_parse_error_expression(expression, errors, warnings);
            }
        }
    }

    for token in ast.not_separated_stmts.iter() {
        errors.push(not_separated_statement_error_1(token))
    }

    errors.extend(unexpected_token_error(&statement_errors, Expected::Statement, 0002));
}

fn collect_parse_error_block(ast: &Block, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    collect_parse_error_program(&ast.program, errors, warnings);
    collect_parse_error_only_parse_result_error(&ast.brace_right, Expected::BraceRight, 0012, errors);
}

fn collect_parse_error_expression(ast: &Expression, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            collect_parse_error_and_expression(&or_expression.left_expr, errors, warnings);
            for expression in or_expression.right_exprs.iter() {
                collect_parse_error_with_parse_result(&expression.1, collect_parse_error_and_expression, Expected::Expression, 0013, errors, warnings);
            }
        },
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = &return_expression.expression {
                collect_parse_error_expression(expression, errors, warnings);
            }
        },
        ExpressionEnum::Closure(closure) => {
            let arguments = &closure.arguments;
            if let Right(arguments) = &arguments.arguments {
                for argument in arguments {
                    if let Left(function_argument) = argument {
                        collect_parse_error_type_tag(&function_argument.type_tag, errors, warnings);
                    }
                }
            }
            collect_error_tokens(&arguments.error_tokens, Expected::ClosureArgument, 0014, errors);
            collect_parse_error_only_parse_result_error(&arguments.vertical_bar_right, Expected::VerticalBarRight, 0014, errors);

            if closure.fat_arrow_span.is_ok() {
                collect_error_tokens(&closure.error_tokens, Expected::Unnecessary, 0014, errors);
            } else {
                if closure.error_tokens.is_empty() {
                    let error = UnexpectedTokens { span: closure.span.clone(), error_code: 0014, expected: Expected::FatArrow };
                    errors.push(TranspileError::new(error));
                }
            }

            collect_parse_error_with_recovered(
                &closure.expression_or_block,
                collect_parse_error_expression_or_block,
                Expected::ExpressionOrBlock,
                0014,
                errors,
                warnings
            );
        }
    }
}

fn collect_parse_error_expression_or_block(ast: &Either<Expression, Block>, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    match ast {
        Left(expression) => collect_parse_error_expression(expression, errors, warnings),
        Right(block) => collect_parse_error_block(block, errors, warnings)
    }
}

fn collect_parse_error_and_expression(ast: &AndExpression, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    collect_parse_error_eqne_expression(&ast.left_expr, errors, warnings);
    for expression in ast.right_exprs.iter() {
        collect_parse_error_with_parse_result(&expression.1, collect_parse_error_eqne_expression, Expected::Expression, 0013, errors, warnings);
    }
}

fn collect_parse_error_eqne_expression(ast: &EQNEExpression, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    collect_parse_error_compare_expression(&ast.left_expr, errors, warnings);
    for expression in ast.right_exprs.iter() {
        collect_parse_error_with_parse_result(&expression.1, collect_parse_error_compare_expression, Expected::Expression, 0013, errors, warnings);
    }
}

fn collect_parse_error_compare_expression(ast: &CompareExpression, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    collect_parse_error_add_or_sub_expression(&ast.left_expr, errors, warnings);
    for expression in ast.right_exprs.iter() {
        collect_parse_error_with_parse_result(&expression.1, collect_parse_error_add_or_sub_expression, Expected::Expression, 0013, errors, warnings);
    }
}

fn collect_parse_error_add_or_sub_expression(ast: &AddOrSubExpression, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    collect_parse_error_mul_or_div_expression(&ast.left_expr, errors, warnings);
    for expression in ast.right_exprs.iter() {
        collect_parse_error_with_parse_result(&expression.1, collect_parse_error_mul_or_div_expression, Expected::Expression, 0013, errors, warnings);
    }
}

fn collect_parse_error_mul_or_div_expression(ast: &MulOrDivExpression, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    collect_parse_error_factor(&ast.left_expr, errors, warnings);
    for expression in ast.right_exprs.iter() {
        collect_parse_error_with_parse_result(&expression.1, collect_parse_error_factor, Expected::Expression, 0013, errors, warnings);
    }
}

fn collect_parse_error_factor(ast: &Factor, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    collect_parse_error_with_parse_result(&ast.primary, collect_parse_error_primary, Expected::Expression, 0013, errors, warnings);
}

fn collect_parse_error_primary(ast: &Primary, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    collect_parse_error_primary_left(&ast.left, errors, warnings);
    for chain in ast.chain.iter() {
        collect_parse_error_primary_right(chain, errors, warnings);
    }
}

fn collect_parse_error_primary_left(ast: &PrimaryLeft, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple_primary) => {
            if let SimplePrimary::Expression { expression, error_tokens } = &simple_primary.0 {
                collect_parse_error_with_parse_result(expression, collect_parse_error_expression, Expected::Expression, 0013, errors, warnings);
                collect_error_tokens(error_tokens, Expected::Unnecessary, 0013, errors);
            }
            if let Some(function_call) = &simple_primary.1 {
                collect_parse_error_function_call(function_call, errors, warnings);
            }
        },
        PrimaryLeftExpr::NewExpression(new_expression) => {
            collect_error_tokens(&new_expression.error_tokens, Expected::Unnecessary, 0016, errors);
            collect_parse_error_with_parse_result(&new_expression.function_call, collect_parse_error_function_call, Expected::FunctionCall, 0016, errors, warnings);
        },
        PrimaryLeftExpr::IfExpression(if_expression) => {
            collect_parse_error_if_statement(&if_expression.if_statement, errors, warnings);
            for if_or_else in if_expression.chain.iter() {
                collect_parse_error_with_recovered(
                    &if_or_else.else_if_or_else,
                    collect_parse_error_if_statement_or_block,
                    Expected::IfStatementOrBlock,
                    0017,
                    errors,
                    warnings
                );
            }
        },
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            collect_parse_error_with_parse_result(&loop_expression.block, collect_parse_error_block, Expected::Block, 0018, errors, warnings)
        }
    }
}

fn collect_parse_error_function_call(ast: &FunctionCall, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    if let Some(generics) = &ast.generics {
        collect_parse_error_with_parse_result(generics, collect_parse_error_generics, Expected::Generics, 0015, errors, warnings);
    }
    collect_error_tokens(&ast.error_tokens, Expected::Unnecessary, 0015, errors);
    collect_parse_error_with_parse_result(&ast.arg_exprs, collect_parse_error_argument_expr, Expected::ArgumentExpression, 0015, errors, warnings);
}

fn collect_parse_error_argument_expr(ast: &bumpalo::collections::Vec<Expression>, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    for expression in ast.iter() {
        collect_parse_error_expression(expression, errors, warnings);
    }
}

fn collect_parse_error_if_statement(ast: &IfStatement, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    collect_parse_error_with_parse_result(&ast.condition, collect_parse_error_expression, Expected::Expression, 0017, errors, warnings);
    collect_parse_error_with_recovered(&ast.block, collect_parse_error_block, Expected::Block, 0017, errors, warnings);
}

fn collect_parse_error_if_statement_or_block(ast: &Either<IfStatement, Block>, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    match ast {
        Left(if_statement) => collect_parse_error_if_statement(if_statement, errors, warnings),
        Right(block) => collect_parse_error_block(block, errors, warnings)
    }
}

fn collect_parse_error_primary_right(ast: &PrimaryRight, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    if let Some(second_expr) = &ast.second_expr {
        if let Some(function_call) = &second_expr.1 {
            collect_parse_error_function_call(function_call, errors, warnings);
        }
    }
    if let Some(mapping_operator) = &ast.mapping_operator {
        let block = match &mapping_operator.value {
            MappingOperatorKind::NullElvisBlock(block) => block,
            MappingOperatorKind::ResultElvisBlock(block) => block,
            _ => return
        };
        collect_parse_error_with_recovered(block, collect_parse_error_block, Expected::Block, 0019, errors, warnings)
    }
}

fn collect_parse_error_type_tag(ast: &TypeTag, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    collect_parse_error_with_parse_result(&ast.type_info, collect_parse_error_type_info, Expected::TypeInfo, 0020, errors, warnings);
}

fn collect_parse_error_type_info(ast: &TypeInfo, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    if let Some(generics) = &ast.generics {
        collect_parse_error_generics(generics, errors, warnings);
    }
    for attribute in ast.type_attributes.iter() {
        if let TypeAttributeEnum::Result(generics) = &attribute.value {
            if let Some(generics) = generics {
                collect_parse_error_generics(generics, errors, warnings);
            }
        }
    }
}

fn collect_parse_error_generics(ast: &Generics, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    for element in ast.elements.iter() {
        collect_parse_error_type_info(element, errors, warnings);
    }
    collect_error_tokens(&ast.error_tokens, Expected::Unnecessary, 0021, errors);
}

fn collect_parse_error_with_parse_result<T>(
    ast: &ParseResult<T>,
    error_collector: fn(ast: &T, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>),
    expected: Expected,
    error_code: usize,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    match ast {
        Ok(ast) => error_collector(ast, errors, warnings),
        Err(error) => errors.extend(unexpected_token_error(&vec![error], expected, error_code))
    }
}

fn collect_parse_error_with_recovered<T>(
    recovered: &Recovered<T>,
    error_collector: fn(ast: &T, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>),
    expected: Expected,
    error_code: usize,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    let expected = if recovered.value.is_some() { Expected::Unnecessary } else { expected };
    if let Err(error) = &recovered.result {
        errors.extend(unexpected_token_error(&vec![error], expected, error_code));
    }

    if let Some(ast) = &recovered.value {
        error_collector(ast, errors, warnings);
    }
}

fn collect_parse_error_only_parse_result_error<T>(result: &ParseResult<T>, expected: Expected, error_code: usize, errors: &mut Vec<TranspileError>) {
    match result {
        Err(error) => errors.extend(unexpected_token_error(&vec![error], expected, error_code)),
        _ => {}
    }
}

fn collect_error_tokens(error_tokens: &bumpalo::collections::Vec<Token>, expected: Expected, error_code: usize, errors: &mut Vec<TranspileError>) {
    if !error_tokens.is_empty() {
        let error = ASTParseError::UnexpectedToken(error_tokens.clone());
        errors.extend(unexpected_token_error(&vec![&error], expected, error_code));
    }
}


