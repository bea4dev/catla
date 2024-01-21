use catla_parser::parser::{Program, StatementAST, Expression, ParseResult, ASTParseError, Generics, TypeTag, Recovered, Block};

use self::{statement::{not_separated_statement_error_1, statement_attributes_without_define}, misc::{Expected, unexpected_token_error_2_or_error_3}};

use super::{TranspileError, TranspileWarning};

pub mod statement;
pub mod misc;



pub fn collect_parse_error_program(ast: Program, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    for token in ast.not_separated_stmts.iter() {
        errors.push(not_separated_statement_error_1(token))
    }
    
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
                    Expected::Expression, errors, warnings
                );
            },
            StatementAST::Exchange(exchange) => {
                collect_parse_error_expression(&exchange.left_expr, errors, warnings);
                collect_parse_error_with_parse_result(
                    &exchange.right_expr,
                    collect_parse_error_expression,
                    Expected::Expression,
                    errors,
                    warnings
                );
            },
            StatementAST::Import(import) => {
                let error = ASTParseError::UnexpectedToken(import.elements.error_tokens.clone());
                errors.extend(unexpected_token_error_2_or_error_3(&vec![&error], Expected::ImportElements));

                collect_parse_error_only_parse_result_error(&import.elements.brace_right, Expected::BraceRight, errors);
            },
            StatementAST::StatementAttributes(statement_attributes) => {
                errors.push(statement_attributes_without_define(statement_attributes));
            },
            StatementAST::VariableDefine(variable_define) => {
                collect_parse_error_only_parse_result_error(&variable_define.name, Expected::VariableName, errors);
                if let Some(expression) = &variable_define.expression {
                    collect_parse_error_with_parse_result(expression, collect_parse_error_expression, Expected::Expression, errors, warnings);
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                if let Some(generics) = &function_define.generics {
                    collect_parse_error_generics(generics, errors, warnings);
                }
                collect_parse_error_only_parse_result_error(&function_define.name, Expected::FunctionName, errors);
                
                let arguments = &function_define.args;
                collect_parse_error_only_parse_result_error(&arguments.paren_left, Expected::ParenthesisLeft, errors);
                for argument in arguments.arguments.iter() {
                    collect_parse_error_type_tag(&argument.type_tag, errors, warnings);
                }
                let argument_error = ASTParseError::UnexpectedToken(arguments.error_tokens.clone());
                errors.extend(unexpected_token_error_2_or_error_3(&vec![&argument_error], Expected::FunctionArgument));
                collect_parse_error_only_parse_result_error(&arguments.paren_right, Expected::ParenthesisRight, errors);

                if let Some(type_tag) = &function_define.type_tag {
                    collect_parse_error_type_tag(type_tag, errors, warnings);
                }

                collect_parse_error_with_recovered(&function_define.block, collect_parse_error_block, Expected::Block, errors, warnings);
            },
            _ => {}
        }
    }

    errors.extend(unexpected_token_error_2_or_error_3(&statement_errors, Expected::Statement));
}

fn collect_parse_error_block(ast: &Block, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {

}

fn collect_parse_error_type_tag(ast: &TypeTag, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {

}

fn collect_parse_error_generics(ast: &Generics, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {

}

fn collect_parse_error_with_parse_result<T>(
    ast: &ParseResult<T>,
    error_collector: fn(ast: &T, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>),
    expected: Expected,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    match ast {
        Ok(ast) => error_collector(ast, errors, warnings),
        Err(error) => errors.extend(unexpected_token_error_2_or_error_3(&vec![error], expected))
    }
}

fn collect_parse_error_with_recovered<T>(
    recovered: &Recovered<T>,
    error_collector: fn(ast: &T, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>),
    expected: Expected,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    let expected = if recovered.value.is_some() { Expected::Unnecessary } else { expected };
    if let Err(error) = &recovered.result {
        errors.extend(unexpected_token_error_2_or_error_3(&vec![error], expected));
    }

    if let Some(ast) = &recovered.value {
        error_collector(ast, errors, warnings);
    }
}

fn collect_parse_error_only_parse_result_error<T>(result: &ParseResult<T>, expected: Expected, errors: &mut Vec<TranspileError>) {
    match result {
        Err(error) => errors.extend(unexpected_token_error_2_or_error_3(&vec![error], expected)),
        _ => {}
    }
}

fn collect_parse_error_expression(ast: &Expression, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
    
}


