use catla_parser::parser::{Program, ASTParseError};

use self::statement::{not_separated_statement_error_1, unexpected_token_error_2_or_error_3, Expected};

use super::{TranspileError, TranspileWarning};

pub mod statement;



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

    }

    errors.extend(unexpected_token_error_2_or_error_3(&statement_errors, Expected::Statement));
}

