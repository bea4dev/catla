use allocator_api2::vec::Vec;
use bumpalo::Bump;

use crate::{
    ast::Program,
    error::{ParseError, ParseErrorKind, recover_until},
    lexer::{GetKind, Lexer, TokenKind},
    parser::statement::parse_statement_with_tag_and_docs,
};

pub(crate) mod expression;
pub(crate) mod literal;
pub(crate) mod statement;
pub(crate) mod types;

pub fn parse_program<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    until: &[TokenKind],
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> &'allocator Program<'input, 'allocator> {
    let anchor = lexer.cast_anchor();

    let mut statements = Vec::new_in(allocator);

    loop {
        match parse_statement_with_tag_and_docs(lexer, errors, allocator) {
            Some(statement) => {
                statements.push(statement);
            }
            None => {
                let current = lexer.current().get_kind();

                if until.contains(&current) {
                    break;
                }

                if current != TokenKind::LineFeed {
                    let error = recover_until(
                        lexer,
                        &[TokenKind::LineFeed],
                        ParseErrorKind::ExtraStatementTokens,
                    );
                    errors.push(error);
                }
            }
        }

        lexer.skip_line_feed();
    }

    allocator.alloc(Program {
        statements,
        span: anchor.elapsed(lexer),
    })
}
