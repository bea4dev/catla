use bumpalo::Bump;

use crate::{ast::Expression, error::ParseError, lexer::Lexer};

pub(crate) fn parse_expression<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<Expression<'input, 'allocator>> {
    todo!()
}
