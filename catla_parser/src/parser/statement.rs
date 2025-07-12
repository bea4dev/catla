use bumpalo::Bump;

use crate::{
    ast::StatementWithTagAndDocs,
    error::ParseError,
    lexer::Lexer,
};

pub(crate) fn parse_statement_with_tag_and_docs<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<StatementWithTagAndDocs<'input, 'allocator>> {
    todo!()
}
