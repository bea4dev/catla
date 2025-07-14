use bumpalo::Bump;

use crate::{ast::GenericsInfo, error::ParseError, lexer::Lexer};

pub(crate) fn parse_generics_info<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<GenericsInfo<'input, 'allocator>> {
    todo!()
}
