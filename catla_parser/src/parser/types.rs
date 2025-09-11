use allocator_api2::vec::Vec;
use bumpalo::Bump;

use crate::{
    ast::{ArrayTypeInfo, BaseTypeInfo, GenericsInfo, TupleTypeInfo, TypeInfo},
    error::{recover_until, ParseError, ParseErrorKind},
    lexer::{GetKind, Lexer, TokenKind},
};

pub(crate) fn parse_generics_info<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<GenericsInfo<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::LessThan {
        return None;
    }
    lexer.next();

    lexer.skip_line_feed();

    let mut types = Vec::new_in(allocator);

    loop {
        match parse_type_info(lexer, errors, allocator) {
            Some(type_info) => {
                types.push(type_info);
            },
            None => break,
        }

        match lexer.current().get_kind() {
            TokenKind::Comma => {
                lexer.next();
            }
            _ => break
        }
    }

    if lexer.current().get_kind() != TokenKind::GreaterThan {
        let error = recover_until(lexer, &[TokenKind::GreaterThan], ParseErrorKind::)
    }

    Some(GenericsInfo {
        types: (),
        span: (),
    })
}

pub(crate) fn parse_type_info<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<TypeInfo<'input, 'allocator>> {
    if let Some(array) = parse_array_type_info(lexer, errors, allocator) {
        return Some(TypeInfo::Array(array));
    }

    if let Some(base) = parse_base_type_info(lexer, errors, allocator) {
        return Some(TypeInfo::Base(base));
    }

    if let Some(tuple) = parse_tuple_info(lexer, errors, allocator) {
        return Some(TypeInfo::Tuple(tuple));
    }

    None
}

fn parse_array_type_info<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<ArrayTypeInfo<'input, 'allocator>> {
    todo!()
}

fn parse_base_type_info<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<BaseTypeInfo<'input, 'allocator>> {
    todo!()
}

fn parse_tuple_info<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<TupleTypeInfo<'input, 'allocator>> {
    todo!()
}
