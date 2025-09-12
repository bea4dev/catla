use allocator_api2::vec::Vec;
use bumpalo::Bump;

use crate::{
    ast::{
        ArrayTypeInfo, BaseTypeInfo, GenericsDefine, GenericsElement, GenericsInfo, TupleTypeInfo,
        TypeAttribute, TypeInfo, TypeInfoBase,
    },
    error::{ParseError, ParseErrorKind, recover_until},
    lexer::{GetKind, Lexer, TokenKind},
    parser::literal::ParseAsLiteral,
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
            }
            None => break,
        }

        match lexer.current().get_kind() {
            TokenKind::Comma => {
                lexer.next();
            }
            _ => break,
        }
    }
    let types = allocator.alloc(types).as_slice();

    if lexer.current().get_kind() != TokenKind::GreaterThan {
        let error = recover_until(
            lexer,
            &[TokenKind::GreaterThan],
            ParseErrorKind::InvalidGenericsInfoOrUnclosedGreaterThan,
        );
        errors.push(error);
    }
    lexer.next();

    Some(GenericsInfo {
        types,
        span: anchor.elapsed(lexer),
    })
}

pub(crate) fn parse_type_info<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<TypeInfo<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let base = if let Some(array) = parse_array_type_info(lexer, errors, allocator) {
        TypeInfoBase::Array(array)
    } else if let Some(base) = parse_base_type_info(lexer, errors, allocator) {
        TypeInfoBase::Base(base)
    } else if let Some(tuple) = parse_tuple_info(lexer, errors, allocator) {
        TypeInfoBase::Tuple(tuple)
    } else if lexer.current().get_kind() == TokenKind::This {
        TypeInfoBase::This(lexer.parse_as_literal())
    } else {
        return None;
    };

    let mut attributes = Vec::new_in(allocator);
    loop {
        let Some(attribute) = parse_type_attribute(lexer, errors, allocator) else {
            break;
        };
        attributes.push(attribute);
    }
    let attributes = allocator.alloc(attributes).as_slice();

    Some(TypeInfo {
        base,
        attributes,
        span: anchor.elapsed(lexer),
    })
}

fn parse_array_type_info<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<ArrayTypeInfo<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::BracketLeft {
        return None;
    }
    lexer.next();

    let base_type = match parse_type_info(lexer, errors, allocator) {
        Some(ty) => Ok(&*allocator.alloc(ty)),
        None => {
            let error = recover_until(
                lexer,
                &[TokenKind::BracketRight],
                ParseErrorKind::MissingBaseTypeInArrayType,
            );
            errors.push(error);

            Err(())
        }
    };

    if lexer.current().get_kind() != TokenKind::BracketRight {
        let error = recover_until(
            lexer,
            &[TokenKind::BracketRight],
            ParseErrorKind::InvalidArrayTypeOrUnclosedBracket,
        );
        errors.push(error);
    }
    lexer.next();

    Some(ArrayTypeInfo {
        base_type,
        span: anchor.elapsed(lexer),
    })
}

fn parse_base_type_info<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<BaseTypeInfo<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Literal {
        return None;
    }
    let first = lexer.parse_as_literal();

    let mut path = Vec::new_in(allocator);
    path.push(first);

    loop {
        if lexer.current().get_kind() != TokenKind::DoubleColon {
            break;
        }
        lexer.next();

        if lexer.current().get_kind() != TokenKind::Literal {
            break;
        }
        path.push(lexer.parse_as_literal());
    }

    let path = allocator.alloc(path).as_slice();

    let generics = parse_generics_info(lexer, errors, allocator);

    Some(BaseTypeInfo {
        path,
        generics,
        span: anchor.elapsed(lexer),
    })
}

fn parse_tuple_info<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<TupleTypeInfo<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::ParenthesesLeft {
        return None;
    }
    lexer.next();

    let mut types = Vec::new_in(allocator);

    lexer.skip_line_feed();

    loop {
        let Some(ty) = parse_type_info(lexer, errors, allocator) else {
            break;
        };
        types.push(ty);

        if lexer.current().get_kind() != TokenKind::Comma {
            break;
        }
        lexer.next();

        lexer.skip_line_feed();
    }

    lexer.skip_line_feed();

    let types = allocator.alloc(types).as_slice();

    if lexer.current().get_kind() != TokenKind::ParenthesesRight {
        let error = recover_until(
            lexer,
            &[TokenKind::ParenthesesRight],
            ParseErrorKind::InvalidTupleExprFormatOrUnclosedParen,
        );
        errors.push(error);
    }
    lexer.next();

    Some(TupleTypeInfo {
        types,
        span: anchor.elapsed(lexer),
    })
}

fn parse_type_attribute<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<TypeAttribute<'input, 'allocator>> {
    match lexer.current().get_kind() {
        TokenKind::QuestionMark => Some(TypeAttribute::Optional {
            span: lexer.next().unwrap().span,
        }),
        TokenKind::ExclamationMark => {
            let anchor = lexer.cast_anchor();

            let generics = parse_generics_info(lexer, errors, allocator);

            Some(TypeAttribute::Result {
                generics,
                span: anchor.elapsed(lexer),
            })
        }
        _ => None,
    }
}

pub(crate) fn parse_generics_define<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<GenericsDefine<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::LessThan {
        return None;
    }
    lexer.next();

    lexer.skip_line_feed();

    let mut elements = Vec::new_in(allocator);

    loop {
        let Some(element) = parse_generics_element(lexer, errors, allocator) else {
            break;
        };
        elements.push(element);

        if lexer.current().get_kind() != TokenKind::Comma {
            break;
        }
        lexer.next();
    }

    let elements = allocator.alloc(elements).as_slice();

    if lexer.current().get_kind() != TokenKind::GreaterThan {
        let error = recover_until(
            lexer,
            &[TokenKind::GreaterThan],
            ParseErrorKind::InvalidGenericsDefineOrUnclosedGreaterThan,
        );
        errors.push(error);
    }
    lexer.next();

    Some(GenericsDefine {
        elements,
        span: anchor.elapsed(lexer),
    })
}

fn parse_generics_element<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<GenericsElement<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Literal {
        return None;
    }
    let name = lexer.parse_as_literal();

    if lexer.current().get_kind() != TokenKind::Colon {
        return Some(GenericsElement {
            name,
            bounds: allocator.alloc(Vec::new_in(allocator)).as_slice(),
            span: anchor.elapsed(lexer),
        });
    }

    let mut bounds = Vec::new_in(allocator);

    loop {
        let Some(bound) = parse_type_info(lexer, errors, allocator) else {
            break;
        };
        bounds.push(bound);

        if lexer.current().get_kind() != TokenKind::Plus {
            break;
        }
        lexer.next();
    }

    let bounds = allocator.alloc(bounds).as_slice();

    Some(GenericsElement {
        name,
        bounds,
        span: anchor.elapsed(lexer),
    })
}
