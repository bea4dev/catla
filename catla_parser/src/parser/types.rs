use bumpalo::Bump;

use crate::lexer::TokenKind;

use super::{parse_as_literal, parse_literal, read_until_token_found, recover_until_token_found, skip, unexpected_token_error, ArrayTypeInfo, BaseTypeInfo, Generics, GetTokenKind, Span, TokenCursor, TupleTypeInfo, TypeAttribute, TypeAttributeEnum, TypeInfo, TypeInfoResult, TypeTag, TypeTagKind, TypeTagKindEnum};


pub fn parse_type_tag<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>) -> Option<TypeTag<'input, 'allocator>> {
    let span = Span::start(cursor);

    let type_tag_kind_token = cursor.next();
    let type_tag_kind_enum = match type_tag_kind_token.get_kind() {
        TokenKind::Colon     => TypeTagKindEnum::Normal,
        TokenKind::ThinArrow => TypeTagKindEnum::ReturnType,
        _ => {
            cursor.prev();
            return None;
        }
    };
    let tag_kind = TypeTagKind::new(type_tag_kind_enum, type_tag_kind_token.unwrap().span.clone());

    let type_info = parse_type_info_result(cursor);

    return Some(TypeTag { tag_kind, type_info, span: span.elapsed(cursor) });
}

pub fn parse_type_info<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>) -> Option<TypeInfo<'input, 'allocator>> {
    if let Some(array_type_info) = parse_array_type_info(cursor) {
        return Some(TypeInfo::ArrayType(array_type_info));
    }
    if let Some(base_type_info) = parse_base_type_info(cursor) {
        return Some(TypeInfo::BaseType(base_type_info));
    }
    if let Some(tuple_type_info) = parse_tuple_type_info(cursor) {
        return Some(TypeInfo::TupleType(tuple_type_info));
    }
    return None;
}

pub fn parse_base_type_info<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>) -> Option<BaseTypeInfo<'input, 'allocator>> {
    let span = Span::start(cursor);

    let mut path = Vec::new_in(cursor.allocator);

    if cursor.current().get_kind() == TokenKind::LargeThis {
        path.push(parse_as_literal(cursor).unwrap());
    }

    loop {
        let literal = match parse_literal(cursor) {
            Some(literal) => literal,
            _ => break
        };
        
        path.push(literal);

        if cursor.next().get_kind() != TokenKind::DoubleColon {
            cursor.prev();
            break;
        }
    }

    if path.is_empty() {
        return None;
    }

    let generics = parse_generics(cursor);

    let type_attributes = parse_type_attributes(cursor);

    return Some(BaseTypeInfo { path, generics, type_attributes, span: span.elapsed(cursor) });
}

pub fn parse_array_type_info<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>) -> Option<ArrayTypeInfo<'input, 'allocator>> {
    let span = Span::start(cursor);
    
    if cursor.next().get_kind() != TokenKind::BracketLeft {
        cursor.prev();
        return None;
    }
    
    skip(cursor, &[TokenKind::LineFeed]);
    
    let type_info = parse_type_info(cursor)
        .ok_or_else(|| { unexpected_token_error(cursor.allocator, cursor.current()) })
        .map(|type_info| { &*cursor.allocator.alloc(type_info) });
    
    let error_tokens = recover_until_token_found(cursor, &[TokenKind::BracketRight, TokenKind::LineFeed]);
    
    skip(cursor, &[TokenKind::LineFeed]);
    
    let allocator = cursor.allocator;
    let last = cursor.next();
    let bracket_right = if last.get_kind() == TokenKind::BracketRight {
        Ok(())
    } else {
        Err(unexpected_token_error(allocator, last))
    };
    
    if bracket_right.is_err() {
        cursor.prev();
    }
    
    return Some(ArrayTypeInfo { type_info, error_tokens, bracket_right, span: span.elapsed(cursor) });
}

pub fn parse_tuple_type_info<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>) -> Option<TupleTypeInfo<'input, 'allocator>> {
    let span = Span::start(cursor);

    if cursor.next().get_kind() != TokenKind::ParenthesisLeft {
        cursor.prev();
        return None;
    }

    let mut error_tokens = Vec::new_in(cursor.allocator);
    let mut types = Vec::new_in(cursor.allocator);

    loop {
        skip(cursor, &[TokenKind::LineFeed]);

        let type_info = match parse_type_info(cursor) {
            Some(type_info) => type_info,
            _ => {
                let dropped_tokens = read_until_token_found(cursor, &[TokenKind::Comma, TokenKind::ParenthesisRight]);
                error_tokens.push(dropped_tokens);

                match cursor.peek_prev().get_kind() {
                    TokenKind::Comma => continue,
                    TokenKind::ParenthesisRight => break,
                    _ => break
                }
            }
        };

        skip(cursor, &[TokenKind::LineFeed]);

        types.push(type_info);

        let comma_or_paren = cursor.next().get_kind();
        match comma_or_paren {
            TokenKind::ParenthesisRight => break,
            _ => {
                cursor.prev();
                continue;
            }
        }
    }

    return Some(TupleTypeInfo { types, error_tokens, span: span.elapsed(cursor) });
}

pub fn parse_type_info_result<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>) -> TypeInfoResult<'input, 'allocator> {
    return parse_type_info(cursor).ok_or_else(|| { unexpected_token_error(cursor.allocator, cursor.current()) });
}

fn parse_type_attributes<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>) -> Vec<TypeAttribute<'input, 'allocator>, &'allocator Bump> {
    let mut attributes = Vec::new_in(cursor.allocator);
    loop {
        let attribute = match parse_type_attribute(cursor) {
            Some(attribute) => attribute,
            _ => break
        };
        attributes.push(attribute);
    }
    return attributes;
}

fn parse_type_attribute<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>) -> Option<TypeAttribute<'input, 'allocator>> {
    let span = Span::start(cursor);

    let attribute = match cursor.next().get_kind() {
        TokenKind::InterrogationMark => TypeAttributeEnum::Optional,
        TokenKind::ExclamationMark   => TypeAttributeEnum::Result(parse_generics(cursor)),
        _ => {
            cursor.prev();
            return None;
        }
    };

    return Some(TypeAttribute::new(attribute, span.elapsed(cursor)));
}

pub fn parse_generics<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>) -> Option<Generics<'input, 'allocator>> {
    let span = Span::start(cursor);

    if cursor.next().get_kind() != TokenKind::LessThan {
        cursor.prev();
        return None;
    }

    let mut elements = Vec::new_in(cursor.allocator);
    let mut error_tokens = Vec::new_in(cursor.allocator);
    loop {
        skip(cursor, &[TokenKind::LineFeed]);

        let element = match parse_type_info(cursor) {
            Some(type_info) => type_info,
            _ => {
                skip(cursor, &[TokenKind::LineFeed]);

                error_tokens.extend(read_until_token_found(cursor, &[TokenKind::Comma, TokenKind::GreaterThan, TokenKind::LineFeed, TokenKind::Semicolon]));
                
                match cursor.peek_prev().get_kind() {
                    TokenKind::Comma => {
                        skip(cursor, &[TokenKind::LineFeed]);
                        continue;
                    },
                    TokenKind::GreaterThan => break,
                    _ => break
                }
            }
        };

        elements.push(element);

        skip(cursor, &[TokenKind::LineFeed]);

        let comma_or_less = cursor.next().get_kind();
        match comma_or_less {
            TokenKind::GreaterThan => break,
            TokenKind::Comma => {
                skip(cursor, &[TokenKind::LineFeed]);
                continue
            },
            _ => {
                let prev = cursor.peek_prev();
                if let Some(prev) = prev {
                    error_tokens.push(prev.clone());
                }
                continue;
            }
        }
    }

    return Some(Generics { elements, error_tokens, span: span.elapsed(cursor) });
}
