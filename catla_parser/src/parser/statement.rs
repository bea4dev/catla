use self::types::parse_type_info_result;

use super::{
    types::{parse_type_info, parse_type_tag},
    *,
};
use allocator_api2::vec;
use expression::*;

pub fn parse_statement<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Statement<'input, 'allocator> {
    if let Some(assignment) = parse_assignment(cursor) {
        return Ok(StatementAST::Assignment(assignment));
    }
    if let Some(exchange) = parse_exchange(cursor) {
        return Ok(StatementAST::Exchange(exchange));
    }
    if let Some(import) = parse_import(cursor) {
        return Ok(StatementAST::Import(import));
    }
    if let Some(define) = parse_statement_with_attributes(cursor) {
        return match define {
            Ok(define) => Ok(define),
            Err(attributes) => Ok(StatementAST::StatementAttributes(attributes)),
        };
    }
    if let Some(type_define) = parse_type_define(cursor) {
        return Ok(StatementAST::TypeDefine(type_define));
    }
    if let Some(implements) = parse_implements(cursor) {
        return Ok(StatementAST::Implements(implements));
    }
    if let Some(drop_statement) = parse_drop_statement(cursor) {
        return Ok(StatementAST::DropStatement(drop_statement));
    }
    if let Some(expression) = parse_expression(cursor) {
        return Ok(StatementAST::Expression(expression));
    }

    return Err(ASTParseError::UnexpectedToken(
        vec![in cursor.allocator; cursor.next().unwrap().clone(); 1],
    ));
}

fn parse_assignment<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<Assignment<'input, 'allocator>> {
    let start_position = cursor.current_position;
    let span = Span::start(cursor);

    let left_expr = match parse_expression(cursor) {
        Some(expr) => expr,
        _ => return None, // Not a assignment grammar (Maybe...)
    };

    let next = cursor.next().get_kind();
    if next != TokenKind::Equal {
        cursor.current_position = start_position;
        return None; // This is single (left) expression!
    }

    skip(cursor, &[TokenKind::LineFeed]);

    let allocator = cursor.allocator;
    let right_expr = match parse_expression(cursor) {
        Some(expr) => Ok(expr),
        _ => match cursor.next() {
            Some(next) => Err(ASTParseError::UnexpectedToken(
                vec![in allocator; next.clone(); 1],
            )),
            _ => Err(ASTParseError::UnexpectedEOF),
        },
    };

    return Some(Assignment {
        left_expr,
        right_expr,
        span: span.elapsed(cursor),
    });
}

fn parse_exchange<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<Exchange<'input, 'allocator>> {
    let start_position = cursor.current_position;
    let span = Span::start(cursor);

    let left_expr = match parse_expression(cursor) {
        Some(expr) => expr,
        _ => return None, // Not a exchange grammar (Maybe...)
    };

    let next = cursor.next().get_kind();
    if next != TokenKind::Exchange {
        cursor.current_position = start_position;
        return None; // This is single (left) expression!
    }

    skip(cursor, &[TokenKind::LineFeed]);

    let right_expr = match parse_expression(cursor) {
        Some(expr) => Ok(expr),
        _ => {
            let allocator = cursor.allocator;
            match cursor.next() {
                Some(next) => Err(ASTParseError::UnexpectedToken(
                    vec![in allocator; next.clone(); 1],
                )),
                _ => Err(ASTParseError::UnexpectedEOF),
            }
        }
    };

    return Some(Exchange {
        left_expr,
        right_expr,
        span: span.elapsed(cursor),
    });
}

fn parse_import<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<Import<'input, 'allocator>> {
    let span = Span::start(cursor);

    let first = cursor.next().get_kind();
    if first != TokenKind::Import {
        cursor.prev();
        return None;
    }

    let mut import_path = Vec::new_in(cursor.allocator);
    let second = match parse_literal(cursor) {
        Some(second) => second,
        _ => {
            let allocator = cursor.allocator;
            let elements = ImportElements {
                elements: vec![in allocator],
                error_tokens: vec![in allocator],
                brace_right: Ok(()),
            };
            return Some(Import {
                import_path: vec![in allocator],
                elements,
                span: span.elapsed(cursor),
            });
        }
    };
    import_path.push(second);

    loop {
        let next_kind = cursor.current().get_kind();
        if next_kind == TokenKind::LineFeed || next_kind == TokenKind::Semicolon {
            // accept as "import literal:: ... ::literal"
            let element = import_path.pop();
            let elements = match element {
                Some(element) => vec![in cursor.allocator; element; 1],
                _ => Vec::new_in(cursor.allocator),
            };
            let elements = ImportElements {
                elements,
                error_tokens: vec![in cursor.allocator],
                brace_right: Ok(()),
            };
            return Some(Import {
                import_path,
                elements,
                span: span.elapsed(cursor),
            });
        }

        let next_kind = cursor.next().get_kind();
        if next_kind != TokenKind::DoubleColon {
            let allocator = cursor.allocator;
            let elements = ImportElements {
                elements: vec![in allocator],
                error_tokens: vec![in allocator],
                brace_right: Ok(()),
            };
            return Some(Import {
                import_path,
                elements,
                span: span.elapsed(cursor),
            });
        }

        let path_or_element = cursor.current().cloned();
        let path_or_element_kind = cursor.current().get_kind();

        match path_or_element_kind {
            TokenKind::Literal => {
                cursor.next();
                let path = path_or_element.unwrap();
                let element = Spanned::new(path.text, path.span.clone());
                import_path.push(element);
            }
            TokenKind::BraceLeft => {
                let elements = parse_import_elements(cursor);
                return Some(Import {
                    import_path,
                    elements,
                    span: span.elapsed(cursor),
                });
            }
            _ => {
                let element = import_path.pop();
                let elements = match element {
                    Some(element) => vec![in cursor.allocator; element; 1],
                    _ => Vec::new_in(cursor.allocator),
                };
                let elements = ImportElements {
                    elements,
                    error_tokens: vec![in cursor.allocator],
                    brace_right: Ok(()),
                };
                return Some(Import {
                    import_path,
                    elements,
                    span: span.elapsed(cursor),
                });
            }
        };
    }
}

fn parse_import_elements<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> ImportElements<'input, 'allocator> {
    cursor.next();

    let mut elements = Vec::new_in(cursor.allocator);
    let mut error_tokens = Vec::new_in(cursor.allocator);
    let brace_right = loop {
        skip(cursor, &[TokenKind::LineFeed]);

        let element = match parse_literal(cursor) {
            Some(element) => element,
            _ => {
                skip(cursor, &[TokenKind::LineFeed]);

                error_tokens.extend(read_until_token_found(
                    cursor,
                    &[
                        TokenKind::Comma,
                        TokenKind::BraceRight,
                        TokenKind::LineFeed,
                        TokenKind::Semicolon,
                    ],
                ));

                match cursor.peek_prev().get_kind() {
                    TokenKind::Comma => {
                        skip(cursor, &[TokenKind::LineFeed]);
                        continue;
                    }
                    TokenKind::BraceRight => break Ok(()),
                    _ => break Err(unexpected_token_error(cursor.allocator, cursor.peek_prev())),
                }
            }
        };

        elements.push(element);

        let comma_or_brace_right = cursor.next().get_kind();
        match comma_or_brace_right {
            TokenKind::BraceRight => break Ok(()),
            TokenKind::Comma => {
                skip(cursor, &[TokenKind::LineFeed]);
                continue;
            }
            _ => {
                let prev = cursor.peek_prev();
                if let Some(prev) = prev {
                    if prev.kind == TokenKind::LineFeed || prev.kind == TokenKind::Semicolon {
                        break Err(unexpected_token_error(cursor.allocator, Some(prev)));
                    }
                    error_tokens.push(prev.clone());
                }
                continue;
            }
        }
    };

    return ImportElements {
        elements,
        error_tokens,
        brace_right,
    };
}

fn parse_statement_with_attributes<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<Result<StatementAST<'input, 'allocator>, Vec<StatementAttribute, &'allocator Bump>>> {
    let statement_attributes = parse_statement_attributes(cursor);

    if let Some(define) = parse_variable_define(cursor, &statement_attributes) {
        return Some(Ok(StatementAST::VariableDefine(define)));
    }

    if let Some(define) = parse_function_define(cursor, &statement_attributes) {
        return Some(Ok(StatementAST::FunctionDefine(define)));
    }

    if let Some(define) = parse_user_type_define(cursor, &statement_attributes) {
        return Some(Ok(StatementAST::UserTypeDefine(define)));
    }

    return if statement_attributes.is_empty() {
        None
    } else {
        Some(Err(statement_attributes))
    };
}

fn parse_statement_attributes<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Vec<StatementAttribute, &'allocator Bump> {
    let mut statement_attributes = Vec::new_in(cursor.allocator);
    loop {
        let next = match cursor.next() {
            Some(next) => next,
            _ => break,
        };
        let attribute_kind = match next.kind {
            TokenKind::Static => StatementAttributeKind::Static,
            TokenKind::Private => StatementAttributeKind::Private,
            TokenKind::Suspend => StatementAttributeKind::Suspend,
            TokenKind::Native => StatementAttributeKind::Native,
            TokenKind::Acyclic => StatementAttributeKind::Acyclic,
            TokenKind::Open => StatementAttributeKind::Open,
            TokenKind::Override => StatementAttributeKind::Override,
            _ => {
                cursor.prev();
                break;
            }
        };
        statement_attributes.push(Spanned::new(attribute_kind, next.span.clone()));
    }

    return statement_attributes;
}

fn parse_variable_define<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
    statement_attributes: &Vec<StatementAttribute, &'allocator Bump>,
) -> Option<VariableDefine<'input, 'allocator>> {
    let span = Span::start(cursor);

    let let_var = match cursor.next() {
        Some(token) => token,
        _ => return None,
    };
    let is_var = match let_var.kind {
        TokenKind::Let => false,
        TokenKind::Var => true,
        _ => {
            cursor.prev();
            return None;
        }
    };
    let attributes = VariableAttributes {
        statement_attributes: statement_attributes.clone(),
        is_var,
        var_let_span: let_var.span.clone(),
    };

    let binding = parse_variable_binding(cursor)
        .ok_or_else(|| unexpected_token_error(cursor.allocator, cursor.current()));

    let type_tag = parse_type_tag(cursor);

    let equal = cursor.current().get_kind();
    if equal != TokenKind::Equal {
        let span = statement_attributes
            .get(0)
            .map_or(span.start_position, |spanned| spanned.span.start)
            ..span.elapsed(cursor).end;
        return Some(VariableDefine {
            attributes,
            binding,
            type_tag,
            expression: None,
            span,
        });
    }
    cursor.next();

    skip(cursor, &[TokenKind::LineFeed]);

    let expression = parse_expression(cursor)
        .ok_or_else(|| unexpected_token_error(cursor.allocator, cursor.current()));

    let span = statement_attributes
        .get(0)
        .map_or(span.start_position, |spanned| spanned.span.start)
        ..span.elapsed(cursor).end;

    return Some(VariableDefine {
        attributes,
        binding,
        type_tag,
        expression: Some(expression),
        span,
    });
}

fn parse_variable_binding<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<VariableBinding<'input, 'allocator>> {
    let span = Span::start(cursor);

    let first_kind = cursor.current().get_kind();

    return match first_kind {
        TokenKind::Literal => {
            let token = cursor.next().unwrap();

            Some(VariableBinding {
                binding: Either::Left(Spanned::new(token.text, token.span.clone())),
                error_tokens: Vec::new_in(cursor.allocator),
                span: span.elapsed(cursor),
            })
        }
        TokenKind::ParenthesisLeft => {
            cursor.next();

            skip(cursor, &[TokenKind::LineFeed]);

            let mut error_tokens = Vec::new_in(cursor.allocator);
            let mut bindings = Vec::new_in(cursor.allocator);

            loop {
                skip(cursor, &[TokenKind::LineFeed]);

                let binding = match parse_variable_binding(cursor) {
                    Some(binding) => binding,
                    _ => {
                        let dropped_tokens = read_until_token_found(
                            cursor,
                            &[TokenKind::Comma, TokenKind::ParenthesisRight],
                        );
                        error_tokens.push(dropped_tokens);

                        match cursor.peek_prev().get_kind() {
                            TokenKind::Comma => continue,
                            TokenKind::ParenthesisRight => break,
                            _ => break,
                        }
                    }
                };

                skip(cursor, &[TokenKind::LineFeed]);

                bindings.push(binding);

                let comma_or_paren = cursor.next().get_kind();
                match comma_or_paren {
                    TokenKind::ParenthesisRight => break,
                    _ => {
                        cursor.prev();
                        continue;
                    }
                }
            }

            Some(VariableBinding {
                binding: Either::Right(bindings),
                error_tokens,
                span: span.elapsed(cursor),
            })
        }
        _ => None,
    };
}

fn parse_function_define<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
    statement_attributes: &Vec<StatementAttribute, &'allocator Bump>,
) -> Option<FunctionDefine<'input, 'allocator>> {
    let span = Span::start(cursor);

    let allocator = cursor.allocator;

    let func_keyword = cursor.next().get_kind();
    if func_keyword != TokenKind::Function {
        cursor.prev();
        return None;
    }

    let generics_define = parse_generics_define(cursor);

    let name_token = cursor.next();
    let name_kind = name_token.get_kind();

    let name = match name_kind {
        TokenKind::Literal | TokenKind::New | TokenKind::Drop | TokenKind::Mutex => Ok(
            Spanned::new(name_token.unwrap().text, name_token.unwrap().span.clone()),
        ),
        _ => Err(unexpected_token_error(allocator, name_token)),
    };

    if name.is_err() {
        cursor.prev();
    }

    let args = parse_function_arguments(cursor);

    let type_tag = parse_type_tag(cursor);

    let where_clause = parse_where_clause(cursor, TokenKind::BraceLeft);

    let block_or_semicolon = parse_with_recover(
        cursor,
        parse_block_or_semicolon,
        &[
            TokenKind::BraceLeft,
            TokenKind::LineFeed,
            TokenKind::Semicolon,
        ],
    );

    let span = statement_attributes
        .get(0)
        .map_or(span.start_position, |spanned| spanned.span.start)
        ..span.elapsed(cursor).end;

    return Some(FunctionDefine {
        attributes: statement_attributes.clone(),
        generics_define,
        name,
        args,
        type_tag,
        where_clause,
        block_or_semicolon,
        span,
    });
}

fn parse_block_or_semicolon<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<Either<Range<usize>, Block<'input, 'allocator>>> {
    match cursor.current().get_kind() {
        TokenKind::Semicolon => Some(Either::Left(cursor.next().unwrap().span.clone())),
        TokenKind::BraceLeft => parse_block(cursor).map(|block| Either::Right(block)),
        _ => None,
    }
}

pub fn parse_where_clause<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
    next_expected_token: TokenKind,
) -> Option<WhereClause<'input, 'allocator>> {
    let span = Span::start(cursor);

    let first_token = cursor.next().get_kind();
    if first_token != TokenKind::Where {
        cursor.prev();
        return None;
    }

    let mut elements = Vec::new_in(cursor.allocator);
    let mut error_tokens = Vec::new_in(cursor.allocator);

    let next_expected_token = loop {
        skip(cursor, &[TokenKind::LineFeed]);

        let element = match parse_where_element(cursor) {
            Some(element) => element,
            _ => {
                skip(cursor, &[TokenKind::LineFeed]);

                error_tokens.push(read_until_token_found(
                    cursor,
                    &[
                        TokenKind::Comma,
                        TokenKind::LineFeed,
                        TokenKind::Semicolon,
                        next_expected_token,
                    ],
                ));

                let prev_token_kind = cursor.peek_prev().get_kind();
                match prev_token_kind {
                    TokenKind::Comma => {
                        skip(cursor, &[TokenKind::LineFeed]);
                        continue;
                    }
                    _ => {
                        break if prev_token_kind == next_expected_token {
                            Ok(())
                        } else {
                            Err(unexpected_token_error(cursor.allocator, cursor.peek_prev()))
                        };
                    }
                }
            }
        };

        elements.push(element);

        let comma_or_next_expected = cursor.next().get_kind();
        match comma_or_next_expected {
            TokenKind::Comma => {
                skip(cursor, &[TokenKind::LineFeed]);
                continue;
            }
            _ => {
                if comma_or_next_expected == next_expected_token {
                    break Ok(());
                }

                let prev = cursor.peek_prev();
                if let Some(prev) = prev {
                    if prev.kind == TokenKind::LineFeed || prev.kind == TokenKind::Semicolon {
                        break Err(unexpected_token_error(cursor.allocator, Some(prev)));
                    }
                    error_tokens.push(vec![in cursor.allocator; prev.clone(); 1]);
                }
                continue;
            }
        }
    };

    if next_expected_token.is_ok() {
        cursor.prev();
    }

    return Some(WhereClause {
        elements,
        error_tokens,
        next_expected_token,
        span: span.elapsed(cursor),
    });
}

pub fn parse_where_element<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<WhereElement<'input, 'allocator>> {
    let span = Span::start(cursor);

    let target_type = parse_type_info(cursor)?;

    let mut bounds = Vec::new_in(cursor.allocator);

    if cursor.next().get_kind() == TokenKind::Colon {
        loop {
            skip(cursor, &[TokenKind::LineFeed]);

            let bound = match parse_type_info(cursor) {
                Some(bound_type_info) => bound_type_info,
                _ => break,
            };

            skip(cursor, &[TokenKind::LineFeed]);

            bounds.push(bound);

            if cursor.next().get_kind() != TokenKind::Plus {
                cursor.prev();
                break;
            }
        }
    } else {
        cursor.prev();
    }

    return Some(WhereElement {
        target_type,
        bounds,
        span: span.elapsed(cursor),
    });
}

fn parse_function_arguments<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> FunctionArguments<'input, 'allocator> {
    let span = Span::start(cursor);

    let paren_left = cursor.next().get_kind();
    let paren_left = if paren_left != TokenKind::ParenthesisLeft {
        if paren_left == TokenKind::None {
            Err(ASTParseError::UnexpectedEOF)
        } else {
            cursor.prev();
            let dropped_tokens = read_until_token_found(cursor, &[TokenKind::ParenthesisLeft]);
            Err(ASTParseError::UnexpectedToken(dropped_tokens))
        }
    } else {
        Ok(())
    };

    skip(cursor, &[TokenKind::LineFeed]);

    let current_kind = cursor.current().get_kind();
    let this_mutability = if current_kind == TokenKind::Let || current_kind == TokenKind::Var {
        let let_or_var = cursor.next().unwrap();
        let is_var = match current_kind {
            TokenKind::Var => true,
            TokenKind::Let => false,
            _ => unreachable!(),
        };
        let is_var = Spanned::new(is_var, let_or_var.span.clone());

        let this_token_kind = cursor.current().get_kind();
        let this_token = cursor.next().cloned();
        let this_span = if this_token_kind == TokenKind::This {
            skip(cursor, &[TokenKind::LineFeed]);
            Ok(this_token.unwrap().span.clone())
        } else {
            cursor.prev();
            Err(unexpected_token_error(&cursor.allocator, cursor.current()))
        };

        Some(ThisMutability { is_var, this_span })
    } else {
        None
    };

    let mut arguments = Vec::new_in(cursor.allocator);
    let mut error_tokens = Vec::new_in(cursor.allocator);

    let paren_right = loop {
        let argument = match parse_function_argument(cursor) {
            Some(argument) => argument,
            _ => {
                skip(cursor, &[TokenKind::LineFeed]);

                error_tokens.extend(read_until_token_found(
                    cursor,
                    &[
                        TokenKind::Comma,
                        TokenKind::ParenthesisRight,
                        TokenKind::BraceLeft,
                        TokenKind::LineFeed,
                        TokenKind::Semicolon,
                    ],
                ));

                match cursor.peek_prev().get_kind() {
                    TokenKind::Comma => {
                        skip(cursor, &[TokenKind::LineFeed]);
                        continue;
                    }
                    TokenKind::ParenthesisRight => break Ok(()),
                    TokenKind::BraceLeft => {
                        cursor.prev();
                        break Err(unexpected_token_error(cursor.allocator, cursor.current()));
                    }
                    _ => break Err(unexpected_token_error(cursor.allocator, cursor.peek_prev())),
                }
            }
        };

        arguments.push(argument);

        let comma_or_paren_right = cursor.next().get_kind();
        match comma_or_paren_right {
            TokenKind::ParenthesisRight => break Ok(()),
            TokenKind::Comma => {
                skip(cursor, &[TokenKind::LineFeed]);
                continue;
            }
            TokenKind::BraceRight => {
                cursor.prev();
                break Err(unexpected_token_error(cursor.allocator, cursor.current()));
            }
            _ => {
                let prev = cursor.peek_prev();
                if let Some(prev) = prev {
                    if prev.kind == TokenKind::LineFeed || prev.kind == TokenKind::Semicolon {
                        break Err(unexpected_token_error(cursor.allocator, Some(prev)));
                    }
                    error_tokens.push(prev.clone());
                }
                continue;
            }
        }
    };

    return FunctionArguments {
        paren_left,
        this_mutability,
        arguments,
        error_tokens,
        paren_right,
        span: span.elapsed(cursor),
    };
}

pub fn parse_function_argument<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<FunctionArgument<'input, 'allocator>> {
    let span = Span::start(cursor);

    let start_position = cursor.current_position;

    let binding = parse_variable_binding(cursor)?;
    let type_tag = match parse_type_tag(cursor) {
        Some(type_tag) => type_tag,
        _ => {
            cursor.current_position = start_position;
            return None;
        }
    };

    return Some(FunctionArgument {
        binding,
        type_tag,
        span: span.elapsed(cursor),
    });
}

pub fn parse_generics_define<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<GenericsDefine<'input, 'allocator>> {
    let span = Span::start(cursor);

    let first_token = cursor.next().get_kind();
    if first_token != TokenKind::LessThan {
        cursor.prev();
        return None;
    }

    let mut elements = Vec::new_in(cursor.allocator);
    let mut error_tokens = Vec::new_in(cursor.allocator);

    let greater_than = loop {
        skip(cursor, &[TokenKind::LineFeed]);

        let element = match parse_generics_define_element(cursor) {
            Some(element) => element,
            _ => {
                skip(cursor, &[TokenKind::LineFeed]);

                error_tokens.extend(read_until_token_found(
                    cursor,
                    &[
                        TokenKind::Comma,
                        TokenKind::GreaterThan,
                        TokenKind::LineFeed,
                        TokenKind::Semicolon,
                    ],
                ));

                match cursor.peek_prev().get_kind() {
                    TokenKind::Comma => {
                        skip(cursor, &[TokenKind::LineFeed]);
                        continue;
                    }
                    TokenKind::GreaterThan => break Ok(()),
                    _ => break Err(unexpected_token_error(cursor.allocator, cursor.peek_prev())),
                }
            }
        };

        elements.push(element);

        let comma_or_greater_than = cursor.next().get_kind();
        match comma_or_greater_than {
            TokenKind::GreaterThan => break Ok(()),
            TokenKind::Comma => {
                skip(cursor, &[TokenKind::LineFeed]);
                continue;
            }
            _ => {
                let prev = cursor.peek_prev();
                if let Some(prev) = prev {
                    if prev.kind == TokenKind::LineFeed || prev.kind == TokenKind::Semicolon {
                        break Err(unexpected_token_error(cursor.allocator, Some(prev)));
                    }
                    error_tokens.push(prev.clone());
                }
                continue;
            }
        }
    };

    return Some(GenericsDefine {
        elements,
        error_tokens,
        greater_than,
        span: span.elapsed(cursor),
    });
}

pub fn parse_generics_define_element<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<GenericsElement<'input, 'allocator>> {
    let span = Span::start(cursor);

    let name = parse_literal(cursor)?;

    let mut bounds = Vec::new_in(cursor.allocator);

    if cursor.next().get_kind() == TokenKind::Colon {
        loop {
            skip(cursor, &[TokenKind::LineFeed]);

            let bound = match parse_type_info(cursor) {
                Some(bound_type_info) => bound_type_info,
                _ => break,
            };

            skip(cursor, &[TokenKind::LineFeed]);

            bounds.push(bound);

            if cursor.next().get_kind() != TokenKind::Plus {
                cursor.prev();
                break;
            }
        }
    } else {
        cursor.prev();
    }

    return Some(GenericsElement {
        name,
        bounds,
        span: span.elapsed(cursor),
    });
}

fn parse_user_type_define<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
    statement_attributes: &Vec<StatementAttribute, &'allocator Bump>,
) -> Option<UserTypeDefine<'input, 'allocator>> {
    let span = Span::start(cursor);

    let kind_token = cursor.next();
    let kind_span = kind_token.map(|token| token.span.clone());

    let kind = match kind_token.get_kind() {
        TokenKind::Class => UserTypeKind::new(UserTypeKindEnum::Class, kind_span.unwrap()),
        TokenKind::Struct => UserTypeKind::new(UserTypeKindEnum::Struct, kind_span.unwrap()),
        TokenKind::Interface => UserTypeKind::new(UserTypeKindEnum::Interface, kind_span.unwrap()),
        _ => {
            cursor.prev();
            return None;
        }
    };

    let name = parse_literal_result(cursor);

    let mut error_tokens = Vec::new_in(cursor.allocator);

    let generics_define = parse_generics_define(cursor);
    if generics_define.is_none() {
        error_tokens.extend(recover_until_token_found(
            cursor,
            &[TokenKind::Colon, TokenKind::Where, TokenKind::BraceLeft],
        ));
    }

    let super_type_info = parse_super_type_info(cursor);
    if super_type_info.is_none() {
        error_tokens.merged_extend(recover_until_token_found(
            cursor,
            &[TokenKind::Where, TokenKind::BraceLeft],
        ));
    }

    let where_clause = parse_where_clause(cursor, TokenKind::BraceLeft);

    let block = parse_with_recover(
        cursor,
        parse_block,
        &[
            TokenKind::BraceLeft,
            TokenKind::LineFeed,
            TokenKind::Semicolon,
        ],
    );

    return Some(UserTypeDefine {
        attributes: statement_attributes.clone(),
        kind,
        name,
        generics_define,
        super_type_info,
        error_tokens,
        where_clause,
        block,
        span: span.elapsed(cursor),
    });
}

fn parse_type_define<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<TypeDefine<'input, 'allocator>> {
    let span = Span::start(cursor);

    if cursor.next().get_kind() != TokenKind::Type {
        cursor.prev();
        return None;
    }

    let name = parse_literal_result(cursor);

    let mut error_tokens = Vec::new_in(cursor.allocator);

    let generics_define = parse_generics_define(cursor);
    if generics_define.is_none() {
        error_tokens.push(recover_until_token_found(
            cursor,
            &[TokenKind::Equal, TokenKind::LineFeed],
        ));
    }

    if cursor.next().get_kind() != TokenKind::Equal {
        cursor.prev();
        error_tokens.push(recover_until_token_found(
            cursor,
            &[TokenKind::Equal, TokenKind::LineFeed],
        ));

        if cursor.current().get_kind() == TokenKind::Equal {
            cursor.next();
        }
    }

    let type_info = parse_type_info_result(cursor);

    return Some(TypeDefine {
        name,
        generics_define,
        type_info,
        error_tokens,
        span: span.elapsed(cursor),
    });
}

fn parse_super_type_info<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<SuperTypeInfo<'input, 'allocator>> {
    let span = Span::start(cursor);

    if cursor.next().get_kind() != TokenKind::Colon {
        cursor.prev();
        return None;
    }

    let mut type_infos = Vec::new_in(cursor.allocator);
    let mut error_tokens = Vec::new_in(cursor.allocator);

    loop {
        skip(cursor, &[TokenKind::LineFeed]);

        let type_info = match parse_type_info(cursor) {
            Some(type_info) => type_info,
            _ => {
                let dropped_tokens =
                    read_until_token_found(cursor, &[TokenKind::Comma, TokenKind::BraceLeft]);
                error_tokens.extend(dropped_tokens);

                match cursor.peek_prev().get_kind() {
                    TokenKind::Comma => continue,
                    TokenKind::BraceLeft => {
                        cursor.prev();
                        break;
                    }
                    _ => break,
                }
            }
        };

        type_infos.push(type_info);

        let comma_or_brace = cursor.next().get_kind();
        match comma_or_brace {
            TokenKind::BraceLeft => {
                cursor.prev();
                break;
            }
            _ => continue,
        }
    }

    return Some(SuperTypeInfo {
        type_infos,
        error_tokens,
        span: span.elapsed(cursor),
    });
}

fn parse_implements<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<Implements<'input, 'allocator>> {
    let span = Span::start(cursor);

    if cursor.next().get_kind() != TokenKind::Implements {
        cursor.prev();
        return None;
    }

    let generics_define = parse_generics_define(cursor);

    let interface = parse_type_info_result(cursor);

    let target_user_type = if cursor.current().get_kind() == TokenKind::For {
        cursor.next();
        parse_type_info_result(cursor)
    } else {
        Err(unexpected_token_error(&cursor.allocator, cursor.current()))
    };

    let where_clause = parse_where_clause(cursor, TokenKind::BraceLeft);

    let block = parse_with_recover(
        cursor,
        parse_block,
        &[
            TokenKind::BraceLeft,
            TokenKind::LineFeed,
            TokenKind::Semicolon,
        ],
    );

    Some(Implements {
        generics_define,
        interface,
        target_user_type,
        where_clause,
        block,
        span: span.elapsed(cursor),
    })
}

fn parse_drop_statement<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<DropStatement<'input, 'allocator>> {
    let span = Span::start(cursor);

    if cursor.next().get_kind() != TokenKind::Drop {
        cursor.prev();
        return None;
    }

    let acyclic_keyword_span = if cursor.current().get_kind() == TokenKind::Acyclic {
        Some(cursor.next().unwrap().span.clone())
    } else {
        None
    };

    let expression = parse_expression(cursor)
        .ok_or_else(|| unexpected_token_error(cursor.allocator, cursor.current()));

    return Some(DropStatement {
        acyclic_keyword_span,
        expression,
        span: span.elapsed(cursor),
    });
}

pub fn parse_block<'input, 'allocator>(
    cursor: &mut TokenCursor<'input, 'allocator>,
) -> Option<Block<'input, 'allocator>> {
    let span = Span::start(cursor);

    if cursor.next().get_kind() != TokenKind::BraceLeft {
        cursor.prev();
        return None;
    }

    let program = parse_program(cursor, &[TokenKind::None, TokenKind::BraceRight]);

    let allocator = cursor.allocator;
    let last = cursor.next();
    let brace_right = if last.get_kind() == TokenKind::BraceRight {
        Ok(())
    } else {
        Err(unexpected_token_error(allocator, last))
    };

    if brace_right.is_err() {
        cursor.prev();
    }

    return Some(Block {
        program,
        brace_right,
        span: span.elapsed(cursor),
    });
}
