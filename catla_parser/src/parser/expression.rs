use super::{*, types::parse_generics, statement::{parse_block, parse_function_argument}};


pub fn parse_expression<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<Expression<'allocator, 'input>> {
    let allocator = cursor.allocator;
    
    if let Some(return_expr) = parse_return_expression(cursor) {
        return Some(allocator.alloc(ExpressionEnum::ReturnExpression(return_expr)));
    }
    if let Some(or_expr) = parse_or_expression(cursor) {
        return Some(allocator.alloc(ExpressionEnum::OrExpression(or_expr)));
    }
    if let Some(closure) = parse_closure(cursor) {
        return Some(allocator.alloc(ExpressionEnum::Closure(closure)));
    }
    return None;
}

fn parse_or_expression<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<OrExpression<'allocator, 'input>> {
    let span = Span::start(cursor);

    let left_expr = parse_and_expression(cursor)?;

    let mut right_exprs = Vec::new_in(cursor.allocator);
    loop {
        let op_span_start = Span::start(cursor);
        let cursor_position = cursor.current_position;
        if cursor.next().get_kind() != TokenKind::VerticalBar || cursor.next().get_kind() != TokenKind::VerticalBar {
            cursor.current_position = cursor_position;
            break;
        }
        let op_span = op_span_start.elapsed(cursor);

        skip(cursor, &[TokenKind::LineFeed]);

        let right_expr = parse_and_expression(cursor).ok_or_else(|| { unexpected_token_error(cursor.allocator, cursor.current()) });
        right_exprs.push((op_span, right_expr));
    }

    return Some(OrExpression { left_expr, right_exprs, span: span.elapsed(cursor) });
}

fn parse_and_expression<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<AndExpression<'allocator, 'input>> {
    let span = Span::start(cursor);

    let left_expr = parse_eq_ne_expression(cursor)?;

    let mut right_exprs = Vec::new_in(cursor.allocator);
    loop {
        let op_span_start = Span::start(cursor);
        let cursor_position = cursor.current_position;
        if cursor.next().get_kind() != TokenKind::And || cursor.next().get_kind() != TokenKind::And {
            cursor.current_position = cursor_position;
            break;
        }
        let op_span = op_span_start.elapsed(cursor);

        skip(cursor, &[TokenKind::LineFeed]);

        let right_expr = parse_eq_ne_expression(cursor).ok_or_else(|| { unexpected_token_error(cursor.allocator, cursor.current()) });
        right_exprs.push((op_span, right_expr));
    }

    return Some(AndExpression { left_expr, right_exprs, span: span.elapsed(cursor) });
}

fn parse_eq_ne_expression<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<EQNEExpression<'allocator, 'input>> {
    let span = Span::start(cursor);

    let left_expr = parse_compare_expression(cursor)?;

    let mut right_exprs = Vec::new_in(cursor.allocator);
    loop {
        let op_token = cursor.next();
        let op_token_kind = op_token.get_kind();
        let op_kind = match op_token_kind {
            TokenKind::EqEqual  => EQNECompareOpKind::Equal,
            TokenKind::NotEqual => EQNECompareOpKind::NotEqual,
            _ => {
                cursor.prev();
                break;
            }
        };
        let op = EQNECompareOp::new(op_kind, op_token.unwrap().span.clone());

        skip(cursor, &[TokenKind::LineFeed]);

        let right_expr = parse_compare_expression(cursor).ok_or_else(|| { unexpected_token_error(cursor.allocator, cursor.current()) });
        right_exprs.push((op, right_expr));
    }

    return Some(EQNEExpression { left_expr, right_exprs, span: span.elapsed(cursor) });
}

fn parse_compare_expression<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<CompareExpression<'allocator, 'input>> {
    let span = Span::start(cursor);

    let left_expr = parse_add_or_sub_expression(cursor)?;

    let mut right_exprs = Vec::new_in(cursor.allocator);
    loop {
        let op_token = cursor.next();
        let op_token_kind = op_token.get_kind();
        let op_kind = match op_token_kind {
            TokenKind::GreaterThan => CompareOpKind::GreaterThan,
            TokenKind::GreaterOrEq => CompareOpKind::GreaterOrEqual,
            TokenKind::LessThan    => CompareOpKind::LessThan,
            TokenKind::LessOrEq    => CompareOpKind::LessOrEqual,
            _ => {
                cursor.prev();
                break;
            }
        };
        let op = CompareOp::new(op_kind, op_token.unwrap().span.clone());

        skip(cursor, &[TokenKind::LineFeed]);

        let right_expr = parse_add_or_sub_expression(cursor).ok_or_else(|| { unexpected_token_error(cursor.allocator, cursor.current()) });
        right_exprs.push((op, right_expr));
    }

    return Some(CompareExpression { left_expr, right_exprs, span: span.elapsed(cursor) });
}

fn parse_add_or_sub_expression<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<AddOrSubExpression<'allocator, 'input>> {
    let span = Span::start(cursor);

    let left_expr = parse_mul_or_div_expression(cursor)?;

    let mut right_exprs = Vec::new_in(cursor.allocator);
    loop {
        let op_token = cursor.next();
        let op_token_kind = op_token.get_kind();
        let op_kind = match op_token_kind {
            TokenKind::Plus  => AddOrSubOpKind::Add,
            TokenKind::Minus => AddOrSubOpKind::Sub,
            _ => {
                cursor.prev();
                break;
            }
        };
        let op = AddOrSubOp::new(op_kind, op_token.unwrap().span.clone());

        skip(cursor, &[TokenKind::LineFeed]);

        let right_expr = parse_mul_or_div_expression(cursor).ok_or_else(|| { unexpected_token_error(cursor.allocator, cursor.current()) });
        right_exprs.push((op, right_expr));
    }
    
    return Some(AddOrSubExpression { left_expr, right_exprs, span: span.elapsed(cursor) });
}

fn parse_mul_or_div_expression<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<MulOrDivExpression<'allocator, 'input>> {
    let span = Span::start(cursor);

    let left_expr = parse_factor(cursor)?;

    let mut right_exprs = Vec::new_in(cursor.allocator);
    loop {
        let op_token = cursor.next();
        let op_token_kind = op_token.get_kind();
        let op_kind = match op_token_kind {
            TokenKind::Star  => MulOrDivOpKind::Mul,
            TokenKind::Slash => MulOrDivOpKind::Div,
            _ => {
                cursor.prev();
                break;
            }
        };
        let op = MulOrDivOp::new(op_kind, op_token.unwrap().span.clone());

        skip(cursor, &[TokenKind::LineFeed]);

        let right_expr = parse_factor(cursor).ok_or_else(|| { unexpected_token_error(cursor.allocator, cursor.current()) });
        right_exprs.push((op, right_expr));
    }
    
    return Some(MulOrDivExpression { left_expr, right_exprs, span: span.elapsed(cursor) });
}

fn parse_factor<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<Factor<'allocator, 'input>> {
    let span = Span::start(cursor);

    let negative_keyword_span = if cursor.current().get_kind() == TokenKind::Minus {
        Some(cursor.next().unwrap().span.clone())
    } else {
        None
    };

    let primary = match parse_primary(cursor) {
        Some(primary) => Ok(primary),
        _ => {
            if negative_keyword_span.is_some() {
                Err(unexpected_token_error(cursor.allocator, cursor.current()))
            } else {
                return None;
            }
        }
    };
    
    return Some(Factor { negative_keyword_span, primary, span: span.elapsed(cursor) });
}

fn parse_primary<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<Primary<'allocator, 'input>> {
    let span = Span::start(cursor);
    
    let left = parse_primary_left(cursor)?;

    let mut chain = Vec::new_in(cursor.allocator);
    loop {
        let primary_right = match parse_primary_right(cursor) {
            Some(primary_right) => primary_right,
            _ => break
        };
        chain.push(primary_right);
    }

    return Some(Primary { left, chain, span: span.elapsed(cursor) });
}

fn parse_primary_left<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<PrimaryLeft<'allocator, 'input>> {
    let span = Span::start(cursor);

    let first_expr = if let Some(simple_primary) = parse_simple_primary(cursor) {
        PrimaryLeftExpr::Simple((simple_primary, parse_function_call(cursor)))
    } else if let Some(new_expression) = parse_new_expression(cursor) {
        PrimaryLeftExpr::NewExpression(new_expression)
    } else if let Some(if_expression) = parse_if_expression(cursor) {
        PrimaryLeftExpr::IfExpression(if_expression)
    } else if let Some(loop_expression) = parse_loop_expression(cursor) {
        PrimaryLeftExpr::LoopExpression(loop_expression)
    } else {
        return None;
    };

    let mapping_operator = parse_mapping_operator(cursor);

    return Some(PrimaryLeft { first_expr, mapping_operator, span: span.elapsed(cursor) });
}

fn parse_if_expression<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<IfExpression<'allocator, 'input>> {
    let span = Span::start(cursor);

    let if_statement = parse_if_statement(cursor)?;

    let mut chain = Vec::new_in(cursor.allocator);
    loop {
        let else_if_or_else = match parse_else_if_or_else(cursor) {
            Some(else_if_or_else) => else_if_or_else,
            _ => break
        };
        chain.push(else_if_or_else);
    }

    return Some(IfExpression { if_statement, chain, span: span.elapsed(cursor) });
}

fn parse_if_statement<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<IfStatement<'allocator, 'input>> {
    let span = Span::start(cursor);

    let if_keyword_token = cursor.next();
    if if_keyword_token.get_kind() != TokenKind::If {
        cursor.prev();
        return None;
    }
    let if_keyword_span = if_keyword_token.unwrap().span.clone();

    skip(cursor, &[TokenKind::LineFeed]);

    let condition = parse_expression(cursor).ok_or_else(|| { unexpected_token_error(cursor.allocator, cursor.current()) });

    skip(cursor, &[TokenKind::LineFeed]);

    let block = parse_with_recover(cursor, parse_block, &[TokenKind::BraceLeft, TokenKind::LineFeed, TokenKind::Semicolon]);

    return Some(IfStatement { if_keyword_span, condition, block, span: span.elapsed(cursor) });
}

fn parse_else_if_or_else<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<ElseIfOrElse<'allocator, 'input>> {
    let span = Span::start(cursor);

    let else_keyword_token = cursor.next();
    if else_keyword_token.get_kind() != TokenKind::Else {
        cursor.prev();
        return None;
    }
    let else_keyword_span = else_keyword_token.unwrap().span.clone();

    skip(cursor, &[TokenKind::LineFeed]);

    let else_if_or_else = parse_with_recover(cursor, parse_else_if_or_else_func, &[TokenKind::If, TokenKind::BraceLeft, TokenKind::LineFeed, TokenKind::Semicolon]);

    return Some(ElseIfOrElse { else_keyword_span, else_if_or_else, span: span.elapsed(cursor) });
}

fn parse_else_if_or_else_func<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<Either<IfStatement<'allocator, 'input>, Block<'allocator, 'input>>> {
    return if let Some(if_statement) = parse_if_statement(cursor) {
        Some(Either::Left(if_statement))
    } else if let Some(block) = parse_block(cursor) {
        Some(Either::Right(block))
    } else {
        None
    };
}

fn parse_loop_expression<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<LoopExpression<'allocator, 'input>> {
    let span = Span::start(cursor);

    if cursor.next().get_kind() != TokenKind::Loop {
        cursor.prev();
        return None;
    }

    let block = parse_block(cursor).ok_or_else(|| { unexpected_token_error(cursor.allocator, cursor.current()) });

    return Some(LoopExpression { block, span: span.elapsed(cursor) });
}

fn parse_primary_right<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<PrimaryRight<'allocator, 'input>> {
    let span = Span::start(cursor);

    let separator_token = cursor.next();
    let separator_kind = match separator_token.get_kind() {
        TokenKind::Dot         => PrimarySeparatorKind::Dot,
        TokenKind::DoubleColon => PrimarySeparatorKind::DoubleColon,
        _ => {
            cursor.prev();
            return None;
        }
    };
    let separator = PrimarySeparator::new(separator_kind, separator_token.unwrap().span.clone());

    skip(cursor, &[TokenKind::LineFeed]);

    let second_expr = match parse_literal(cursor) {
        Some(literal) => {
            let function_call = parse_function_call(cursor);
            Some((literal, function_call))
        },
        _ => None
    };

    let mapping_operator = parse_mapping_operator(cursor);

    return Some(PrimaryRight { separator, second_expr, mapping_operator, span: span.elapsed(cursor) });
}

fn parse_simple_primary<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<SimplePrimary<'allocator, 'input>> {
    return match cursor.current().get_kind() {
        TokenKind::ParenthesisLeft => {
            cursor.next();

            let expression = parse_expression(cursor).ok_or_else(|| { unexpected_token_error(cursor.allocator, cursor.current()) });

            let error_tokens = if cursor.next().get_kind() != TokenKind::ParenthesisRight {
                cursor.prev();
                read_until_token_found(cursor, &[TokenKind::ParenthesisRight])
            } else {
                Vec::new_in(cursor.allocator)
            };

            Some(SimplePrimary::Expression { expression, error_tokens })
        },
        TokenKind::Literal => Some(SimplePrimary::Identifier(parse_literal(cursor).unwrap())),
        TokenKind::Null    => Some(SimplePrimary::NullKeyword(cursor.next().unwrap().span.clone())),
        _ => None
    }
}

fn parse_function_call<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<FunctionCall<'allocator, 'input>> {
    let span = Span::start(cursor);

    let generics = if cursor.current().get_kind() == TokenKind::Colon {
        cursor.next();
        Some(parse_generics(cursor).ok_or_else(|| { unexpected_token_error(cursor.allocator, cursor.current()) }))
    } else {
        None
    };

    let mut error_tokens = Vec::new_in(cursor.allocator);

    if cursor.current().get_kind() != TokenKind::ParenthesisLeft {
        if generics.is_none() {
            return None;
        }

        error_tokens.extend(recover_until_token_found(cursor, &[TokenKind::ParenthesisLeft]));

        if cursor.current().get_kind() != TokenKind::ParenthesisLeft {
            return Some(FunctionCall { generics, error_tokens, arg_exprs: Err(ASTParseError::UnexpectedEOF), span: span.elapsed(cursor) });
        }
    }

    cursor.next();

    let mut arg_exprs = Vec::new_in(cursor.allocator);
    loop {
        skip(cursor, &[TokenKind::LineFeed]);

        let expression = match parse_expression(cursor) {
            Some(expression) => expression,
            _ => {
                let dropped_tokens = read_until_token_found(cursor, &[TokenKind::Comma, TokenKind::ParenthesisRight]);
                error_tokens.extend(dropped_tokens);

                match cursor.peek_prev().get_kind() {
                    TokenKind::Comma => continue,
                    TokenKind::ParenthesisRight => break,
                    _ => break
                }
            }
        };

        arg_exprs.push(expression);

        let comma_or_paren = cursor.next().get_kind();
        match comma_or_paren {
            TokenKind::ParenthesisRight => break,
            _ => {
                cursor.prev();
                continue;
            }
        }
    }

    return Some(FunctionCall { generics, error_tokens, arg_exprs: Ok(arg_exprs), span: span.elapsed(cursor) });
}

fn parse_new_expression<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<NewExpression<'allocator, 'input>> {
    let span = Span::start(cursor);
    
    let new_keyword_token = cursor.next();
    if new_keyword_token.get_kind() != TokenKind::New {
        cursor.prev();
        return None;
    }
    let new_keyword_span = new_keyword_token.unwrap().span.clone();
    
    let acyclic_keyword_token = cursor.next();
    let acyclic_keyword_span = if acyclic_keyword_token.get_kind() == TokenKind::Acyclic {
        acyclic_keyword_token.map(|token| { token.span.clone() })
    } else {
        cursor.prev();
        None
    };

    let mut path = Vec::new_in(cursor.allocator);
    let mut error_tokens = Vec::new_in(cursor.allocator);
    match parse_literal(cursor) {
        Some(literal) => path.push(literal),
        _ => error_tokens.extend(recover_until_token_found(cursor, &[TokenKind::BraceLeft, TokenKind::LineFeed]))
    }

    if error_tokens.is_empty() {
        loop {
            match cursor.current().get_kind() {
                TokenKind::DoubleColon => {
                    cursor.next();

                    match parse_literal(cursor) {
                        Some(literal) => path.push(literal),
                        _ => error_tokens.extend(recover_until_token_found(cursor, &[TokenKind::BraceLeft, TokenKind::LineFeed]))
                    };
                },
                TokenKind::BraceLeft => break,
                _ => error_tokens.extend(recover_until_token_found(cursor, &[TokenKind::BraceLeft, TokenKind::LineFeed]))
            }
        }
    }
    
    let brace_left_token = cursor.current();
    let field_assigns = if brace_left_token.get_kind() == TokenKind::BraceLeft {
        cursor.next();

        let mut field_assigns = Vec::new_in(cursor.allocator);

        loop {
            skip(cursor, &[TokenKind::LineFeed]);

            let name = match cursor.current().get_kind() {
                TokenKind::Literal => parse_literal(cursor).unwrap(),
                TokenKind::BraceRight => {
                    cursor.next();
                    break;
                },
                _ => {
                    error_tokens.extend(recover_until_token_found(cursor, &[TokenKind::BraceRight, TokenKind::Comma, TokenKind::LineFeed]));

                    match cursor.next().get_kind() {
                        TokenKind::BraceRight => break,
                        TokenKind::Comma => continue,
                        _ => {
                            cursor.prev();
                            break
                        }
                    }
                }
            };

            if cursor.next().get_kind() != TokenKind::Colon {
                cursor.prev();
                error_tokens.extend(recover_until_token_found(cursor, &[TokenKind::BraceRight, TokenKind::Comma, TokenKind::LineFeed]));

                match cursor.next().get_kind() {
                    TokenKind::BraceRight => break,
                    TokenKind::Comma => continue,
                    _ => {
                        cursor.prev();
                        break
                    }
                }
            }

            skip(cursor, &[TokenKind::LineFeed]);

            let expression = parse_expression(cursor).ok_or_else(|| { unexpected_token_error(&cursor.allocator, cursor.current()) });

            skip(cursor, &[TokenKind::LineFeed]);

            field_assigns.push(FieldAssign { name, expression });

            match cursor.next().get_kind() {
                TokenKind::BraceRight => break,
                TokenKind::Comma => continue,
                _ => {
                    cursor.prev();
                    error_tokens.extend(recover_until_token_found(cursor, &[TokenKind::BraceRight, TokenKind::Comma, TokenKind::LineFeed]));

                    match cursor.next().get_kind() {
                        TokenKind::BraceRight => break,
                        TokenKind::Comma => continue,
                        _ => {
                            cursor.prev();
                            break
                        }
                    }
                }
            }
        }

        Ok(field_assigns)
    } else {
        Err(unexpected_token_error(cursor.allocator, brace_left_token))
    };

    return Some(NewExpression { new_keyword_span, acyclic_keyword_span, path, error_tokens, field_assigns, span: span.elapsed(cursor) });
}

fn parse_mapping_operator<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<MappingOperator<'allocator, 'input>> {
    let span = Span::start(cursor);

    let first_token = cursor.next();
    let first_token_kind = first_token.get_kind();

    let mapping_operator_kind = match first_token_kind {
        TokenKind::InterrogationMark => {
            if cursor.current().get_kind() == TokenKind::ExclamationMark {
                cursor.next();
                MappingOperatorKind::NullUnwrap
            } else {
                MappingOperatorKind::NullPropagation
            }
        },
        TokenKind::ExclamationMark => {
            if cursor.current().get_kind() == TokenKind::ExclamationMark {
                cursor.next();
                MappingOperatorKind::ResultUnwrap
            } else {
                MappingOperatorKind::ResultPropagation
            }
        },
        TokenKind::InterrogationElvis => {
            let block_recovered = parse_with_recover(cursor, parse_block, &[TokenKind::ParenthesisLeft, TokenKind::LineFeed, TokenKind::Semicolon]);
            MappingOperatorKind::NullElvisBlock(block_recovered)
        },
        TokenKind::ExclamationElvis => {
            let block_recovered = parse_with_recover(cursor, parse_block, &[TokenKind::ParenthesisLeft, TokenKind::LineFeed, TokenKind::Semicolon]);
            MappingOperatorKind::ResultElvisBlock(block_recovered)
        },
        _ => {
            cursor.prev();
            return None;
        }
    };

    return Some(MappingOperator::new(mapping_operator_kind, span.elapsed(cursor)));
}

fn parse_return_expression<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<ReturnExpression<'allocator, 'input>> {
    let span = Span::start(cursor);

    let return_keyword_token = cursor.next();
    if return_keyword_token.get_kind() != TokenKind::Return {
        cursor.prev();
        return None;
    }
    let return_keyword_span = return_keyword_token.unwrap().span.clone();

    let expression = parse_expression(cursor);

    return Some(ReturnExpression { return_keyword_span, expression, span: span.elapsed(cursor) });
}

fn parse_closure<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<Closure<'allocator, 'input>> {
    let span = Span::start(cursor);

    let arguments = parse_closure_arguments(cursor)?;

    let mut error_tokens = Vec::new_in(cursor.allocator);
    let fat_arrow_span = if cursor.current().get_kind() == TokenKind::FatArrow {
        cursor.next();
        Ok(cursor.peek_prev().unwrap().span.clone())
    } else {
        error_tokens.extend(recover_until_token_found(cursor, &[TokenKind::FatArrow, TokenKind::BraceLeft, TokenKind::LineFeed, TokenKind::Semicolon]));
        if cursor.current().get_kind() == TokenKind::FatArrow {
            cursor.next();
            Ok(cursor.peek_prev().unwrap().span.clone())
        } else {
            Err(())
        }
    };

    let expression_or_block = parse_with_recover(cursor, parse_expression_or_block, &[TokenKind::BraceLeft, TokenKind::LineFeed, TokenKind::Semicolon]);

    return Some(Closure { arguments, error_tokens, fat_arrow_span, expression_or_block, span: span.elapsed(cursor) });
}

fn parse_expression_or_block<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<Either<Expression<'allocator, 'input>, Block<'allocator, 'input>>> {
    return if let Some(expression) = parse_expression(cursor) {
        Some(Either::Left(expression))
    } else if let Some(block) = parse_block(cursor) {
        Some(Either::Right(block))
    } else {
        None
    };
}

fn parse_closure_arguments<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<ClosureArguments<'allocator, 'input>> {
    let span = Span::start(cursor);

    let start_position = cursor.current_position;
    let first_token = cursor.next();
    let first_token_kind = first_token.get_kind();
    let first_token = first_token.cloned();

    return match first_token_kind {
        TokenKind::Literal => {
            let error_tokens = recover_until_token_found(cursor, &[TokenKind::FatArrow, TokenKind::LineFeed, TokenKind::Semicolon]);

            if cursor.current().get_kind() == TokenKind::FatArrow {
                let literal_token = first_token.unwrap();
                let arguments = Either::Left(Literal::new(literal_token.text, literal_token.span.clone()));
                Some(ClosureArguments { arguments, error_tokens, vertical_bar_right: Ok(()), span: span.elapsed(cursor) })
            } else {
                cursor.current_position = start_position;
                None
            }
        },
        TokenKind::VerticalBar => {
            let mut arguments = Vec::new_in(cursor.allocator);
            let mut error_tokens = Vec::new_in(cursor.allocator);
            let vertical_bar_right = loop {
                
                let argument = if let Some(function_argument) = parse_function_argument(cursor) {
                    Either::Left(function_argument)
                } else if let Some(literal) = parse_literal(cursor) {
                    Either::Right(literal)
                } else {
                    error_tokens.extend(read_until_token_found(cursor, &[TokenKind::Comma, TokenKind::VerticalBar, TokenKind::FatArrow, TokenKind::LineFeed, TokenKind::Semicolon]));
                    
                    match cursor.peek_prev().get_kind() {
                        TokenKind::Comma => {
                            skip(cursor, &[TokenKind::LineFeed]);
                            continue;
                        },
                        TokenKind::VerticalBar => break Ok(()),
                        TokenKind::FatArrow => {
                            cursor.prev();
                            break Err(unexpected_token_error(cursor.allocator, cursor.current()));
                        },
                        _ => break Err(unexpected_token_error(cursor.allocator, cursor.peek_prev()))
                    }
                };

                arguments.push(argument);

                let comma_or_bar = cursor.next().get_kind();
                match comma_or_bar {
                    TokenKind::VerticalBar => break Ok(()),
                    TokenKind::Comma => {
                        skip(cursor, &[TokenKind::LineFeed]);
                        continue
                    },
                    TokenKind::FatArrow => {
                        cursor.prev();
                        break Err(unexpected_token_error(cursor.allocator, cursor.current()));
                    },
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

            Some(ClosureArguments { arguments: Either::Right(arguments), error_tokens, vertical_bar_right, span: span.elapsed(cursor) })
        },
        _ => {
            cursor.current_position = start_position;
            None
        }
    }
}