use allocator_api2::vec::Vec;
use bumpalo::Bump;

use crate::{
    ast::{
        AddOrSub, AddOrSubExpression, AndExpression, Block, EqualOrNotEqual, EqualsExpression,
        Expression, Factor, FunctionCall, LessOrGreater, LessOrGreaterExpression, MappingOperator,
        MulOrDiv, MulOrDivExpression, OrExpression, Primary, PrimaryLeft, PrimaryLeftExpr,
        PrimaryRight, PrimaryRightExpr, PrimarySeparator, SimplePrimary, Spanned,
    },
    error::{ParseError, ParseErrorKind, recover_until},
    lexer::{GetKind, Lexer, TokenKind},
    parser::{literal::ParseAsLiteral, parse_program, types::parse_generics_info},
};

pub(crate) fn parse_expression<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<Expression<'input, 'allocator>> {
    if let Some(or_expression) = parse_or_expression(lexer, errors, allocator) {
        return Some(Expression::Or(allocator.alloc(or_expression)));
    }

    None
}

fn parse_or_expression<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<OrExpression<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let left = parse_and_expression(lexer, errors, allocator)?;

    let mut chain = Vec::new_in(allocator);

    loop {
        if lexer.current().get_kind() != TokenKind::Or {
            break;
        }
        lexer.next();

        let Some(right) = parse_and_expression(lexer, errors, allocator) else {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingExpressionIn2OpChain,
            );
            errors.push(error);

            break;
        };

        chain.push(right);
    }

    Some(OrExpression {
        left,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_and_expression<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<AndExpression<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let left = parse_equals_expression(lexer, errors, allocator)?;

    let mut chain = Vec::new_in(allocator);

    loop {
        if lexer.current().get_kind() != TokenKind::And {
            break;
        }
        lexer.next();

        let Some(right) = parse_equals_expression(lexer, errors, allocator) else {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingExpressionIn2OpChain,
            );
            errors.push(error);
            break;
        };

        chain.push(right);
    }

    Some(AndExpression {
        left,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_equals_expression<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<EqualsExpression<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let left = parse_less_or_greater_expression(lexer, errors, allocator)?;

    let mut chain = Vec::new_in(allocator);

    loop {
        let equals = match lexer.current().get_kind() {
            TokenKind::DoubleEqual => EqualOrNotEqual::Equal,
            TokenKind::NotEqual => EqualOrNotEqual::NotEqual,
            _ => break,
        };
        let equals = Spanned::new(equals, lexer.next().unwrap().span);

        let Some(right) = parse_less_or_greater_expression(lexer, errors, allocator) else {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingExpressionIn2OpChain,
            );
            errors.push(error);
            break;
        };

        chain.push((equals, right));
    }

    Some(EqualsExpression {
        left,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_less_or_greater_expression<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<LessOrGreaterExpression<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let left = parse_add_or_sub_expression(lexer, errors, allocator)?;

    let mut chain = Vec::new_in(allocator);

    loop {
        let less_or_greater = match lexer.current().get_kind() {
            TokenKind::LessThan => LessOrGreater::LessThan,
            TokenKind::GreaterThan => LessOrGreater::GreaterThan,
            TokenKind::LessThanOrEqual => LessOrGreater::LessThanOrEqual,
            TokenKind::GreaterThanOrEqual => LessOrGreater::GreaterThanOrEqual,
            _ => break,
        };
        let less_or_greater = Spanned::new(less_or_greater, lexer.next().unwrap().span);

        let Some(right) = parse_add_or_sub_expression(lexer, errors, allocator) else {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingExpressionIn2OpChain,
            );
            errors.push(error);
            break;
        };

        chain.push((less_or_greater, right));
    }

    Some(LessOrGreaterExpression {
        left,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_add_or_sub_expression<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<AddOrSubExpression<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let left = parse_mul_or_div_expression(lexer, errors, allocator)?;

    let mut chain = Vec::new_in(allocator);

    loop {
        let add_or_sub = match lexer.current().get_kind() {
            TokenKind::Plus => AddOrSub::Add,
            TokenKind::Minus => AddOrSub::Sub,
            _ => break,
        };
        let add_or_sub = Spanned::new(add_or_sub, lexer.next().unwrap().span);

        let Some(right) = parse_mul_or_div_expression(lexer, errors, allocator) else {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingExpressionIn2OpChain,
            );
            errors.push(error);
            break;
        };

        chain.push((add_or_sub, right));
    }

    Some(AddOrSubExpression {
        left,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_mul_or_div_expression<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<MulOrDivExpression<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let left = parse_factor(lexer, errors, allocator)?;

    let mut chain = Vec::new_in(allocator);

    loop {
        let mul_or_div = match lexer.current().get_kind() {
            TokenKind::Asterisk => MulOrDiv::Mul,
            TokenKind::Slash => MulOrDiv::Div,
            _ => break,
        };
        let mul_or_div = Spanned::new(mul_or_div, lexer.next().unwrap().span);

        let Some(right) = parse_factor(lexer, errors, allocator) else {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingExpressionIn2OpChain,
            );
            errors.push(error);
            break;
        };

        chain.push((mul_or_div, right));
    }

    Some(MulOrDivExpression {
        left,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_factor<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<Factor<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    match lexer.current().get_kind() {
        TokenKind::Minus => {
            let minus = Some(lexer.next().unwrap().span);

            let primary = parse_primary(lexer, errors, allocator).ok_or(());

            if primary.is_err() {
                let error = recover_until(
                    lexer,
                    &[TokenKind::LineFeed, TokenKind::SemiColon],
                    ParseErrorKind::MissingExpressionIn1Op,
                );
                errors.push(error);
            }

            Some(Factor {
                minus,
                primary,
                span: anchor.elapsed(lexer),
            })
        }
        _ => {
            let primary = parse_primary(lexer, errors, allocator)?;

            Some(Factor {
                minus: None,
                primary: Ok(primary),
                span: anchor.elapsed(lexer),
            })
        }
    }
}

fn parse_primary<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<Primary<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let left = parse_primary_left(lexer, errors, allocator)?;

    let mut chain = Vec::new_in(allocator);

    loop {
        let Some(right) = parse_primary_right(lexer, errors, allocator) else {
            break;
        };

        chain.push(right);
    }

    Some(Primary {
        left,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_primary_left<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<PrimaryLeft<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let first = parse_primary_left_expr(lexer, errors, allocator)?;

    let mapping_operator = parse_mapping_operator(lexer, errors, allocator);

    Some(PrimaryLeft {
        first,
        mapping_operator,
        span: anchor.elapsed(lexer),
    })
}

fn parse_primary_left_expr<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<PrimaryLeftExpr<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if let Some(left) = parse_simple_primary(lexer, errors, allocator) {
        let generics = match lexer.current().get_kind() {
            TokenKind::Colon => {
                lexer.next();

                let generics_info = parse_generics_info(lexer, errors, allocator);

                if generics_info.is_none() {
                    let error = recover_until(
                        lexer,
                        &[
                            TokenKind::ParenthesesLeft,
                            TokenKind::LineFeed,
                            TokenKind::SemiColon,
                        ],
                        ParseErrorKind::MissingGenericsInfoAfterColon,
                    );
                    errors.push(error);
                }

                generics_info
            }
            _ => None,
        };

        let function_call = parse_function_call(lexer, errors, allocator);

        return Some(PrimaryLeftExpr::Simple {
            left,
            generics,
            function_call,
            span: anchor.elapsed(lexer),
        });
    }

    None
}

fn parse_simple_primary<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<SimplePrimary<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let simple_primary = match lexer.current().get_kind() {
        TokenKind::ParenthesesLeft => {
            lexer.next();

            let mut expressions = Vec::new_in(allocator);

            loop {
                let Some(expression) = parse_expression(lexer, errors, allocator) else {
                    break;
                };

                expressions.push(expression);

                if lexer.current().get_kind() != TokenKind::Comma {
                    break;
                }
                lexer.next();
            }

            if lexer.current().get_kind() != TokenKind::ParenthesesRight {
                let error = recover_until(
                    lexer,
                    &[TokenKind::ParenthesesRight],
                    ParseErrorKind::InvalidTupleExprFormat,
                );
                errors.push(error);
            }

            if lexer.current().get_kind() != TokenKind::ParenthesesRight {
                let error = ParseError {
                    kind: ParseErrorKind::UnclosedParen,
                    span: lexer.cast_anchor().elapsed(lexer),
                };
                errors.push(error);
            }

            if lexer.current().get_kind() == TokenKind::ParenthesesRight {
                lexer.next();
            }

            SimplePrimary::Tuple {
                expressions,
                span: anchor.elapsed(lexer),
            }
        }
        TokenKind::Literal => SimplePrimary::Literal(lexer.parse_as_literal()),
        TokenKind::StringLiteral => SimplePrimary::StringLiteral(lexer.parse_as_literal()),
        TokenKind::Null => SimplePrimary::Null(lexer.next().unwrap().span),
        TokenKind::True => SimplePrimary::True(lexer.next().unwrap().span),
        TokenKind::False => SimplePrimary::False(lexer.next().unwrap().span),
        TokenKind::This => SimplePrimary::This(lexer.next().unwrap().span),
        TokenKind::LargeThis => SimplePrimary::LargeThis(lexer.next().unwrap().span),
        _ => return None,
    };

    Some(simple_primary)
}

fn parse_mapping_operator<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<MappingOperator<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let mapping_operator = match lexer.current().get_kind() {
        TokenKind::QuestionMark => {
            lexer.next();

            match lexer.current().get_kind() {
                TokenKind::ExclamationMark => {
                    lexer.next();

                    MappingOperator::NullUnwrap {
                        span: anchor.elapsed(lexer),
                    }
                }
                _ => MappingOperator::NullPropagation {
                    span: anchor.elapsed(lexer),
                },
            }
        }
        TokenKind::ExclamationMark => {
            lexer.next();

            match lexer.current().get_kind() {
                TokenKind::ExclamationMark => {
                    lexer.next();

                    MappingOperator::ResultUnwrap {
                        span: anchor.elapsed(lexer),
                    }
                }
                _ => MappingOperator::ResultPropagation {
                    span: anchor.elapsed(lexer),
                },
            }
        }
        TokenKind::QuestionElvis => {
            lexer.next();

            let block = parse_block(lexer, errors, allocator);

            if block.is_none() {
                let error = ParseError {
                    kind: ParseErrorKind::MissingBlockInElvis,
                    span: anchor.elapsed(lexer),
                };
                errors.push(error);
            }

            MappingOperator::NullElvis {
                block: block.ok_or(()),
                span: anchor.elapsed(lexer),
            }
        }
        TokenKind::ExclamationElvis => {
            lexer.next();

            let block = parse_block(lexer, errors, allocator);

            if block.is_none() {
                let error = ParseError {
                    kind: ParseErrorKind::MissingBlockInElvis,
                    span: anchor.elapsed(lexer),
                };
                errors.push(error);
            }

            MappingOperator::ResultElvis {
                block: block.ok_or(()),
                span: anchor.elapsed(lexer),
            }
        }
        _ => return None,
    };

    Some(mapping_operator)
}

fn parse_primary_right<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<PrimaryRight<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    lexer.skip_line_feed();

    let separator = match lexer.current().get_kind() {
        TokenKind::Dot => PrimarySeparator::Dot,
        TokenKind::DoubleColon => PrimarySeparator::DoubleColon,
        _ => {
            // not primary right
            lexer.back_to_anchor(anchor);
            return None;
        }
    };

    let separator = Spanned::new(separator, lexer.next().unwrap().span);

    let mut second = None;

    if lexer.current().get_kind() == TokenKind::Literal {
        let literal = lexer.parse_as_literal();

        let generics = match lexer.current().get_kind() {
            TokenKind::Colon => {
                lexer.next();

                let generics = parse_generics_info(lexer, errors, allocator);

                if generics.is_none() {
                    let error = recover_until(
                        lexer,
                        &[
                            TokenKind::ParenthesesLeft,
                            TokenKind::LineFeed,
                            TokenKind::SemiColon,
                        ],
                        ParseErrorKind::MissingGenericsInfoAfterColon,
                    );
                    errors.push(error);
                }

                generics
            }
            _ => None,
        };

        let function_call = parse_function_call(lexer, errors, allocator);

        second = Some(PrimaryRightExpr {
            literal,
            generics,
            function_call,
        });
    }

    let mapping_operator = parse_mapping_operator(lexer, errors, allocator);

    Some(PrimaryRight {
        separator,
        second,
        mapping_operator,
        span: anchor.elapsed(lexer),
    })
}

fn parse_function_call<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<FunctionCall<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::ParenthesesLeft {
        return None;
    }
    lexer.next();

    let mut arguments = Vec::new_in(allocator);

    loop {
        let Some(argument) = parse_expression(lexer, errors, allocator) else {
            break;
        };

        arguments.push(argument);

        if lexer.current().get_kind() != TokenKind::Comma {
            break;
        }
        lexer.next();
    }

    if lexer.current().get_kind() != TokenKind::ParenthesesRight {
        let error = recover_until(
            lexer,
            &[TokenKind::ParenthesesRight],
            ParseErrorKind::InvalidFunctionCallFormat,
        );
        errors.push(error);
    }

    if lexer.current().get_kind() != TokenKind::ParenthesesRight {
        let error = ParseError {
            kind: ParseErrorKind::UnclosedParen,
            span: lexer.cast_anchor().elapsed(lexer),
        };
        errors.push(error);
    }

    if lexer.current().get_kind() == TokenKind::ParenthesesRight {
        lexer.next();
    }

    Some(FunctionCall {
        arguments,
        span: anchor.elapsed(lexer),
    })
}

fn parse_block<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<Block<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::BraceLeft {
        return None;
    }
    lexer.next();

    let program = parse_program(lexer, &[TokenKind::BraceRight], errors, allocator);

    if lexer.current().get_kind() != TokenKind::BraceRight {
        let error = ParseError {
            kind: ParseErrorKind::UnclosedBrace,
            span: lexer.cast_anchor().elapsed(lexer),
        };
        errors.push(error);
    }

    if lexer.current().get_kind() == TokenKind::BraceRight {
        lexer.next();
    }

    Some(Block {
        program,
        span: anchor.elapsed(lexer),
    })
}
