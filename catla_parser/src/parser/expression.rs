use allocator_api2::vec::Vec;
use bumpalo::Bump;

use crate::{
    ast::{
        AddOrSub, AddOrSubExpression, AndExpression, Block, ElseChain, EqualOrNotEqual,
        EqualsExpression, Expression, Factor, FieldAssign, FieldAssignElement, FunctionCall,
        IfExpression, IfStatement, LessOrGreater, LessOrGreaterExpression, LoopExpression,
        MappingOperator, MulOrDiv, MulOrDivExpression, NewArrayExpression, NewArrayInitExpression,
        NewObjectExpression, OrExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight,
        PrimaryRightExpr, PrimarySeparator, SimplePrimary, Spanned,
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
    let chain = allocator.alloc(chain).as_slice();

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
    let chain = allocator.alloc(chain).as_slice();

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
    let chain = allocator.alloc(chain).as_slice();

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
    let chain = allocator.alloc(chain).as_slice();

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
    let chain = allocator.alloc(chain).as_slice();

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
    let chain = allocator.alloc(chain).as_slice();

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
    let chain = allocator.alloc(chain).as_slice();

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

    if let Some(new_array_init_expression) = parse_new_array_init_expr(lexer, errors, allocator) {
        return Some(PrimaryLeftExpr::NewArrayInit {
            new_array_init: new_array_init_expression,
        });
    }

    if let Some(new_array_expression) = parse_new_array_expression(lexer, errors, allocator) {
        return Some(PrimaryLeftExpr::NewArray {
            new_array: new_array_expression,
        });
    }

    if let Some(new_object_expr) = parse_new_object_expression(lexer, errors, allocator) {
        return Some(PrimaryLeftExpr::NewObject {
            new_object: new_object_expr,
        });
    }

    if let Some(if_expression) = parse_if_expression(lexer, errors, allocator) {
        return Some(PrimaryLeftExpr::If { if_expression });
    }

    if let Some(loop_expression) = parse_loop_expression(lexer, errors, allocator) {
        return Some(PrimaryLeftExpr::Loop { loop_expression });
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
            let expressions = allocator.alloc(expressions).as_slice();

            if lexer.current().get_kind() != TokenKind::ParenthesesRight {
                let error = recover_until(
                    lexer,
                    &[TokenKind::ParenthesesRight],
                    ParseErrorKind::InvalidTupleExprFormatOrUnclosedParen,
                );
                errors.push(error);
            }
            lexer.next();

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
    let arguments = allocator.alloc(arguments).as_slice();

    if lexer.current().get_kind() != TokenKind::ParenthesesRight {
        let error = recover_until(
            lexer,
            &[TokenKind::ParenthesesRight],
            ParseErrorKind::InvalidFunctionCallFormatOrUnclosedParen,
        );
        errors.push(error);
    }
    lexer.next();

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

fn parse_new_object_expression<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<NewObjectExpression<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::New {
        return None;
    }
    let new = lexer.next().unwrap().span;

    let acyclic = match lexer.current().get_kind() {
        TokenKind::Acyclic => Some(lexer.next().unwrap().span),
        _ => None,
    };

    let first_literal = match lexer.current().get_kind() {
        TokenKind::LargeThis | TokenKind::Literal => lexer.parse_as_literal(),
        TokenKind::BracketLeft => {
            // maybe NewArrayInitExpression
            lexer.back_to_anchor(anchor);
            return None;
        }
        _ => {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingTypeNameInNewObjectExpr,
            );
            errors.push(error);

            return Some(NewObjectExpression {
                new,
                acyclic,
                path: allocator.alloc(Vec::new_in(allocator)),
                field_assign: FieldAssign {
                    elements: allocator.alloc(Vec::new_in(allocator)),
                    span: lexer.cast_anchor().elapsed(lexer),
                },
                span: anchor.elapsed(lexer),
            });
        }
    };

    let mut path = Vec::new_in(allocator);
    path.push(first_literal);

    loop {
        if lexer.current().get_kind() != TokenKind::DoubleColon {
            break;
        }
        let double_colon = lexer.next().unwrap();

        if lexer.current().get_kind() != TokenKind::Literal {
            let error = ParseError {
                kind: ParseErrorKind::ExtraDoubleColonInPath,
                span: double_colon.span,
            };
            errors.push(error);
            break;
        }
        let literal = lexer.parse_as_literal();

        path.push(literal);
    }
    let path = allocator.alloc(path).as_slice();

    lexer.skip_line_feed();

    let field_assign_anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::BraceLeft {
        let error = recover_until(
            lexer,
            &[TokenKind::LineFeed, TokenKind::SemiColon],
            ParseErrorKind::MissingFieldAssignInNewObjectExpr,
        );
        errors.push(error);

        return Some(NewObjectExpression {
            new,
            acyclic,
            path,
            field_assign: FieldAssign {
                elements: allocator.alloc(Vec::new_in(allocator)).as_slice(),
                span: lexer.cast_anchor().elapsed(lexer),
            },
            span: anchor.elapsed(lexer),
        });
    }
    lexer.next();

    lexer.skip_line_feed();

    let mut elements = Vec::new_in(allocator);

    loop {
        let anchor = lexer.cast_anchor();

        let literal = match lexer.current().get_kind() {
            TokenKind::Literal => lexer.parse_as_literal(),
            _ => break,
        };

        if lexer.current().get_kind() != TokenKind::Colon {
            let error = recover_until(
                lexer,
                &[TokenKind::Comma, TokenKind::BraceRight],
                ParseErrorKind::MissingColonInFieldAssign,
            );
            errors.push(error);

            continue;
        }
        lexer.next();

        let Some(expression) = parse_expression(lexer, errors, allocator) else {
            let error = recover_until(
                lexer,
                &[TokenKind::Comma, TokenKind::BraceRight],
                ParseErrorKind::MissingExpressionInFieldAssign,
            );
            errors.push(error);

            continue;
        };
        elements.push(FieldAssignElement {
            field: literal,
            expression,
            span: anchor.elapsed(lexer),
        });

        if lexer.current().get_kind() != TokenKind::Comma
            && lexer.current().get_kind() != TokenKind::LineFeed
        {
            break;
        }
        lexer.next();

        lexer.skip_line_feed();
    }
    let elements = allocator.alloc(elements).as_slice();

    if lexer.current().get_kind() != TokenKind::BraceRight {
        let error = recover_until(
            lexer,
            &[TokenKind::BraceRight],
            ParseErrorKind::InvalidFieldAssignFormatOrUnclosedBrace,
        );
        errors.push(error);
    }
    lexer.next();

    Some(NewObjectExpression {
        new,
        acyclic,
        path,
        field_assign: FieldAssign {
            elements,
            span: field_assign_anchor.elapsed(lexer),
        },
        span: anchor.elapsed(lexer),
    })
}

fn parse_new_array_init_expr<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<NewArrayInitExpression<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::New {
        return None;
    }
    let new = lexer.next().unwrap().span;

    let acyclic = match lexer.current().get_kind() {
        TokenKind::Acyclic => Some(lexer.next().unwrap().span),
        _ => None,
    };

    let for_keyword = match lexer.current().get_kind() {
        TokenKind::For => Some(lexer.next().unwrap().span),
        _ => None,
    };

    if lexer.current().get_kind() != TokenKind::BracketLeft {
        lexer.back_to_anchor(anchor);
        return None;
    }
    lexer.next();

    let init_expression = match parse_expression(lexer, errors, allocator) {
        Some(expression) => Ok(expression),
        None => {
            let error = recover_until(
                lexer,
                &[TokenKind::SemiColon, TokenKind::BracketRight],
                ParseErrorKind::MissingInitExpression,
            );
            errors.push(error);

            if let TokenKind::BracketRight | TokenKind::None = lexer.current().get_kind() {
                lexer.next();
                return Some(NewArrayInitExpression {
                    new,
                    acyclic,
                    for_keyword,
                    init_expression: Err(()),
                    length_expression: Err(()),
                    span: anchor.elapsed(lexer),
                });
            }

            Err(())
        }
    };

    if lexer.current().get_kind() != TokenKind::SemiColon {
        let error = recover_until(
            lexer,
            &[TokenKind::SemiColon, TokenKind::BracketRight],
            ParseErrorKind::MissingSemiColonInArrayInitExpression,
        );
        errors.push(error);

        if let TokenKind::BracketRight | TokenKind::None = lexer.current().get_kind() {
            lexer.next();
            return Some(NewArrayInitExpression {
                new,
                acyclic,
                for_keyword,
                init_expression,
                length_expression: Err(()),
                span: anchor.elapsed(lexer),
            });
        }
    }
    lexer.next();

    let length_expression = match parse_expression(lexer, errors, allocator) {
        Some(expression) => Ok(expression),
        None => {
            let error = recover_until(
                lexer,
                &[TokenKind::BracketRight],
                ParseErrorKind::MissingLengthExpression,
            );
            errors.push(error);
            Err(())
        }
    };

    if lexer.current().get_kind() == TokenKind::BracketRight {
        lexer.next();
    }

    Some(NewArrayInitExpression {
        new,
        acyclic,
        for_keyword,
        init_expression,
        length_expression,
        span: anchor.elapsed(lexer),
    })
}

fn parse_new_array_expression<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<NewArrayExpression<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::New {
        return None;
    }
    let new = lexer.next().unwrap().span;

    let acyclic = match lexer.current().get_kind() {
        TokenKind::Acyclic => Some(lexer.next().unwrap().span),
        _ => None,
    };

    if lexer.current().get_kind() != TokenKind::BraceLeft {
        lexer.back_to_anchor(anchor);
        return None;
    }
    lexer.next();

    let mut elements = Vec::new_in(allocator);
    loop {
        let Some(expression) = parse_expression(lexer, errors, allocator) else {
            break;
        };
        elements.push(expression);

        match lexer.current().get_kind() {
            TokenKind::Comma => {
                lexer.next();
            }
            TokenKind::BraceRight => break,
            _ => {}
        }
    }
    let elements = allocator.alloc(elements).as_slice();

    if lexer.current().get_kind() != TokenKind::BraceRight {
        let error = recover_until(
            lexer,
            &[TokenKind::Comma, TokenKind::BraceRight],
            ParseErrorKind::InvalidExpressionInNewArrayExprOrUnclosedBrace,
        );
        errors.push(error);
    }
    lexer.next();

    Some(NewArrayExpression {
        new,
        acyclic,
        elements,
        span: anchor.elapsed(lexer),
    })
}

fn parse_if_expression<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<IfExpression<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let Some(first) = parse_if_statement(lexer, errors, allocator) else {
        return None;
    };

    let mut chain = Vec::new_in(allocator);
    loop {
        if lexer.current().get_kind() != TokenKind::Else {
            break;
        }
        lexer.next();

        let Some(else_chain) = parse_else_chain(lexer, errors, allocator) else {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingElseChain,
            );
            errors.push(error);
            break;
        };
        chain.push(else_chain);
    }
    let chain = allocator.alloc(chain).as_slice();

    Some(IfExpression {
        first,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_if_statement<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<IfStatement<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::If {
        return None;
    }
    let if_keyword = lexer.next().unwrap().span;

    let condition = match parse_expression(lexer, errors, allocator) {
        Some(condition) => Ok(condition),
        None => {
            let error = recover_until(
                lexer,
                &[TokenKind::BraceLeft, TokenKind::LineFeed],
                ParseErrorKind::MissingIfCondition,
            );
            errors.push(error);

            match lexer.current().get_kind() {
                TokenKind::BraceLeft => Err(()),
                _ => {
                    return Some(IfStatement {
                        if_keyword,
                        condition: Err(()),
                        block: Err(()),
                        span: anchor.elapsed(lexer),
                    });
                }
            }
        }
    };

    let block = match parse_block(lexer, errors, allocator) {
        Some(block) => Ok(block),
        None => {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingIfBlock,
            );
            errors.push(error);

            Err(())
        }
    };

    Some(IfStatement {
        if_keyword,
        condition,
        block,
        span: anchor.elapsed(lexer),
    })
}

fn parse_else_chain<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<ElseChain<'input, 'allocator>> {
    if let Some(if_statement) = parse_if_statement(lexer, errors, allocator) {
        return Some(ElseChain::ElseIf { if_statement });
    }
    if let Some(block) = parse_block(lexer, errors, allocator) {
        return Some(ElseChain::Else { block });
    }

    None
}

fn parse_loop_expression<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<LoopExpression<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Loop {
        return None;
    }
    let loop_keyword = lexer.next().unwrap().span;

    let block = match parse_block(lexer, errors, allocator) {
        Some(block) => Ok(block),
        None => {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingLoopBlock,
            );
            errors.push(error);

            Err(())
        }
    };

    Some(LoopExpression {
        loop_keyword,
        block,
        span: anchor.elapsed(lexer),
    })
}
