use allocator_api2::vec::Vec;
use bumpalo::Bump;

use crate::{
    ast::{
        Assignment, Define, DefineWithAttribute, Documents, FunctionDefine, ImportStatement,
        Spanned, Statement, StatementAttribute, StatementWithTagAndDocs, SwapStatement,
        UserTypeDefine, VariableDefine,
    },
    error::{ParseError, ParseErrorKind, recover_until},
    lexer::{GetKind, Lexer, TokenKind},
    parser::{expression::parse_expression, literal::ParseAsLiteral},
};

pub(crate) fn parse_statement_with_tag_and_docs<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<StatementWithTagAndDocs<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let documents = parse_documents(lexer, allocator);

    let Some(statement) = parse_statement(lexer, errors, allocator) else {
        lexer.back_to_anchor(anchor);
        return None;
    };

    Some(StatementWithTagAndDocs {
        documents,
        statement,
        span: anchor.elapsed(lexer),
    })
}

pub(crate) fn parse_documents<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    allocator: &'allocator Bump,
) -> Documents<'input, 'allocator> {
    let anchor = lexer.cast_anchor();
    let mut documents = Vec::new_in(allocator);

    loop {
        if lexer.current().get_kind() != TokenKind::Document {
            break;
        }
        documents.push(lexer.next().unwrap().text);
    }
    let documents = allocator.alloc(documents).as_slice();

    Documents {
        documents,
        span: anchor.elapsed(lexer),
    }
}

fn parse_statement<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<Statement<'input, 'allocator>> {
    if let Some(assignment) = parse_assignment(lexer, errors, allocator) {
        return Some(Statement::Assignment(assignment));
    }

    if let Some(swap) = parse_swap_statement(lexer, errors, allocator) {
        return Some(Statement::Swap(swap));
    }

    if let Some(import) = parse_import_statement(lexer, errors, allocator) {
        return Some(Statement::Import(import));
    }

    if let Some(expression) = parse_expression(lexer, errors, allocator) {
        return Some(Statement::Expression(expression));
    }

    None
}

fn parse_assignment<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<Assignment<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let Some(left) = parse_expression(lexer, errors, allocator) else {
        return None;
    };

    if lexer.current().get_kind() != TokenKind::Equal {
        lexer.back_to_anchor(anchor);
        return None;
    }
    lexer.next();

    let right = match parse_expression(lexer, errors, allocator) {
        Some(right) => Ok(right),
        None => {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingRightExprInAssign,
            );
            errors.push(error);

            Err(())
        }
    };

    Some(Assignment {
        left,
        right,
        span: anchor.elapsed(lexer),
    })
}

fn parse_swap_statement<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<SwapStatement<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let Some(left) = parse_expression(lexer, errors, allocator) else {
        lexer.back_to_anchor(anchor);
        return None;
    };
    lexer.next();

    let right = match parse_expression(lexer, errors, allocator) {
        Some(right) => Ok(right),
        None => {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingRightExprInSwap,
            );
            errors.push(error);

            Err(())
        }
    };

    Some(SwapStatement {
        left,
        right,
        span: anchor.elapsed(lexer),
    })
}

fn parse_import_statement<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<ImportStatement<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Import {
        return None;
    }
    let import = lexer.next().unwrap().span;

    let mut path = Vec::new_in(allocator);

    match lexer.current().get_kind() {
        TokenKind::Literal => {
            path.push(lexer.parse_as_literal());
        }
        _ => {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingPathInImport,
            );
            errors.push(error);
        }
    }

    let mut elements = None;

    loop {
        if lexer.current().get_kind() != TokenKind::DoubleColon {
            break;
        }
        lexer.next();

        match lexer.current().get_kind() {
            TokenKind::Literal => {
                path.push(lexer.parse_as_literal());
            }
            TokenKind::BraceLeft => {
                lexer.next();

                let mut import_elements = Vec::new_in(allocator);

                loop {
                    lexer.skip_line_feed();

                    match lexer.current().get_kind() {
                        TokenKind::Literal => {
                            import_elements.push(lexer.parse_as_literal());
                        }
                        _ => break,
                    }

                    match lexer.current().get_kind() {
                        TokenKind::Comma => {
                            lexer.next();
                        }
                        _ => break,
                    }
                }

                lexer.skip_line_feed();

                if lexer.current().get_kind() != TokenKind::BraceRight {
                    let error = recover_until(
                        lexer,
                        &[TokenKind::BraceRight],
                        ParseErrorKind::InvalidImportElementsOrUnclosed,
                    );
                    errors.push(error);
                }
                lexer.next();

                elements = Some(allocator.alloc(import_elements).as_slice());
            }
            _ => break,
        }
    }

    let path = allocator.alloc(path).as_slice();

    Some(ImportStatement {
        import,
        path,
        elements,
        span: anchor.elapsed(lexer),
    })
}

fn parse_define_with_attribute<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<DefineWithAttribute<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let mut attributes = Vec::new_in(allocator);

    loop {
        let attribute = match lexer.current().get_kind() {
            TokenKind::Static => {
                Spanned::new(StatementAttribute::Static, lexer.next().unwrap().span)
            }
            TokenKind::Private => {
                Spanned::new(StatementAttribute::Private, lexer.next().unwrap().span)
            }
            TokenKind::Suspend => {
                Spanned::new(StatementAttribute::Suspend, lexer.next().unwrap().span)
            }
            TokenKind::Native => {
                Spanned::new(StatementAttribute::Native, lexer.next().unwrap().span)
            }
            TokenKind::Acyclic => {
                Spanned::new(StatementAttribute::Acyclic, lexer.next().unwrap().span)
            }
            TokenKind::Open => Spanned::new(StatementAttribute::Open, lexer.next().unwrap().span),
            TokenKind::Override => {
                Spanned::new(StatementAttribute::Override, lexer.next().unwrap().span)
            }
            _ => break,
        };
        attributes.push(attribute);
    }

    if attributes.is_empty() {
        return None;
    }

    let attribute = allocator.alloc(attributes).as_slice();

    let define = match parse_define(lexer, errors, allocator) {
        Some(define) => Ok(define),
        None => {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingDefineAfterAttribute,
            );
            errors.push(error);

            Err(())
        }
    };

    Some(DefineWithAttribute {
        attribute,
        define,
        span: anchor.elapsed(lexer),
    })
}

fn parse_define<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<Define<'input, 'allocator>> {
    if let Some(function) = parse_function_define(lexer, errors, allocator) {
        return Some(Define::Function(function));
    }

    if let Some(user_type) = parse_user_type_define(lexer, errors, allocator) {
        return Some(Define::UserType(user_type));
    }

    if let Some(variable) = parse_variable_define(lexer, errors, allocator) {
        return Some(Define::Variable(variable));
    }

    None
}

fn parse_function_define<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<FunctionDefine<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Function {
        return None;
    }
    let function = lexer.next().unwrap().span;

    Some(FunctionDefine {
        function,
        generics: (),
        name: (),
        arguments: (),
        return_type: (),
        where_clause: (),
        block: (),
        span: (),
    })
}

fn parse_user_type_define<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<UserTypeDefine<'input, 'allocator>> {
    todo!()
}

fn parse_variable_define<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<VariableDefine<'input, 'allocator>> {
    todo!()
}
