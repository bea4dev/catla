use allocator_api2::vec::Vec;
use bumpalo::Bump;

use crate::{
    ast::{
        Assignment, CompilerTag, Define, DefineWithAttribute, Documents, DropStatement,
        ElementsOrWildCard, FunctionArgument, FunctionArguments, FunctionDefine, Implements,
        ImportStatement, LetVar, Spanned, Statement, StatementAttribute, StatementWithTagAndDocs,
        SuperTypeInfo, SwapStatement, ThisMutability, TypeAlias, UserTypeDefine, UserTypeKind,
        VariableBinding, VariableDefine,
    },
    error::{ParseError, ParseErrorKind, recover_until},
    lexer::{GetKind, Lexer, TokenKind},
    parser::{
        expression::{parse_block, parse_expression},
        literal::ParseAsLiteral,
        types::{
            ParseTypeTagKind, parse_generics_define, parse_type_info, parse_type_tag,
            parse_where_clause,
        },
    },
};

pub(crate) fn parse_statement_with_tag_and_docs<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<StatementWithTagAndDocs<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let documents = parse_documents(lexer, allocator);

    let compiler_tags = parse_compiler_tags(lexer, errors, allocator);

    let Some(statement) = parse_statement(lexer, errors, allocator) else {
        lexer.back_to_anchor(anchor);
        return None;
    };

    Some(StatementWithTagAndDocs {
        documents,
        compiler_tags,
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

pub(crate) fn parse_compiler_tags<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> &'allocator [CompilerTag<'input>] {
    let mut compiler_tags = Vec::new_in(allocator);

    loop {
        let anchor = lexer.cast_anchor();

        if lexer.current().get_kind() != TokenKind::Hash {
            break;
        }
        lexer.next();

        if lexer.current().get_kind() != TokenKind::BracketLeft {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed],
                ParseErrorKind::InvalidCompilerTagFormat,
            );
            errors.push(error);
            continue;
        }
        lexer.next();

        if lexer.current().get_kind() != TokenKind::Literal {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed],
                ParseErrorKind::InvalidCompilerTagFormat,
            );
            errors.push(error);
            continue;
        }
        let literal = lexer.parse_as_literal();

        if lexer.current().get_kind() != TokenKind::BracketRight {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed],
                ParseErrorKind::InvalidCompilerTagFormat,
            );
            errors.push(error);
        } else {
            lexer.next();
        }

        lexer.skip_line_feed();

        compiler_tags.push(CompilerTag {
            literal,
            span: anchor.elapsed(lexer),
        });
    }

    if compiler_tags.is_empty() {
        &[]
    } else {
        allocator.alloc(compiler_tags).as_slice()
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

    if let Some(define_with_attribute) = parse_define_with_attribute(lexer, errors, allocator) {
        return Some(Statement::DefineWithAttribute(define_with_attribute));
    }

    if let Some(drop) = parse_drop_statement(lexer, errors, allocator) {
        return Some(Statement::Drop(drop));
    }

    if let Some(expression) = parse_expression(lexer, errors, allocator) {
        return Some(Statement::Expression(expression));
    }

    if let Some(implements) = parse_implements(lexer, errors, allocator) {
        return Some(Statement::Implements(implements));
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
        return None;
    };

    if lexer.current().get_kind() != TokenKind::Swap {
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

    let mut elements_or_wild_card = None;

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

                elements_or_wild_card = Some(ElementsOrWildCard::Elements(
                    allocator.alloc(import_elements).as_slice(),
                ));

                break;
            }
            TokenKind::Asterisk => {
                elements_or_wild_card =
                    Some(ElementsOrWildCard::WildCard(lexer.parse_as_literal()));
                break;
            }
            _ => break,
        }
    }

    let path = allocator.alloc(path).as_slice();

    Some(ImportStatement {
        import,
        path,
        elements_or_wild_card,
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

    let attribute = allocator.alloc(attributes).as_slice();

    let define = match parse_define(lexer, errors, allocator) {
        Some(define) => Ok(define),
        None => {
            if attribute.is_empty() {
                lexer.back_to_anchor(anchor);
                return None;
            } else {
                let error = recover_until(
                    lexer,
                    &[TokenKind::LineFeed, TokenKind::SemiColon],
                    ParseErrorKind::MissingDefineAfterAttribute,
                );
                errors.push(error);

                Err(())
            }
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

    if let Some(type_alias) = parse_type_alias(lexer, errors, allocator) {
        return Some(Define::TypeAlias(type_alias));
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

    let generics = parse_generics_define(lexer, errors, allocator);

    if lexer.current().get_kind() != TokenKind::Literal {
        let error = recover_until(
            lexer,
            &[TokenKind::LineFeed, TokenKind::SemiColon],
            ParseErrorKind::MissingFunctionName,
        );
        errors.push(error);

        return Some(FunctionDefine {
            function,
            generics,
            name: Err(()),
            arguments: Err(()),
            return_type: None,
            where_clause: None,
            block: None,
            span: anchor.elapsed(lexer),
        });
    }
    let name = lexer.parse_as_literal();

    let Some(arguments) = parse_function_arguments(lexer, errors, allocator) else {
        let error = recover_until(
            lexer,
            &[TokenKind::LineFeed, TokenKind::SemiColon],
            ParseErrorKind::MissingFunctionArguments,
        );
        errors.push(error);

        return Some(FunctionDefine {
            function,
            generics,
            name: Ok(name),
            arguments: Err(()),
            return_type: None,
            where_clause: None,
            block: None,
            span: anchor.elapsed(lexer),
        });
    };

    let return_type = parse_type_tag(ParseTypeTagKind::Arrow, lexer, errors, allocator);

    let where_clause = parse_where_clause(lexer, errors, allocator);

    let block = match lexer.current().get_kind() {
        TokenKind::BraceLeft => parse_block(lexer, errors, allocator),
        TokenKind::SemiColon => {
            lexer.next();
            None
        }
        _ => {
            let error = ParseError {
                kind: ParseErrorKind::MissingBlockOrSemiClonInFunctionDefine,
                span: anchor.elapsed(lexer),
            };
            errors.push(error);
            None
        }
    };

    Some(FunctionDefine {
        function,
        generics,
        name: Ok(name),
        arguments: Ok(arguments),
        return_type,
        where_clause,
        block,
        span: anchor.elapsed(lexer),
    })
}

fn parse_function_arguments<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<FunctionArguments<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::ParenthesesLeft {
        return None;
    }
    lexer.next();

    lexer.skip_line_feed();

    let this_mutability = parse_this_mutability(lexer, errors);

    if let Some(_) = &this_mutability {
        if lexer.current().get_kind() == TokenKind::Comma {
            lexer.next();
        }
        lexer.skip_line_feed();
    }

    let mut arguments = Vec::new_in(allocator);

    loop {
        let Some(argument) = parse_function_argument(lexer, errors, allocator) else {
            break;
        };
        arguments.push(argument);

        if lexer.current().get_kind() != TokenKind::Comma {
            break;
        }
        lexer.next();
        lexer.skip_line_feed();
    }

    let arguments = allocator.alloc(arguments).as_slice();

    if lexer.current().get_kind() != TokenKind::ParenthesesRight {
        let error = recover_until(
            lexer,
            &[TokenKind::ParenthesesRight],
            ParseErrorKind::InvalidFunctionArgumentOrUnclosedParen,
        );
        errors.push(error);
    }
    lexer.next();

    Some(FunctionArguments {
        this_mutability,
        arguments,
        span: anchor.elapsed(lexer),
    })
}

fn parse_this_mutability<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
) -> Option<ThisMutability> {
    let anchor = lexer.cast_anchor();

    let is_mutable = match lexer.current().get_kind() {
        TokenKind::Let => Spanned::new(false, lexer.next().unwrap().span),
        TokenKind::Var => Spanned::new(true, lexer.next().unwrap().span),
        _ => return None,
    };

    if lexer.current().get_kind() == TokenKind::This {
        lexer.next();
    } else {
        let error = ParseError {
            kind: ParseErrorKind::MissingThisInThisMutability,
            span: anchor.elapsed(lexer),
        };
        errors.push(error);
    }

    Some(ThisMutability {
        is_mutable,
        span: anchor.elapsed(lexer),
    })
}

pub(crate) fn parse_function_argument<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<FunctionArgument<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let Some(binding) = parse_variable_binding(lexer, errors, allocator) else {
        return None;
    };

    let Some(type_tag) = parse_type_tag(ParseTypeTagKind::Normal, lexer, errors, allocator) else {
        lexer.back_to_anchor(anchor);
        return None;
    };

    Some(FunctionArgument {
        binding,
        type_tag,
        span: anchor.elapsed(lexer),
    })
}

pub(crate) fn parse_variable_binding<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<VariableBinding<'input, 'allocator>> {
    match lexer.current().get_kind() {
        TokenKind::Literal => Some(VariableBinding::Literal(lexer.parse_as_literal())),
        TokenKind::ParenthesesLeft => {
            let anchor = lexer.cast_anchor();

            lexer.next();

            lexer.skip_line_feed();

            let mut bindings = Vec::new_in(allocator);

            loop {
                let Some(binding) = parse_variable_binding(lexer, errors, allocator) else {
                    break;
                };
                bindings.push(binding);

                if lexer.current().get_kind() != TokenKind::Comma {
                    break;
                }
                lexer.next();

                lexer.skip_line_feed();
            }

            lexer.skip_line_feed();

            if lexer.current().get_kind() != TokenKind::ParenthesesRight {
                let error = recover_until(
                    lexer,
                    &[TokenKind::ParenthesesRight],
                    ParseErrorKind::InvalidVariableBindingOrUnclosedParen,
                );
                errors.push(error);
            }
            lexer.next();

            let bindings = allocator.alloc(bindings).as_slice();

            Some(VariableBinding::Binding {
                bindings,
                span: anchor.elapsed(lexer),
            })
        }
        _ => None,
    }
}

fn parse_user_type_define<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<UserTypeDefine<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let kind = match lexer.current().get_kind() {
        TokenKind::Class => Spanned::new(UserTypeKind::Class, lexer.next().unwrap().span),
        TokenKind::Struct => Spanned::new(UserTypeKind::Struct, lexer.next().unwrap().span),
        TokenKind::Interface => Spanned::new(UserTypeKind::Interface, lexer.next().unwrap().span),
        _ => return None,
    };

    let name = match lexer.current().get_kind() {
        TokenKind::Literal => Ok(lexer.parse_as_literal()),
        _ => {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingNameInUserTypeDefine,
            );
            errors.push(error);

            return Some(UserTypeDefine {
                kind,
                name: Err(()),
                generics: None,
                super_type: None,
                where_clause: None,
                block: Err(()),
                span: anchor.elapsed(lexer),
            });
        }
    };

    let generics = parse_generics_define(lexer, errors, allocator);

    let super_type = parse_super_type_info(lexer, errors, allocator);

    let where_clause = parse_where_clause(lexer, errors, allocator);

    let block = match parse_block(lexer, errors, allocator) {
        Some(block) => Ok(block),
        None => {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingBlockInUserTypeDefine,
            );
            errors.push(error);

            Err(())
        }
    };

    Some(UserTypeDefine {
        kind,
        name,
        generics,
        super_type,
        where_clause,
        block,
        span: anchor.elapsed(lexer),
    })
}

fn parse_super_type_info<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<SuperTypeInfo<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Colon {
        return None;
    }
    lexer.next();

    lexer.skip_line_feed();

    let mut types = Vec::new_in(allocator);

    loop {
        let Some(type_info) = parse_type_info(lexer, errors, allocator) else {
            break;
        };
        types.push(type_info);

        if lexer.current().get_kind() != TokenKind::Comma {
            break;
        }
        lexer.next();
    }

    let types = allocator.alloc(types).as_slice();

    Some(SuperTypeInfo {
        types,
        span: anchor.elapsed(lexer),
    })
}

fn parse_variable_define<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<VariableDefine<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let let_var = match lexer.current().get_kind() {
        TokenKind::Let => Spanned::new(LetVar::Let, lexer.next().unwrap().span),
        TokenKind::Var => Spanned::new(LetVar::Var, lexer.next().unwrap().span),
        _ => return None,
    };

    let Some(binding) = parse_variable_binding(lexer, errors, allocator) else {
        let error = recover_until(
            lexer,
            &[TokenKind::LineFeed, TokenKind::SemiColon],
            ParseErrorKind::MissingBindingInVariableDefine,
        );
        errors.push(error);

        return Some(VariableDefine {
            let_var,
            binding: Err(()),
            type_tag: None,
            expression: None,
            span: anchor.elapsed(lexer),
        });
    };

    let type_tag = parse_type_tag(ParseTypeTagKind::Normal, lexer, errors, allocator);

    let expression = match lexer.current().get_kind() {
        TokenKind::Equal => {
            lexer.next();

            match parse_expression(lexer, errors, allocator) {
                Some(expression) => Some(expression),
                None => {
                    let error = recover_until(
                        lexer,
                        &[TokenKind::LineFeed, TokenKind::SemiColon],
                        ParseErrorKind::MissingExpressionInVariableDefine,
                    );
                    errors.push(error);

                    None
                }
            }
        }
        _ => None,
    };

    Some(VariableDefine {
        let_var,
        binding: Ok(binding),
        type_tag,
        expression,
        span: anchor.elapsed(lexer),
    })
}

fn parse_implements<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<Implements<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Implements {
        return None;
    }
    let implements = lexer.next().unwrap().span;

    let generics = parse_generics_define(lexer, errors, allocator);

    let Some(interface) = parse_type_info(lexer, errors, allocator) else {
        let error = recover_until(
            lexer,
            &[
                TokenKind::LineFeed,
                TokenKind::SemiColon,
                TokenKind::BraceLeft,
            ],
            ParseErrorKind::MissingInterfaceInImplements,
        );
        errors.push(error);

        return Some(Implements {
            implements,
            generics,
            interface: Err(()),
            concrete: Err(()),
            where_clause: None,
            block: Err(()),
            span: anchor.elapsed(lexer),
        });
    };

    if lexer.current().get_kind() != TokenKind::For {
        let error = recover_until(
            lexer,
            &[
                TokenKind::LineFeed,
                TokenKind::SemiColon,
                TokenKind::BraceLeft,
            ],
            ParseErrorKind::MissingForKeywordInImplements,
        );
        errors.push(error);

        return Some(Implements {
            implements,
            generics,
            interface: Err(()),
            concrete: Err(()),
            where_clause: None,
            block: Err(()),
            span: anchor.elapsed(lexer),
        });
    }
    lexer.next();

    let Some(target) = parse_type_info(lexer, errors, allocator) else {
        let error = recover_until(
            lexer,
            &[
                TokenKind::LineFeed,
                TokenKind::SemiColon,
                TokenKind::BraceLeft,
            ],
            ParseErrorKind::MissingTargetTypeInImplements,
        );
        errors.push(error);

        return Some(Implements {
            implements,
            generics,
            interface: Ok(interface),
            concrete: Err(()),
            where_clause: None,
            block: Err(()),
            span: anchor.elapsed(lexer),
        });
    };

    let where_clause = parse_where_clause(lexer, errors, allocator);

    let block = match parse_block(lexer, errors, allocator) {
        Some(block) => Ok(block),
        None => {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingBlockInImplements,
            );
            errors.push(error);

            Err(())
        }
    };

    Some(Implements {
        implements,
        generics,
        interface: Ok(interface),
        concrete: Ok(target),
        where_clause,
        block,
        span: anchor.elapsed(lexer),
    })
}

fn parse_type_alias<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<TypeAlias<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Type {
        return None;
    }
    let type_keyword = lexer.next().unwrap().span;

    let name = match lexer.current().get_kind() {
        TokenKind::Literal => lexer.parse_as_literal(),
        _ => {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingNameInTypeAlias,
            );
            errors.push(error);

            return Some(TypeAlias {
                type_keyword,
                name: Err(()),
                generics: None,
                alias_type: Err(()),
                span: anchor.elapsed(lexer),
            });
        }
    };

    let generics = parse_generics_define(lexer, errors, allocator);

    if lexer.current().get_kind() != TokenKind::Equal {
        let error = recover_until(
            lexer,
            &[TokenKind::LineFeed, TokenKind::SemiColon],
            ParseErrorKind::MissingEqualInTypeAlias,
        );
        errors.push(error);

        return Some(TypeAlias {
            type_keyword,
            name: Ok(name),
            generics,
            alias_type: Err(()),
            span: anchor.elapsed(lexer),
        });
    }
    lexer.next();

    let alias_type = match parse_type_info(lexer, errors, allocator) {
        Some(alias_type) => Ok(alias_type),
        None => {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingTypeInTypeAlias,
            );
            errors.push(error);

            Err(())
        }
    };

    Some(TypeAlias {
        type_keyword,
        name: Ok(name),
        generics,
        alias_type,
        span: anchor.elapsed(lexer),
    })
}

fn parse_drop_statement<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut std::vec::Vec<ParseError>,
    allocator: &'allocator Bump,
) -> Option<DropStatement<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Drop {
        return None;
    }
    let drop = lexer.next().unwrap().span;

    let acyclic = match lexer.current().get_kind() {
        TokenKind::Acyclic => Some(lexer.next().unwrap().span),
        _ => None,
    };

    let expression = match parse_expression(lexer, errors, allocator) {
        Some(expression) => Ok(expression),
        None => {
            let error = recover_until(
                lexer,
                &[TokenKind::LineFeed, TokenKind::SemiColon],
                ParseErrorKind::MissingExpressionInDropStatement,
            );
            errors.push(error);

            Err(())
        }
    };

    Some(DropStatement {
        drop,
        acyclic,
        expression,
        span: anchor.elapsed(lexer),
    })
}
