use allocator_api2::vec::Vec;
use bumpalo::Bump;

use crate::{
    ast::{Documents, Statement, StatementWithTagAndDocs},
    error::ParseError,
    lexer::{GetKind, Lexer, TokenKind},
    parser::expression::parse_expression,
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
    if let Some(expression) = parse_expression(lexer, errors, allocator) {
        return Some(Statement::Expression(expression));
    }

    None
}
