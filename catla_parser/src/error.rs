use std::ops::Range;

use crate::lexer::{GetKind, Lexer, TokenKind};

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseErrorKind {
    ExtraStatementTokens,
    ExtraDoubleColonInPath,
    MissingExpressionIn2OpChain,
    MissingExpressionIn1Op,
    MissingGenericsInfoAfterColon,
    MissingBlockInElvis,
    MissingTypeNameInNewObjectExpr,
    MissingFieldAssignInNewObjectExpr,
    MissingColonInFieldAssign,
    MissingExpressionInFieldAssign,
    UnclosedParen,
    UnclosedBrace,
    InvalidFunctionCallFormat,
    InvalidTupleExprFormat,
}

pub(crate) fn recover_until(
    lexer: &mut Lexer,
    until: &[TokenKind],
    kind: ParseErrorKind,
) -> ParseError {
    let anchor = lexer.cast_anchor();

    loop {
        let token = lexer.current();

        if until.contains(&token.get_kind()) {
            break;
        }

        lexer.next();
    }

    ParseError {
        kind,
        span: anchor.elapsed(lexer),
    }
}
