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
    MissingInitExpression,
    MissingLengthExpression,
    MissingSemiColonInArrayInitExpression,
    MissingElseChain,
    MissingIfCondition,
    MissingIfBlock,
    MissingLoopBlock,
    MissingRightExprInAssign,
    MissingRightExprInSwap,
    MissingPathInImport,
    MissingDefineAfterAttribute,
    MissingBaseTypeInArrayType,
    MissingFunctionName,
    MissingThisInThisMutability,
    MissingTypeTagInFunctionArgument,
    MissingFunctionArguments,
    MissingBlockOrSemiClonInFunctionDefine,
    MissingFatArrowInClosure,
    MissingExpressionOrBlockInClosure,
    MissingNameInUserTypeDefine,
    MissingBlockInUserTypeDefine,
    MissingBindingInVariableDefine,
    MissingExpressionInVariableDefine,
    MissingInterfaceInImplements,
    MissingForKeywordInImplements,
    MissingTargetTypeInImplements,
    MissingBlockInImplements,
    MissingNameInTypeAlias,
    MissingEqualInTypeAlias,
    MissingTypeInTypeAlias,
    MissingExpressionInDropStatement,
    UnclosedBrace,
    InvalidFunctionCallFormatOrUnclosedParen,
    InvalidTupleExprFormatOrUnclosedParen,
    InvalidFieldAssignFormatOrUnclosedBrace,
    InvalidExpressionInNewArrayExprOrUnclosedBrace,
    InvalidImportElementsOrUnclosed,
    InvalidGenericsInfoOrUnclosedGreaterThan,
    InvalidArrayTypeOrUnclosedBracket,
    InvalidYupleOrUnclosedParen,
    InvalidGenericsDefineOrUnclosedGreaterThan,
    InvalidFunctionArgumentOrUnclosedParen,
    InvalidTypeTag,
    InvalidVariableBindingOrUnclosedParen,
    InvalidClosureArgumentOrUnclosedVerticalLine,
    InvalidCompilerTagFormat,
}

pub(crate) fn recover_until(
    lexer: &mut Lexer,
    until: &[TokenKind],
    kind: ParseErrorKind,
) -> ParseError {
    let anchor = lexer.cast_anchor();

    loop {
        let token = lexer.current();

        if token.is_none() || until.contains(&token.get_kind()) {
            break;
        }

        lexer.next();
    }

    ParseError {
        kind,
        span: anchor.elapsed(lexer),
    }
}
