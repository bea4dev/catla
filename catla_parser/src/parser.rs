pub mod statement;
pub mod expression;
pub mod types;

use std::ops::Range;
use either::Either;
use crate::{lexer::{Token, Lexer, TokenKind}, util::parser_utils::bump_vec};

use bumpalo::{collections::Vec, Bump};


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Range<usize>
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Range<usize>) -> Self {
        return Self {
            value,
            span
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Recovered<'allocator, 'input, T> {
    pub error_tokens: Vec<'allocator, Token<'input>>,
    pub value: Option<T>
}

pub struct StringToken(pub usize, pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ASTParseError<'allocator, 'input> {
    UnexpectedToken(Vec<'allocator, Token<'input>>),
    UnexpectedEOF
}

pub type ParseResult<'allocator, 'input, T> = Result<T, ASTParseError<'allocator, 'input>>;

pub type Program<'allocator, 'input> = &'allocator ProgramAST<'allocator, 'input>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProgramAST<'allocator, 'input> {
    pub statements: Vec<'allocator, Statement<'allocator, 'input>>,
    pub not_separated_stmts: Vec<'allocator, Token<'input>>,
    pub span: Range<usize>
}

pub type Statement<'allocator, 'input> = ParseResult<'allocator, 'input, StatementAST<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementAST<'allocator, 'input> {
    Assignment(Assignment<'allocator, 'input>),
    Exchange(Exchange<'allocator, 'input>),
    Import(Import<'allocator, 'input>),
    StatementAttributes(Vec<'allocator, StatementAttribute>),
    VariableDefine(VariableDefine<'allocator, 'input>),
    FunctionDefine(FunctionDefine<'allocator, 'input>),
    DataStructDefine(DataStructDefine<'allocator, 'input>),
    DropStatement(DropStatement<'allocator, 'input>),
    Expression(Expression<'allocator, 'input>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefine<'allocator, 'input> {
    pub attributes: Vec<'allocator, StatementAttribute>,
    pub generics: Option<Generics<'allocator, 'input>>,
    pub name: FunctionName<'allocator, 'input>,
    pub args: FunctionArguments<'allocator, 'input>,
    pub type_tag: Option<TypeTag<'allocator, 'input>>,
    pub block: BlockRecovered<'allocator, 'input>,
    pub span: Range<usize>
}

pub type FunctionName<'allocator, 'input> = ParseResult<'allocator, 'input, Either<Literal<'input>, MemoryManageAttribute>>;

pub type StatementAttribute = Spanned<StatementAttributeKind>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementAttributeKind {
    Static,
    Private,
    Suspend,
    Native,
    Uncycle,
    Open
}

pub type MemoryManageAttribute = Spanned<MemoryManageAttributeKind>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemoryManageAttributeKind {
    New,
    Drop,
    Mutex
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionArgument<'allocator, 'input> {
    pub name: Literal<'input>,
    pub type_tag: TypeTag<'allocator, 'input>,
    pub span: Range<usize>
}

pub type FunctionArgumentResult<'allocator, 'input> = ParseResult<'allocator, 'input, FunctionArgument<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionArguments<'allocator, 'input> {
    pub paren_left: ParseResult<'allocator, 'input, ()>,
    pub arguments: Vec<'allocator, FunctionArgument<'allocator, 'input>>,
    pub error_tokens: Vec<'allocator, Token<'input>>,
    pub paren_right: ParseResult<'allocator, 'input, ()>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataStructDefine<'allocator, 'input> {
    pub attributes: Vec<'allocator, StatementAttribute>,
    pub kind: DataStructKind,
    pub name: LiteralResult<'allocator, 'input>,
    pub generics: Option<Generics<'allocator, 'input>>,
    pub extends: Option<Extends<'allocator, 'input>>,
    pub implements: Option<Implements<'allocator, 'input>>,
    pub error_tokens: Vec<'allocator, Token<'input>>,
    pub block: BlockRecovered<'allocator, 'input>,
    pub span: Range<usize>
}

pub type DataStructKind = Spanned<DataStructKindEnum>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataStructKindEnum {
    Class,
    Struct,
    Interface
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Extends<'allocator, 'input> {
    pub type_info: TypeInfoResult<'allocator, 'input>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Implements<'allocator, 'input> {
    pub type_infos: Vec<'allocator, TypeInfo<'allocator, 'input>>,
    pub error_tokens: Vec<'allocator, Token<'input>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import<'allocator, 'input> {
    pub import_path: Vec<'allocator, Literal<'input>>,
    pub elements: ImportElements<'allocator, 'input>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportElements<'allocator, 'input> {
    elements: Vec<'allocator, Literal<'input>>,
    error_tokens: Vec<'allocator, Token<'input>>,
    brace_right: ParseResult<'allocator, 'input, ()>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DropStatement<'allocator, 'input> {
    pub uncycle_keyword_span: Option<Range<usize>>,
    pub expression: ExpressionResult<'allocator, 'input>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block<'allocator, 'input> {
    pub program: Program<'allocator, 'input>,
    pub brace_right: ParseResult<'allocator, 'input, ()>,
    pub span: Range<usize>
}

pub type BlockResult<'allocator, 'input> = ParseResult<'allocator, 'input, Block<'allocator, 'input>>;
pub type BlockRecovered<'allocator, 'input> = Recovered<'allocator, 'input, Block<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableDefine<'allocator, 'input> {
    pub attributes: VariableAttributes<'allocator>,
    pub name: LiteralResult<'allocator, 'input>,
    pub type_tag: Option<TypeTag<'allocator, 'input>>,
    pub expression: Option<ExpressionResult<'allocator, 'input>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableAttributes<'allocator> {
    pub statement_attributes: Vec<'allocator, StatementAttribute>,
    pub is_var: bool,
    pub var_let_span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment<'allocator, 'input> {
    pub left_expr: Expression<'allocator, 'input>,
    pub right_expr: ParseResult<'allocator, 'input, Expression<'allocator, 'input>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exchange<'allocator, 'input> {
    pub left_expr: Expression<'allocator, 'input>,
    pub right_expr: ParseResult<'allocator, 'input, Expression<'allocator, 'input>>,
    pub span: Range<usize>
}

pub type Expression<'allocator, 'input> = &'allocator ExpressionEnum<'allocator, 'input>;
pub type ExpressionResult<'allocator, 'input> = ParseResult<'allocator, 'input, Expression<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionEnum<'allocator, 'input> {
    OrExpression(OrExpression<'allocator, 'input>),
    ReturnExpression(ReturnExpression<'allocator, 'input>),
    Closure(Closure<'allocator, 'input>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OrExpression<'allocator, 'input> {
    pub left_expr: AndExpression<'allocator, 'input>,
    pub right_exprs: Vec<'allocator, (OrOperatorSpan, AndExpressionResult<'allocator, 'input>)>,
    pub span: Range<usize>
}

pub type OrOperatorSpan = Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AndExpression<'allocator, 'input> {
    pub left_expr: EQNEExpression<'allocator, 'input>,
    pub right_exprs: Vec<'allocator, (AndOperatorSpan, EQNEExpressionResult<'allocator, 'input>)>,
    pub span: Range<usize>
}

pub type AndOperatorSpan = Range<usize>;
pub type AndExpressionResult<'allocator, 'input> = ParseResult<'allocator, 'input, AndExpression<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EQNEExpression<'allocator, 'input> {
    pub left_expr: CompareExpression<'allocator, 'input>,
    pub right_exprs: Vec<'allocator, (EQNECompareOp, CompareExpressionResult<'allocator, 'input>)>,
    pub span: Range<usize>
}

pub type EQNECompareOp = Spanned<EQNECompareOpKind>;
pub type EQNEExpressionResult<'allocator, 'input> = ParseResult<'allocator, 'input, EQNEExpression<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EQNECompareOpKind {
    Equal,
    NotEqual
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareExpression<'allocator, 'input> {
    pub left_expr: AddOrSubExpression<'allocator, 'input>,
    pub right_exprs: Vec<'allocator, (CompareOp, AddOrSubExpressionResult<'allocator, 'input>)>,
    pub span: Range<usize>
}

pub type CompareOp = Spanned<CompareOpKind>;
pub type CompareExpressionResult<'allocator, 'input> = ParseResult<'allocator, 'input, CompareExpression<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompareOpKind {
    GreaterThan,
    GreaterOrEqual,
    LessThan,
    LessOrEqual
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AddOrSubExpression<'allocator, 'input> {
    pub left_expr: MulOrDivExpression<'allocator, 'input>,
    pub right_exprs: Vec<'allocator, (AddOrSubOp, MulOrDivExpressionResult<'allocator, 'input>)>,
    pub span: Range<usize>
}

pub type AddOrSubOp = Spanned<AddOrSubOpKind>;
pub type AddOrSubExpressionResult<'allocator, 'input> = ParseResult<'allocator, 'input, AddOrSubExpression<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AddOrSubOpKind {
    Add,
    Sub
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MulOrDivExpression<'allocator, 'input> {
    pub left_expr: Factor<'allocator, 'input>,
    pub right_exprs: Vec<'allocator, (MulOrDivOp, FactorResult<'allocator, 'input>)>,
    pub span: Range<usize>
}

pub type MulOrDivOp = Spanned<MulOrDivOpKind>;
pub type MulOrDivExpressionResult<'allocator, 'input> = ParseResult<'allocator, 'input, MulOrDivExpression<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MulOrDivOpKind {
    Mul,
    Div
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Factor<'allocator, 'input> {
    pub negative_keyword_span: Option<Range<usize>>,
    pub primary: PrimaryResult<'allocator, 'input>,
    pub span: Range<usize>
}

pub type FactorResult<'allocator, 'input> = ParseResult<'allocator, 'input, Factor<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Primary<'allocator, 'input> {
    pub left: PrimaryLeft<'allocator, 'input>,
    pub chain: Vec<'allocator, PrimaryRight<'allocator, 'input>>,
    pub span: Range<usize>
}

pub type PrimaryResult<'allocator, 'input> = ParseResult<'allocator, 'input, Primary<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrimaryLeft<'allocator, 'input> {
    pub first_expr: PrimaryLeftExpr<'allocator, 'input>,
    pub mapping_operator: Option<MappingOperator<'allocator, 'input>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimaryLeftExpr<'allocator, 'input> {
    Simple((SimplePrimary<'allocator, 'input>, Option<FunctionCall<'allocator, 'input>>)),
    NewExpression(NewExpression<'allocator, 'input>),
    IfExpression(IfExpression<'allocator, 'input>),
    LoopExpression(LoopExpression<'allocator, 'input>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopExpression<'allocator, 'input> {
    pub block: BlockResult<'allocator, 'input>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfExpression<'allocator, 'input> {
    pub if_statement: IfStatement<'allocator, 'input>,
    pub chain: Vec<'allocator, ElseIfOrElse<'allocator, 'input>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStatement<'allocator, 'input> {
    pub if_keyword_span: Range<usize>,
    pub condition: ExpressionResult<'allocator, 'input>,
    pub block: BlockRecovered<'allocator, 'input>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrimaryRight<'allocator, 'input> {
    pub separator: PrimarySeparator,
    pub second_expr: Option<(Literal<'input>, Option<FunctionCall<'allocator, 'input>>)>,
    pub mapping_operator: Option<MappingOperator<'allocator, 'input>>,
    pub span: Range<usize>
}

pub type PrimarySeparator = Spanned<PrimarySeparatorKind>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimarySeparatorKind {
    Dot,
    DoubleColon
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimplePrimary<'allocator, 'input> {
    Expression{ expression: ExpressionResult<'allocator, 'input>, error_tokens: Vec<'allocator, Token<'input>> },
    Identifier(Literal<'input>),
    NullKeyword(Range<usize>)
}

pub type MappingOperator<'allocator, 'input> = Spanned<MappingOperatorKind<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MappingOperatorKind<'allocator, 'input> {
    NullPropagation,
    NullUnwrap,
    NullElvisBlock(BlockRecovered<'allocator, 'input>),
    ResultPropagation,
    ResultUnwrap,
    ResultElvisBlock(BlockRecovered<'allocator, 'input>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionCall<'allocator, 'input> {
    pub generics: Option<GenericsResult<'allocator, 'input>>,
    pub error_tokens: Vec<'allocator, Token<'input>>,
    pub arg_exprs: CallArgumentsResult<'allocator, 'input>,
    pub span: Range<usize>
}

pub type FunctionCallResult<'allocator, 'input> = ParseResult<'allocator, 'input, FunctionCall<'allocator, 'input>>;
pub type CallArgumentsResult<'allocator, 'input> = ParseResult<'allocator, 'input, Vec<'allocator, Expression<'allocator, 'input>>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NewExpression<'allocator, 'input> {
    pub new_keyword_span: Range<usize>,
    pub uncycle_keyword_span: Option<Range<usize>>,
    pub path: Vec<'allocator, Literal<'input>>,
    pub error_tokens: Vec<'allocator, Token<'input>>,
    pub function_call: FunctionCallResult<'allocator, 'input>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElseIfOrElse<'allocator, 'input> {
    pub else_keyword_span: Range<usize>,
    pub else_if_or_else: Recovered<'allocator, 'input, Either<IfStatement<'allocator, 'input>, Block<'allocator, 'input>>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnExpression<'allocator, 'input> {
    pub return_keyword_span: Range<usize>,
    pub expression: Option<Expression<'allocator, 'input>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure<'allocator, 'input> {
    pub arguments: ClosureArguments<'allocator, 'input>,
    pub error_tokens: Vec<'allocator, Token<'input>>,
    pub expression_or_block: Recovered<'allocator, 'input, Either<Expression<'allocator, 'input>, Block<'allocator, 'input>>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClosureArguments<'allocator, 'input> {
    pub arguments: Either<Literal<'input>, Vec<'allocator, Either<FunctionArgument<'allocator, 'input>, Literal<'input>>>>,
    pub error_tokens: Vec<'allocator, Token<'input>>,
    pub vertical_bar_right: ParseResult<'allocator, 'input, ()>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeTag<'allocator, 'input> {
    pub tag_kind: TypeTagKind,
    pub type_info: TypeInfoResult<'allocator, 'input>,
    pub span: Range<usize>
}

pub type TypeTagKind = Spanned<TypeTagKindEnum>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeTagKindEnum {
    Normal,
    ReturnType
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeInfo<'allocator, 'input> {
    pub path: Vec<'allocator, Literal<'input>>,
    pub generics: Option<Generics<'allocator, 'input>>,
    pub type_attributes: Vec<'allocator, TypeAttribute<'allocator, 'input>>,
    pub span: Range<usize>
}

pub type TypeInfoResult<'allocator, 'input> = ParseResult<'allocator, 'input, TypeInfo<'allocator, 'input>>;

pub type TypeAttribute<'allocator, 'input> = Spanned<TypeAttributeEnum<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAttributeEnum<'allocator, 'input> {
    Optional,
    Result(Option<Generics<'allocator, 'input>>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Generics<'allocator, 'input> {
    pub elements: Vec<'allocator, TypeInfo<'allocator, 'input>>,
    pub error_tokens: Vec<'allocator, Token<'input>>,
    pub span: Range<usize>
}

pub type GenericsResult<'allocator, 'input> = ParseResult<'allocator, 'input, Generics<'allocator, 'input>>;

pub type Literal<'input> = Spanned<&'input str>;
pub type LiteralResult<'allocator, 'input> = ParseResult<'allocator, 'input, Literal<'input>>;

pub struct TokenCursor<'allocator, 'input> {
    tokens: Vec<'allocator, Token<'input>>,
    current_position: usize,
    allocator: &'allocator Bump
}

impl<'allocator, 'input> TokenCursor<'allocator, 'input> {
    
    fn new(allocator: &'allocator Bump, lexer: Lexer<'input>) -> TokenCursor<'allocator, 'input> {
        let mut tokens = Vec::new_in(allocator);
        for token in lexer {
            tokens.push(token);
        }
        return Self {
            tokens,
            current_position: 0,
            allocator
        }
    }

    fn current(&self) -> Option<&Token<'input>> {
        return self.tokens.get(self.current_position);
    }

    fn next(&mut self) -> Option<&Token<'input>> {
        let current = self.tokens.get(self.current_position);
        self.current_position += 1;
        return current;
    }

    #[allow(unused)]
    fn peek_next(&self) -> Option<&Token<'input>> {
        return self.tokens.get(self.current_position + 1);
    }

    fn prev(&mut self) -> Option<&Token<'input>> {
        let current = self.tokens.get(self.current_position);
        if self.current_position > 0 {
            self.current_position -= 1;
        }
        return current;
    }

    fn peek_prev(&self) -> Option<&Token<'input>> {
        return self.tokens.get(self.current_position - 1);
    }

}

pub struct Span {
    start_position: usize
}

impl Span {
    pub fn start<'allocator, 'input>(cursor: &TokenCursor<'allocator, 'input>) -> Span {
        return Self {
            start_position: cursor.current().map_or(0, |token| { token.span.start })
        }
    }

    pub fn elapsed<'allocator, 'input>(&self, cursor: &TokenCursor<'allocator, 'input>) -> Range<usize> {
        return self.start_position..cursor.peek_prev().map_or(0, |prev| { prev.span.end });
    }
}




pub fn parse_source<'allocator, 'input>(source: &'input str, allocator: &'allocator Bump) -> Program<'allocator, 'input> {
    let lexer = Lexer::new(source);
    let mut cursor = TokenCursor::new(allocator, lexer);
    return parse_program(&mut cursor, &[TokenKind::None]);
}

fn parse_program<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>, end: &[TokenKind]) -> Program<'allocator, 'input> {
    let span = Span::start(cursor);

    let mut statements = Vec::new_in(cursor.allocator);
    let mut not_separated_stmts = Vec::new_in(cursor.allocator);
    loop {
        let skipped_tokens = skip(cursor, &[TokenKind::Semicolon, TokenKind::LineFeed]);
        if end.contains(&cursor.current().get_kind()) {
            break;
        }
        
        let mut not_separated = if statements.len() != 0 && skipped_tokens == 0 {
            Some(cursor.current().unwrap().clone())
        } else {
            None
        };

        let statement = statement::parse_statement(cursor);
        if statement.is_err() {
            not_separated = None;
        }

        statements.push(statement);
        if let Some(not_separated) = not_separated {
            not_separated_stmts.push(not_separated);
        }
    }

    return cursor.allocator.alloc(ProgramAST { statements, not_separated_stmts, span: span.elapsed(cursor) })
}

fn skip<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>, skip_kinds: &[TokenKind]) -> usize {
    let mut iteration_count = 0;
    loop {
        let next = cursor.current().get_kind();
        if !skip_kinds.contains(&next) {
            break;
        }
        cursor.next();
        iteration_count += 1;
    }
    return iteration_count;
}

fn parse_literal<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> Option<Literal<'input>> {
    return match cursor.next() {
        Some(next) => {
            if next.kind == TokenKind::Literal {
                Some(Spanned::new(next.text, next.span.clone()))
            } else {
                cursor.prev();
                None
            }
        },
        _ => None
    }
}

fn parse_literal_result<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>) -> LiteralResult<'allocator, 'input> {
    return match cursor.next().cloned() {
        Some(next) => {
            if next.kind == TokenKind::Literal {
                Ok(Spanned::new(next.text, next.span.clone()))
            } else {
                cursor.prev();
                let mut tokens = Vec::new_in(cursor.allocator);
                tokens.push(next.clone());
                Err(ASTParseError::UnexpectedToken(tokens))
            }
        },
        _ => Err(ASTParseError::UnexpectedEOF)
    }
}

fn read_until_token_found<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>, until: &[TokenKind]) -> Vec<'allocator, Token<'input>> {
    let mut dropped_tokens = Vec::new_in(cursor.allocator);
    loop {
        let drop = match cursor.next() {
            Some(drop) => drop,
            _ => break
        };
        if until.contains(&drop.kind) {
            break;
        }
        dropped_tokens.push(drop.clone());
    }
    return dropped_tokens;
}

fn recover_until_token_found<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>, until: &[TokenKind]) -> Vec<'allocator, Token<'input>> {
    let dropped_tokens = read_until_token_found(cursor, until);
    cursor.prev();
    return dropped_tokens;
}

fn unexpected_token_error<'allocator, 'input>(allocator: &'allocator Bump, token: Option<&Token<'input>>) -> ASTParseError<'allocator, 'input> {
    return match token {
        Some(next) => ASTParseError::UnexpectedToken(bump_vec![allocator, next.clone()]),
        _ => ASTParseError::UnexpectedEOF
    }
}

fn parse_with_recover<'allocator, 'input, T>(
    cursor: &mut TokenCursor<'allocator, 'input>,
    parser: fn(&mut TokenCursor<'allocator, 'input>) -> Option<T>, recover_until: &[TokenKind]
) -> Recovered<'allocator, 'input, T> {

    return match parser(cursor) {
        Some(ast) => Recovered { error_tokens: bump_vec![cursor.allocator], value: Some(ast) },
        _ => {
            let error_tokens = recover_until_token_found(cursor, recover_until);
            Recovered { error_tokens, value: parser(cursor) }
        }
    }
}


trait GetTokenKind {
    fn get_kind(&self) -> TokenKind;
}

impl<'input> GetTokenKind for Option<&Token<'input>> {
    fn get_kind(&self) -> TokenKind {
        return self.map_or(TokenKind::None, |token| { token.kind });
    }
}


trait MergedExtend {
    fn merged_extend(&mut self, other: Self);
}

impl<'allocator, 'input> MergedExtend for Vec<'allocator, Token<'input>> {
    fn merged_extend(&mut self, other: Self) {
        let cancel = if let Some(last) = other.last() {
            if let Some(self_last) = self.last() {
                last == self_last
            } else {
                false
            }
        } else {
            false
        };

        if !cancel {
            self.extend(other)
        }
    }
}