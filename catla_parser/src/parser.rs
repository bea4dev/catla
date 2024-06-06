pub mod statement;
pub mod expression;
pub mod types;

use std::ops::Range;
use either::Either;
use crate::{lexer::{Token, Lexer, TokenKind}, util::parser_utils::{bump_vec, impl_ast}};

use bumpalo::Bump;


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Range<usize>
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Range<usize>) -> Self {
        Self {
            value,
            span
        }
    }

    pub fn map<U, F>(self, function: F) -> Spanned<U>
    where F: FnOnce(T) -> U {
        let value = function(self.value);
        Spanned::new(value, self.span)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Recovered<'allocator, 'input, T> {
    pub result: ParseResult<'allocator, 'input, ()>,
    pub value: Option<T>
}

pub struct StringToken(pub usize, pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ASTParseError<'allocator, 'input> {
    UnexpectedToken(Vec<Token<'input>, &'allocator Bump>),
    UnexpectedEOF
}

pub type ParseResult<'allocator, 'input, T> = Result<T, ASTParseError<'allocator, 'input>>;
pub type ErrorTokens<'allocator, 'input> = Vec<Vec<Token<'input>, &'allocator Bump>, &'allocator Bump>;


pub trait AST {}
impl_ast!{
    ProgramAST<'_, '_>,
    StatementAST<'_, '_>,
    FunctionDefine<'_, '_>,
    FunctionArgument<'_, '_>,
    FunctionArguments<'_, '_>,
    GenericsDefine<'_, '_>,
    GenericsElement<'_, '_>,
    UserTypeDefine<'_, '_>,
    TypeDefine<'_, '_>,
    Implements<'_, '_>,
    Import<'_, '_>,
    ImportElements<'_, '_>,
    DropStatement<'_, '_>,
    Block<'_, '_>,
    VariableDefine<'_, '_>,
    Assignment<'_, '_>,
    Exchange<'_, '_>,
    ExpressionEnum<'_, '_>,
    OrExpression<'_, '_>,
    AndExpression<'_, '_>,
    EQNEExpression<'_, '_>,
    CompareExpression<'_, '_>,
    AddOrSubExpression<'_, '_>,
    MulOrDivExpression<'_, '_>,
    Factor<'_, '_>,
    Primary<'_, '_>,
    PrimaryLeft<'_, '_>,
    PrimaryLeftExpr<'_, '_>,
    LoopExpression<'_, '_>,
    IfExpression<'_, '_>,
    IfStatement<'_, '_>,
    PrimaryRight<'_, '_>,
    SimplePrimary<'_, '_>,
    MappingOperatorKind<'_, '_>,
    FunctionCall<'_, '_>,
    NewExpression<'_, '_>,
    FieldAssign<'_, '_>,
    ElseIfOrElse<'_, '_>,
    ReturnExpression<'_, '_>,
    Closure<'_, '_>,
    ClosureArguments<'_, '_>,
    TypeTag<'_, '_>,
    TypeInfo<'_, '_>,
    TypeAttributeEnum<'_, '_>,
    Generics<'_, '_>,
    Literal<'_>
}

pub type Program<'allocator, 'input> = &'allocator ProgramAST<'allocator, 'input>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProgramAST<'allocator, 'input> {
    pub statements: Vec<Statement<'allocator, 'input>, &'allocator Bump>,
    pub not_separated_stmts: Vec<Token<'input>, &'allocator Bump>,
    pub span: Range<usize>
}
unsafe impl Send for ProgramAST<'_, '_> {}
unsafe impl Sync for ProgramAST<'_, '_> {}

pub type Statement<'allocator, 'input> = ParseResult<'allocator, 'input, StatementAST<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementAST<'allocator, 'input> {
    Assignment(Assignment<'allocator, 'input>),
    Exchange(Exchange<'allocator, 'input>),
    Import(Import<'allocator, 'input>),
    StatementAttributes(Vec<StatementAttribute, &'allocator Bump>),
    VariableDefine(VariableDefine<'allocator, 'input>),
    FunctionDefine(FunctionDefine<'allocator, 'input>),
    UserTypeDefine(UserTypeDefine<'allocator, 'input>),
    TypeDefine(TypeDefine<'allocator, 'input>),
    Implements(Implements<'allocator, 'input>),
    DropStatement(DropStatement<'allocator, 'input>),
    Expression(Expression<'allocator, 'input>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefine<'allocator, 'input> {
    pub attributes: Vec<StatementAttribute, &'allocator Bump>,
    pub generics_define: Option<GenericsDefine<'allocator, 'input>>,
    pub name: FunctionName<'allocator, 'input>,
    pub args: FunctionArguments<'allocator, 'input>,
    pub type_tag: Option<TypeTag<'allocator, 'input>>,
    pub where_clause: Option<WhereClause<'allocator, 'input>>,
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
    Acyclic,
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
    pub arguments: Vec<FunctionArgument<'allocator, 'input>, &'allocator Bump>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub paren_right: ParseResult<'allocator, 'input, ()>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericsDefine<'allocator, 'input> {
    pub elements: Vec<GenericsElement<'allocator, 'input>, &'allocator Bump>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub greater_than: ParseResult<'allocator, 'input, ()>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericsElement<'allocator, 'input> {
    pub name: Literal<'input>,
    pub bounds: Vec<TypeInfo<'allocator, 'input>, &'allocator Bump>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UserTypeDefine<'allocator, 'input> {
    pub attributes: Vec<StatementAttribute, &'allocator Bump>,
    pub kind: UserTypeKind,
    pub name: LiteralResult<'allocator, 'input>,
    pub generics_define: Option<GenericsDefine<'allocator, 'input>>,
    pub super_type_info: Option<SuperTypeInfo<'allocator, 'input>>,
    pub where_clause: Option<WhereClause<'allocator, 'input>>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub block: BlockRecovered<'allocator, 'input>,
    pub span: Range<usize>
}

pub type UserTypeKind = Spanned<DataStructKindEnum>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataStructKindEnum {
    Class,
    Struct,
    Interface
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuperTypeInfo<'allocator, 'input> {
    pub type_infos: Vec<TypeInfo<'allocator, 'input>, &'allocator Bump>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Implements<'allocator, 'input> {
    pub generics_define: Option<GenericsDefine<'allocator, 'input>>,
    pub interface: TypeInfoResult<'allocator, 'input>,
    pub target_user_type: TypeInfoResult<'allocator, 'input>,
    pub where_clause: Option<WhereClause<'allocator, 'input>>,
    pub block: BlockRecovered<'allocator, 'input>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefine<'allocator, 'input> {
    pub name: LiteralResult<'allocator, 'input>,
    pub generics_define: Option<GenericsDefine<'allocator, 'input>>,
    pub type_info: TypeInfoResult<'allocator, 'input>,
    pub error_tokens: ErrorTokens<'allocator, 'input>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhereClause<'allocator, 'input> {
    pub elements: Vec<WhereElement<'allocator, 'input>, &'allocator Bump>,
    pub error_tokens: ErrorTokens<'allocator, 'input>,
    pub next_expected_token: ParseResult<'allocator, 'input, ()>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhereElement<'allocator, 'input> {
    pub target_type: TypeInfo<'allocator, 'input>,
    pub bounds: Vec<TypeInfo<'allocator, 'input>, &'allocator Bump>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import<'allocator, 'input> {
    pub import_path: Vec<Literal<'input>, &'allocator Bump>,
    pub elements: ImportElements<'allocator, 'input>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportElements<'allocator, 'input> {
    pub elements: Vec<Literal<'input>, &'allocator Bump>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub brace_right: ParseResult<'allocator, 'input, ()>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DropStatement<'allocator, 'input> {
    pub acyclic_keyword_span: Option<Range<usize>>,
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
    pub statement_attributes: Vec<StatementAttribute, &'allocator Bump>,
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

impl ExpressionEnum<'_, '_> {
    pub fn get_span(&self) -> Range<usize> {
        match self {
            ExpressionEnum::OrExpression(or_expression) => or_expression.span.clone(),
            ExpressionEnum::ReturnExpression(return_expression) => return_expression.span.clone(),
            ExpressionEnum::Closure(closure) => closure.span.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OrExpression<'allocator, 'input> {
    pub left_expr: AndExpression<'allocator, 'input>,
    pub right_exprs: Vec<(OrOperatorSpan, AndExpressionResult<'allocator, 'input>), &'allocator Bump>,
    pub span: Range<usize>
}

pub type OrOperatorSpan = Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AndExpression<'allocator, 'input> {
    pub left_expr: EQNEExpression<'allocator, 'input>,
    pub right_exprs: Vec<(AndOperatorSpan, EQNEExpressionResult<'allocator, 'input>), &'allocator Bump>,
    pub span: Range<usize>
}

pub type AndOperatorSpan = Range<usize>;
pub type AndExpressionResult<'allocator, 'input> = ParseResult<'allocator, 'input, AndExpression<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EQNEExpression<'allocator, 'input> {
    pub left_expr: CompareExpression<'allocator, 'input>,
    pub right_exprs: Vec<(EQNECompareOp, CompareExpressionResult<'allocator, 'input>), &'allocator Bump>,
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
    pub right_exprs: Vec<(CompareOp, AddOrSubExpressionResult<'allocator, 'input>), &'allocator Bump>,
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
    pub right_exprs: Vec<(AddOrSubOp, MulOrDivExpressionResult<'allocator, 'input>), &'allocator Bump>,
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
    pub right_exprs: Vec<(MulOrDivOp, FactorResult<'allocator, 'input>), &'allocator Bump>,
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
#[repr(C)]
pub struct Factor<'allocator, 'input> {
    pub negative_keyword_span: Option<Range<usize>>,
    pub primary: PrimaryResult<'allocator, 'input>,
    pub span: Range<usize>
}

pub type FactorResult<'allocator, 'input> = ParseResult<'allocator, 'input, Factor<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Primary<'allocator, 'input> {
    pub left: PrimaryLeft<'allocator, 'input>,
    pub chain: Vec<PrimaryRight<'allocator, 'input>, &'allocator Bump>,
    pub span: Range<usize>
}

pub type PrimaryResult<'allocator, 'input> = ParseResult<'allocator, 'input, Primary<'allocator, 'input>>;

#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(C)]
pub struct PrimaryLeft<'allocator, 'input> {
    pub first_expr: PrimaryLeftExpr<'allocator, 'input>,
    pub mapping_operator: Option<MappingOperator<'allocator, 'input>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimaryLeftExpr<'allocator, 'input> {
    Simple((SimplePrimary<'allocator, 'input>, Option<GenericsResult<'allocator, 'input>>, Option<FunctionCall<'allocator, 'input>>)),
    NewExpression(NewExpression<'allocator, 'input>),
    IfExpression(IfExpression<'allocator, 'input>),
    LoopExpression(LoopExpression<'allocator, 'input>)
}

impl PrimaryLeftExpr<'_, '_> {
    pub fn get_span(&self) -> Range<usize> {
        match self {
            PrimaryLeftExpr::Simple((simple_primary, generics, function_call)) => {
                if let Some(function_call) = function_call {
                    let span_start = simple_primary.get_span().start;
                    let span_end = function_call.span.end;
                    span_start..span_end
                } else {
                    if let Some(generics) = generics {
                        if let Ok(generics) = generics {
                            let span_start = simple_primary.get_span().start;
                            let span_end = generics.span.end;
                            span_start..span_end
                        } else {
                            simple_primary.get_span()
                        }
                    } else {
                        simple_primary.get_span()
                    }
                }
            },
            PrimaryLeftExpr::NewExpression(new_expression) => new_expression.span.clone(),
            PrimaryLeftExpr::IfExpression(if_expression) => if_expression.span.clone(),
            PrimaryLeftExpr::LoopExpression(loop_expression) => loop_expression.span.clone()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopExpression<'allocator, 'input> {
    pub block: BlockResult<'allocator, 'input>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfExpression<'allocator, 'input> {
    pub if_statement: IfStatement<'allocator, 'input>,
    pub chain: Vec<ElseIfOrElse<'allocator, 'input>, &'allocator Bump>,
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
    pub second_expr: Option<(Literal<'input>, Option<GenericsResult<'allocator, 'input>>, Option<FunctionCall<'allocator, 'input>>)>,
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
    Expression{
        expression: ExpressionResult<'allocator, 'input>,
        error_tokens: Vec<Token<'input>, &'allocator Bump>,
        span: Range<usize>
    },
    Identifier(Literal<'input>),
    NullKeyword(Range<usize>),
    TrueKeyword(Range<usize>),
    FalseKeyword(Range<usize>)
}

impl SimplePrimary<'_, '_> {
    pub fn get_span(&self) -> Range<usize> {
        match self {
            SimplePrimary::Expression { expression: _, error_tokens: _, span } => {
                span.clone()
            },
            SimplePrimary::Identifier(literal) => literal.span.clone(),
            SimplePrimary::NullKeyword(span) => span.clone(),
            SimplePrimary::TrueKeyword(span) => span.clone(),
            SimplePrimary::FalseKeyword(span) => span.clone()
        }
    }
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
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub arg_exprs: CallArgumentsResult<'allocator, 'input>,
    pub span: Range<usize>
}

pub type FunctionCallResult<'allocator, 'input> = ParseResult<'allocator, 'input, FunctionCall<'allocator, 'input>>;
pub type CallArgumentsResult<'allocator, 'input> = ParseResult<'allocator, 'input, Vec<Expression<'allocator, 'input>, &'allocator Bump>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NewExpression<'allocator, 'input> {
    pub new_keyword_span: Range<usize>,
    pub acyclic_keyword_span: Option<Range<usize>>,
    pub path: Vec<Literal<'input>, &'allocator Bump>,
    pub field_assigns: FieldAssignsResult<'allocator, 'input>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub span: Range<usize>
}

pub type FieldAssignsResult<'allocator, 'input> = ParseResult<'allocator, 'input, Vec<FieldAssign<'allocator, 'input>, &'allocator Bump>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAssign<'allocator, 'input> {
    pub name: Literal<'input>,
    pub expression: ExpressionResult<'allocator, 'input>
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
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub fat_arrow_span: Result<Range<usize>, ()>,
    pub expression_or_block: Recovered<'allocator, 'input, Either<Expression<'allocator, 'input>, Block<'allocator, 'input>>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClosureArguments<'allocator, 'input> {
    pub arguments: Either<Literal<'input>, Vec<Either<FunctionArgument<'allocator, 'input>, Literal<'input>>, &'allocator Bump>>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
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
    pub path: Vec<Literal<'input>, &'allocator Bump>,
    pub generics: Option<Generics<'allocator, 'input>>,
    pub type_attributes: Vec<TypeAttribute<'allocator, 'input>, &'allocator Bump>,
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
    pub elements: Vec<TypeInfo<'allocator, 'input>, &'allocator Bump>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub span: Range<usize>
}

pub type GenericsResult<'allocator, 'input> = ParseResult<'allocator, 'input, Generics<'allocator, 'input>>;

pub type Literal<'input> = Spanned<&'input str>;
pub type LiteralResult<'allocator, 'input> = ParseResult<'allocator, 'input, Literal<'input>>;

pub struct TokenCursor<'allocator, 'input> {
    tokens: Vec<Token<'input>, &'allocator Bump>,
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
        return self.tokens.get(self.current_position.overflowing_sub(1).0);
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

fn read_until_token_found<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>, until: &[TokenKind]) -> Vec<Token<'input>, &'allocator Bump> {
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

fn recover_until_token_found<'allocator, 'input>(cursor: &mut TokenCursor<'allocator, 'input>, until: &[TokenKind]) -> Vec<Token<'input>, &'allocator Bump> {
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
    parser: fn(&mut TokenCursor<'allocator, 'input>) -> Option<T>,
    recover_until: &[TokenKind]
) -> Recovered<'allocator, 'input, T> {

    return match parser(cursor) {
        Some(ast) => Recovered { result: Ok(()), value: Some(ast) },
        _ => {
            let mut error_tokens = recover_until_token_found(cursor, recover_until);
            let ast = parser(cursor);
            if ast.is_none() && error_tokens.is_empty() {
                if let Some(token) = cursor.current() {
                    error_tokens.push(token.clone())
                }
            }
            let error = if error_tokens.is_empty() {
                ASTParseError::UnexpectedEOF
            } else {
                ASTParseError::UnexpectedToken(error_tokens)
            };
            Recovered { result: Err(error), value: ast }
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

impl<'allocator, 'input> MergedExtend for Vec<Token<'input>, &'allocator Bump> {
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