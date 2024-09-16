pub mod statement;
pub mod expression;
pub mod types;

use std::{alloc::Allocator, ops::Range};
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
pub enum ASTParseError<'input, 'allocator> {
    UnexpectedToken(Vec<Token<'input>, &'allocator Bump>),
    UnexpectedEOF
}

pub type ParseResult<'allocator, 'input, T> = Result<T, ASTParseError<'input, 'allocator>>;
pub type ErrorTokens<'input, 'allocator> = Vec<Vec<Token<'input>, &'allocator Bump>, &'allocator Bump>;


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
    NewArrayInitExpression<'_, '_>,
    NewArrayExpression<'_, '_>,
    NewExpression<'_, '_>,
    FieldAssign<'_, '_>,
    ElseIfOrElse<'_, '_>,
    ReturnExpression<'_, '_>,
    Closure<'_, '_>,
    ClosureArguments<'_, '_>,
    TypeTag<'_, '_>,
    TypeInfo<'_, '_>,
    BaseTypeInfo<'_, '_>,
    ArrayTypeInfo<'_, '_>,
    TypeAttributeEnum<'_, '_>,
    Generics<'_, '_>,
    Literal<'_>
}

pub type Program<'input, 'allocator> = &'allocator ProgramAST<'input, 'allocator>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProgramAST<'input, 'allocator> {
    pub statements: Vec<Statement<'input, 'allocator>, &'allocator Bump>,
    pub not_separated_stmts: Vec<Token<'input>, &'allocator Bump>,
    pub span: Range<usize>
}
unsafe impl Send for ProgramAST<'_, '_> {}
unsafe impl Sync for ProgramAST<'_, '_> {}

pub type Statement<'input, 'allocator> = ParseResult<'allocator, 'input, StatementAST<'input, 'allocator>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementAST<'input, 'allocator> {
    Assignment(Assignment<'input, 'allocator>),
    Exchange(Exchange<'input, 'allocator>),
    Import(Import<'input, 'allocator>),
    StatementAttributes(Vec<StatementAttribute, &'allocator Bump>),
    VariableDefine(VariableDefine<'input, 'allocator>),
    FunctionDefine(FunctionDefine<'input, 'allocator>),
    UserTypeDefine(UserTypeDefine<'input, 'allocator>),
    TypeDefine(TypeDefine<'input, 'allocator>),
    Implements(Implements<'input, 'allocator>),
    DropStatement(DropStatement<'input, 'allocator>),
    Expression(Expression<'input, 'allocator>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefine<'input, 'allocator> {
    pub attributes: Vec<StatementAttribute, &'allocator Bump>,
    pub generics_define: Option<GenericsDefine<'input, 'allocator>>,
    pub name: LiteralResult<'input, 'allocator>,
    pub args: FunctionArguments<'input, 'allocator>,
    pub type_tag: Option<TypeTag<'input, 'allocator>>,
    pub where_clause: Option<WhereClause<'input, 'allocator>>,
    pub block_or_semicolon: Recovered<'allocator, 'input, Either<Range<usize>, Block<'input, 'allocator>>>,
    pub span: Range<usize>
}

pub type StatementAttribute = Spanned<StatementAttributeKind>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementAttributeKind {
    Static,
    Private,
    Suspend,
    Native,
    Acyclic,
    Open,
    Override
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionArgument<'input, 'allocator> {
    pub binding: VariableBinding<'input, 'allocator>,
    pub type_tag: TypeTag<'input, 'allocator>,
    pub span: Range<usize>
}

pub type FunctionArgumentResult<'input, 'allocator> = ParseResult<'allocator, 'input, FunctionArgument<'input, 'allocator>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionArguments<'input, 'allocator> {
    pub paren_left: ParseResult<'allocator, 'input, ()>,
    pub this_mutability: Option<ThisMutability<'input, 'allocator>>,
    pub arguments: Vec<FunctionArgument<'input, 'allocator>, &'allocator Bump>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub paren_right: ParseResult<'allocator, 'input, ()>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ThisMutability<'input, 'allocator> {
    pub is_var: Spanned<bool>,
    pub this_span: ParseResult<'allocator, 'input, Range<usize>>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericsDefine<'input, 'allocator> {
    pub elements: Vec<GenericsElement<'input, 'allocator>, &'allocator Bump>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub greater_than: ParseResult<'allocator, 'input, ()>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericsElement<'input, 'allocator> {
    pub name: Literal<'input>,
    pub bounds: Vec<TypeInfo<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UserTypeDefine<'input, 'allocator> {
    pub attributes: Vec<StatementAttribute, &'allocator Bump>,
    pub kind: UserTypeKind,
    pub name: LiteralResult<'input, 'allocator>,
    pub generics_define: Option<GenericsDefine<'input, 'allocator>>,
    pub super_type_info: Option<SuperTypeInfo<'input, 'allocator>>,
    pub where_clause: Option<WhereClause<'input, 'allocator>>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub block: BlockRecovered<'input, 'allocator>,
    pub span: Range<usize>
}

pub type UserTypeKind = Spanned<UserTypeKindEnum>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UserTypeKindEnum {
    Class,
    Struct,
    Interface
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuperTypeInfo<'input, 'allocator> {
    pub type_infos: Vec<TypeInfo<'input, 'allocator>, &'allocator Bump>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Implements<'input, 'allocator> {
    pub generics_define: Option<GenericsDefine<'input, 'allocator>>,
    pub interface: TypeInfoResult<'input, 'allocator>,
    pub target_user_type: TypeInfoResult<'input, 'allocator>,
    pub where_clause: Option<WhereClause<'input, 'allocator>>,
    pub block: BlockRecovered<'input, 'allocator>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefine<'input, 'allocator> {
    pub name: LiteralResult<'input, 'allocator>,
    pub generics_define: Option<GenericsDefine<'input, 'allocator>>,
    pub type_info: TypeInfoResult<'input, 'allocator>,
    pub error_tokens: ErrorTokens<'input, 'allocator>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhereClause<'input, 'allocator> {
    pub elements: Vec<WhereElement<'input, 'allocator>, &'allocator Bump>,
    pub error_tokens: ErrorTokens<'input, 'allocator>,
    pub next_expected_token: ParseResult<'allocator, 'input, ()>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhereElement<'input, 'allocator> {
    pub target_type: TypeInfo<'input, 'allocator>,
    pub bounds: Vec<TypeInfo<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import<'input, 'allocator> {
    pub import_path: Vec<Literal<'input>, &'allocator Bump>,
    pub elements: ImportElements<'input, 'allocator>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportElements<'input, 'allocator> {
    pub elements: Vec<Literal<'input>, &'allocator Bump>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub brace_right: ParseResult<'allocator, 'input, ()>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DropStatement<'input, 'allocator> {
    pub acyclic_keyword_span: Option<Range<usize>>,
    pub expression: ExpressionResult<'input, 'allocator>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block<'input, 'allocator> {
    pub program: Program<'input, 'allocator>,
    pub brace_right: ParseResult<'allocator, 'input, ()>,
    pub span: Range<usize>
}

pub type BlockResult<'input, 'allocator> = ParseResult<'allocator, 'input, Block<'input, 'allocator>>;
pub type BlockRecovered<'input, 'allocator> = Recovered<'allocator, 'input, Block<'input, 'allocator>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableDefine<'input, 'allocator> {
    pub attributes: VariableAttributes<'allocator>,
    pub binding: VariableBindingResult<'input, 'allocator>,
    pub type_tag: Option<TypeTag<'input, 'allocator>>,
    pub expression: Option<ExpressionResult<'input, 'allocator>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableAttributes<'allocator> {
    pub statement_attributes: Vec<StatementAttribute, &'allocator Bump>,
    pub is_var: bool,
    pub var_let_span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableBinding<'input, 'allocator> {
    pub binding: Either<Literal<'input>, Vec<VariableBinding<'input, 'allocator>, &'allocator Bump>>,
    pub error_tokens: Vec<Vec<Token<'input>, &'allocator Bump>, &'allocator Bump>,
    pub span: Range<usize>
}

pub type VariableBindingResult<'input, 'allocator> = ParseResult<'allocator, 'input, VariableBinding<'input, 'allocator>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment<'input, 'allocator> {
    pub left_expr: Expression<'input, 'allocator>,
    pub right_expr: ParseResult<'allocator, 'input, Expression<'input, 'allocator>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exchange<'input, 'allocator> {
    pub left_expr: Expression<'input, 'allocator>,
    pub right_expr: ParseResult<'allocator, 'input, Expression<'input, 'allocator>>,
    pub span: Range<usize>
}

pub type Expression<'input, 'allocator> = &'allocator ExpressionEnum<'input, 'allocator>;
pub type ExpressionResult<'input, 'allocator> = ParseResult<'allocator, 'input, Expression<'input, 'allocator>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionEnum<'input, 'allocator> {
    OrExpression(OrExpression<'input, 'allocator>),
    ReturnExpression(ReturnExpression<'input, 'allocator>),
    Closure(Closure<'input, 'allocator>)
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
pub struct OrExpression<'input, 'allocator> {
    pub left_expr: AndExpression<'input, 'allocator>,
    pub right_exprs: Vec<(OrOperatorSpan, AndExpressionResult<'input, 'allocator>), &'allocator Bump>,
    pub span: Range<usize>
}

pub type OrOperatorSpan = Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AndExpression<'input, 'allocator> {
    pub left_expr: CompareExpression<'input, 'allocator>,
    pub right_exprs: Vec<(AndOperatorSpan, CompareExpressionResult<'input, 'allocator>), &'allocator Bump>,
    pub span: Range<usize>
}

pub type AndOperatorSpan = Range<usize>;
pub type AndExpressionResult<'input, 'allocator> = ParseResult<'allocator, 'input, AndExpression<'input, 'allocator>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareExpression<'input, 'allocator> {
    pub left_expr: AddOrSubExpression<'input, 'allocator>,
    pub right_exprs: Vec<(CompareOp, AddOrSubExpressionResult<'input, 'allocator>), &'allocator Bump>,
    pub span: Range<usize>
}

pub type CompareOp = Spanned<CompareOpKind>;
pub type CompareExpressionResult<'input, 'allocator> = ParseResult<'allocator, 'input, CompareExpression<'input, 'allocator>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompareOpKind {
    GreaterThan,
    GreaterOrEqual,
    LessThan,
    LessOrEqual,
    Equal,
    NotEqual
}

impl CompareOpKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            CompareOpKind::GreaterThan    => ">",
            CompareOpKind::GreaterOrEqual => ">=",
            CompareOpKind::LessThan       => "<",
            CompareOpKind::LessOrEqual    => "<=",
            CompareOpKind::Equal          => "==",
            CompareOpKind::NotEqual       => "!="
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AddOrSubExpression<'input, 'allocator> {
    pub left_expr: MulOrDivExpression<'input, 'allocator>,
    pub right_exprs: Vec<(AddOrSubOp, MulOrDivExpressionResult<'input, 'allocator>), &'allocator Bump>,
    pub span: Range<usize>
}

pub type AddOrSubOp = Spanned<AddOrSubOpKind>;
pub type AddOrSubExpressionResult<'input, 'allocator> = ParseResult<'allocator, 'input, AddOrSubExpression<'input, 'allocator>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AddOrSubOpKind {
    Add,
    Sub
}

impl AddOrSubOpKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            AddOrSubOpKind::Add => "+",
            AddOrSubOpKind::Sub => "-"
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MulOrDivExpression<'input, 'allocator> {
    pub left_expr: Factor<'input, 'allocator>,
    pub right_exprs: Vec<(MulOrDivOp, FactorResult<'input, 'allocator>), &'allocator Bump>,
    pub span: Range<usize>
}

pub type MulOrDivOp = Spanned<MulOrDivOpKind>;
pub type MulOrDivExpressionResult<'input, 'allocator> = ParseResult<'allocator, 'input, MulOrDivExpression<'input, 'allocator>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MulOrDivOpKind {
    Mul,
    Div
}

impl MulOrDivOpKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            MulOrDivOpKind::Mul => "*",
            MulOrDivOpKind::Div => "/"
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(C)]
pub struct Factor<'input, 'allocator> {
    pub negative_keyword_span: Option<Range<usize>>,
    pub primary: PrimaryResult<'input, 'allocator>,
    pub span: Range<usize>
}

pub type FactorResult<'input, 'allocator> = ParseResult<'allocator, 'input, Factor<'input, 'allocator>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Primary<'input, 'allocator> {
    pub left: PrimaryLeft<'input, 'allocator>,
    pub chain: Vec<PrimaryRight<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>
}

pub type PrimaryResult<'input, 'allocator> = ParseResult<'allocator, 'input, Primary<'input, 'allocator>>;

#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(C)]
pub struct PrimaryLeft<'input, 'allocator> {
    pub first_expr: PrimaryLeftExpr<'input, 'allocator>,
    pub mapping_operator: Option<MappingOperator<'input, 'allocator>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimaryLeftExpr<'input, 'allocator> {
    Simple((SimplePrimary<'input, 'allocator>, Option<GenericsResult<'input, 'allocator>>, Option<FunctionCall<'input, 'allocator>>)),
    NewArrayInitExpression(NewArrayInitExpression<'input, 'allocator>),
    NewArrayExpression(NewArrayExpression<'input, 'allocator>),
    NewExpression(NewExpression<'input, 'allocator>),
    IfExpression(IfExpression<'input, 'allocator>),
    LoopExpression(LoopExpression<'input, 'allocator>)
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
            PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => new_array_init_expression.span.clone(),
            PrimaryLeftExpr::NewArrayExpression(new_array_expression) => new_array_expression.span.clone(),
            PrimaryLeftExpr::NewExpression(new_expression) => new_expression.span.clone(),
            PrimaryLeftExpr::IfExpression(if_expression) => if_expression.span.clone(),
            PrimaryLeftExpr::LoopExpression(loop_expression) => loop_expression.span.clone()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopExpression<'input, 'allocator> {
    pub block: BlockResult<'input, 'allocator>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfExpression<'input, 'allocator> {
    pub if_statement: IfStatement<'input, 'allocator>,
    pub chain: Vec<ElseIfOrElse<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStatement<'input, 'allocator> {
    pub if_keyword_span: Range<usize>,
    pub condition: ExpressionResult<'input, 'allocator>,
    pub block: BlockRecovered<'input, 'allocator>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrimaryRight<'input, 'allocator> {
    pub separator: PrimarySeparator,
    pub second_expr: Option<(Literal<'input>, Option<GenericsResult<'input, 'allocator>>, Option<FunctionCall<'input, 'allocator>>)>,
    pub mapping_operator: Option<MappingOperator<'input, 'allocator>>,
    pub span: Range<usize>
}

pub type PrimarySeparator = Spanned<PrimarySeparatorKind>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimarySeparatorKind {
    Dot,
    DoubleColon
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimplePrimary<'input, 'allocator> {
    Expressions{
        expressions: Vec<Expression<'input, 'allocator>, &'allocator Bump>,
        error_tokens: Vec<Vec<Token<'input>, &'allocator Bump>, &'allocator Bump>,
        span: Range<usize>
    },
    Identifier(Literal<'input>),
    NullKeyword(Range<usize>),
    TrueKeyword(Range<usize>),
    FalseKeyword(Range<usize>),
    ThisKeyword(Literal<'input>),
    LargeThisKeyword(Literal<'input>)
}

impl SimplePrimary<'_, '_> {
    pub fn get_span(&self) -> Range<usize> {
        match self {
            SimplePrimary::Expressions { expressions: _, error_tokens: _, span } => {
                span.clone()
            },
            SimplePrimary::Identifier(literal) => literal.span.clone(),
            SimplePrimary::NullKeyword(span) => span.clone(),
            SimplePrimary::TrueKeyword(span) => span.clone(),
            SimplePrimary::FalseKeyword(span) => span.clone(),
            SimplePrimary::ThisKeyword(literal) => literal.span.clone(),
            SimplePrimary::LargeThisKeyword(literal) => literal.span.clone()
        }
    }
}

pub type MappingOperator<'input, 'allocator> = Spanned<MappingOperatorKind<'input, 'allocator>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MappingOperatorKind<'input, 'allocator> {
    NullPropagation,
    NullUnwrap,
    NullElvisBlock(BlockRecovered<'input, 'allocator>),
    ResultPropagation,
    ResultUnwrap,
    ResultElvisBlock(BlockRecovered<'input, 'allocator>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionCall<'input, 'allocator> {
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub arg_exprs: CallArgumentsResult<'input, 'allocator>,
    pub span: Range<usize>
}

pub type FunctionCallResult<'input, 'allocator> = ParseResult<'allocator, 'input, FunctionCall<'input, 'allocator>>;
pub type CallArgumentsResult<'input, 'allocator> = ParseResult<'allocator, 'input, Vec<Expression<'input, 'allocator>, &'allocator Bump>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NewExpression<'input, 'allocator> {
    pub new_keyword_span: Range<usize>,
    pub acyclic_keyword_span: Option<Range<usize>>,
    pub path: Vec<Literal<'input>, &'allocator Bump>,
    pub field_assigns: FieldAssignsResult<'input, 'allocator>,
    pub error_tokens: Vec<Vec<Token<'input>, &'allocator Bump>, &'allocator Bump>,
    pub span: Range<usize>
}

pub type FieldAssignsResult<'input, 'allocator> = ParseResult<'allocator, 'input, Vec<FieldAssign<'input, 'allocator>, &'allocator Bump>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAssign<'input, 'allocator> {
    pub name: Literal<'input>,
    pub expression: ExpressionResult<'input, 'allocator>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NewArrayInitExpression<'input, 'allocator> {
    pub new_keyword_span: Range<usize>,
    pub acyclic_keyword_span: Option<Range<usize>>,
    pub for_keyword_span: Option<Range<usize>>,
    pub init_expression: ParseResult<'allocator, 'input, Expression<'input, 'allocator>>,
    pub semicolon: ParseResult<'allocator, 'input, ()>,
    pub length_expression: ParseResult<'allocator, 'input, Expression<'input, 'allocator>>,
    pub bracket_right: ParseResult<'allocator, 'input, ()>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NewArrayExpression<'input, 'allocator> {
    pub new_keyword_span: Range<usize>,
    pub acyclic_keyword_span: Option<Range<usize>>,
    pub value_expressions: Vec<ExpressionResult<'input, 'allocator>, &'allocator Bump>,
    pub error_tokens: Vec<Vec<Token<'input>, &'allocator Bump>, &'allocator Bump>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElseIfOrElse<'input, 'allocator> {
    pub else_keyword_span: Range<usize>,
    pub else_if_or_else: Recovered<'allocator, 'input, Either<IfStatement<'input, 'allocator>, Block<'input, 'allocator>>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnExpression<'input, 'allocator> {
    pub return_keyword_span: Range<usize>,
    pub expression: Option<Expression<'input, 'allocator>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure<'input, 'allocator> {
    pub arguments: ClosureArguments<'input, 'allocator>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub fat_arrow_span: Result<Range<usize>, ()>,
    pub expression_or_block: Recovered<'allocator, 'input, Either<Expression<'input, 'allocator>, Block<'input, 'allocator>>>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClosureArguments<'input, 'allocator> {
    pub arguments: Either<Literal<'input>, Vec<Either<FunctionArgument<'input, 'allocator>, Literal<'input>>, &'allocator Bump>>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub vertical_bar_right: ParseResult<'allocator, 'input, ()>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeTag<'input, 'allocator> {
    pub tag_kind: TypeTagKind,
    pub type_info: TypeInfoResult<'input, 'allocator>,
    pub span: Range<usize>
}

pub type TypeTagKind = Spanned<TypeTagKindEnum>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeTagKindEnum {
    Normal,
    ReturnType
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInfo<'input, 'allocator> {
    BaseType(BaseTypeInfo<'input, 'allocator>),
    ArrayType(ArrayTypeInfo<'input, 'allocator>),
    TupleType(TupleTypeInfo<'input, 'allocator>)
}

impl TypeInfo<'_, '_> {
    pub fn get_span(&self) -> Range<usize> {
        match self {
            TypeInfo::BaseType(base_type_info) => base_type_info.span.clone(),
            TypeInfo::ArrayType(array_type_info) => array_type_info.span.clone(),
            TypeInfo::TupleType(tuple_type_info) => tuple_type_info.span.clone()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TupleTypeInfo<'input, 'allocator> {
    pub types: Vec<TypeInfo<'input, 'allocator>, &'allocator Bump>,
    pub error_tokens: Vec<Vec<Token<'input>, &'allocator Bump>, &'allocator Bump>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayTypeInfo<'input, 'allocator> {
    pub type_info: ParseResult<'allocator, 'input, &'allocator TypeInfo<'input, 'allocator>>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub bracket_right: ParseResult<'allocator, 'input, ()>,
    pub span: Range<usize>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BaseTypeInfo<'input, 'allocator> {
    pub path: Vec<Literal<'input>, &'allocator Bump>,
    pub generics: Option<Generics<'input, 'allocator>>,
    pub type_attributes: Vec<TypeAttribute<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>
}

pub type TypeInfoResult<'input, 'allocator> = ParseResult<'allocator, 'input, TypeInfo<'input, 'allocator>>;

pub type TypeAttribute<'input, 'allocator> = Spanned<TypeAttributeEnum<'input, 'allocator>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAttributeEnum<'input, 'allocator> {
    Optional,
    Result(Option<Generics<'input, 'allocator>>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Generics<'input, 'allocator> {
    pub elements: Vec<TypeInfo<'input, 'allocator>, &'allocator Bump>,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub span: Range<usize>
}

pub type GenericsResult<'input, 'allocator> = ParseResult<'allocator, 'input, Generics<'input, 'allocator>>;

pub type Literal<'input> = Spanned<&'input str>;
pub type LiteralResult<'input, 'allocator> = ParseResult<'allocator, 'input, Literal<'input>>;

pub struct TokenCursor<'input, 'allocator> {
    tokens: Vec<Token<'input>, &'allocator Bump>,
    current_position: usize,
    allocator: &'allocator Bump
}

impl<'input, 'allocator> TokenCursor<'input, 'allocator> {
    
    fn new(allocator: &'allocator Bump, lexer: Lexer<'input>) -> TokenCursor<'input, 'allocator> {
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
    pub fn start<'input, 'allocator>(cursor: &TokenCursor<'input, 'allocator>) -> Span {
        return Self {
            start_position: cursor.current().map_or(0, |token| { token.span.start })
        }
    }

    pub fn elapsed<'input, 'allocator>(&self, cursor: &TokenCursor<'input, 'allocator>) -> Range<usize> {
        return self.start_position..cursor.peek_prev().map_or(0, |prev| { prev.span.end });
    }
}




pub fn parse_source<'input, 'allocator>(source: &'input str, allocator: &'allocator Bump) -> Program<'input, 'allocator> {
    let lexer = Lexer::new(source);
    let mut cursor = TokenCursor::new(allocator, lexer);
    return parse_program(&mut cursor, &[TokenKind::None]);
}

fn parse_program<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>, end: &[TokenKind]) -> Program<'input, 'allocator> {
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

fn skip<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>, skip_kinds: &[TokenKind]) -> usize {
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

fn parse_literal<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>) -> Option<Literal<'input>> {
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

fn parse_as_literal<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>) -> Option<Literal<'input>> {
    return match cursor.next() {
        Some(next) => Some(Spanned::new(next.text, next.span.clone())),
        _ => None
    }
}

fn parse_literal_result<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>) -> LiteralResult<'input, 'allocator> {
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

fn read_until_token_found<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>, until: &[TokenKind]) -> Vec<Token<'input>, &'allocator Bump> {
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

fn recover_until_token_found<'input, 'allocator>(cursor: &mut TokenCursor<'input, 'allocator>, until: &[TokenKind]) -> Vec<Token<'input>, &'allocator Bump> {
    let dropped_tokens = read_until_token_found(cursor, until);
    cursor.prev();
    return dropped_tokens;
}

fn unexpected_token_error<'input, 'allocator>(allocator: &'allocator Bump, token: Option<&Token<'input>>) -> ASTParseError<'input, 'allocator> {
    return match token {
        Some(next) => ASTParseError::UnexpectedToken(bump_vec![allocator, next.clone()]),
        _ => ASTParseError::UnexpectedEOF
    }
}

fn parse_with_recover<'allocator, 'input, T>(
    cursor: &mut TokenCursor<'input, 'allocator>,
    parser: fn(&mut TokenCursor<'input, 'allocator>) -> Option<T>,
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

impl<'input, 'allocator> MergedExtend for Vec<Token<'input>, &'allocator Bump> {
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


pub trait StatementAttributes {
    fn contains_kind(&self, kind: StatementAttributeKind) -> bool;
    fn get_span(&self, kind: StatementAttributeKind) -> Option<Range<usize>>;
}

impl<A: Allocator> StatementAttributes for Vec<StatementAttribute, A> {
    
    fn contains_kind(&self, kind: StatementAttributeKind) -> bool {
        self.iter().any(|attribute| { attribute.value == kind })
    }

    fn get_span(&self, kind: StatementAttributeKind) -> Option<Range<usize>> {
        self.iter()
            .find(|attribute| { attribute.value == kind })
            .map(|attribute| { attribute.span.clone() })
    }

}
