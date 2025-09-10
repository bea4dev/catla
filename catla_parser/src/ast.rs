use std::ops::Range;

use allocator_api2::vec::Vec;
use bumpalo::Bump;

#[derive(Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Range<usize>,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Range<usize>) -> Self {
        Self { value, span }
    }

    pub fn map<V, F: FnOnce(T) -> V>(self, f: F) -> Spanned<V> {
        Spanned::new(f(self.value), self.span)
    }
}

pub type Literal<'input> = Spanned<&'input str>;

#[derive(Debug)]
pub struct Program<'input, 'allocator> {
    pub statements: &'allocator [StatementWithTagAndDocs<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct StatementWithTagAndDocs<'input, 'allocator> {
    pub documents: Documents<'input, 'allocator>,
    pub statement: Statement<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct Documents<'input, 'allocator> {
    pub documents: &'allocator [&'input str],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum Statement<'input, 'allocator> {
    Assignment(Assignment<'input, 'allocator>),
    Swap(SwapStatement<'input, 'allocator>),
    Import(ImportStatement<'input, 'allocator>),
    DefineWithAttribute(DefineWithAttribute<'input, 'allocator>),
    Drop(DropStatement<'input, 'allocator>),
    Expression(Expression<'input, 'allocator>),
    Implements(Implements<'input, 'allocator>),
    TypeAlias(TypeAlias<'input, 'allocator>),
}

#[derive(Debug)]
pub struct DefineWithAttribute<'input, 'allocator> {
    pub attribute: &'allocator [Spanned<StatementAttribute>],
    pub define: Define<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum Define<'input, 'allocator> {
    Function(FunctionDefine<'input, 'allocator>),
    UserType(UserTypeDefine<'input, 'allocator>),
    Variable(),
}

#[derive(Debug)]
pub struct FunctionDefine<'input, 'allocator> {
    pub generics: GenericsDefine<'input, 'allocator>,
    pub name: Literal<'input>,
    pub arguments: FunctionArguments<'input, 'allocator>,
    pub return_type: Option<TypeTag<'input, 'allocator>>,
    pub where_clause: Option<WhereClause<'input, 'allocator>>,
    pub block: Option<Block<'input, 'allocator>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct FunctionArguments<'input, 'allocator> {
    pub this_mulability: Option<ThisMutability>,
    pub arguments: &'allocator [FunctionArgument<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct FunctionArgument<'input, 'allocator> {
    pub binding: VariableBinding<'input, 'allocator>,
    pub type_tag: TypeTag<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum VariableBinding<'input, 'allocator> {
    Literal(Literal<'input>),
    Binding(Vec<VariableBinding<'input, 'allocator>, &'allocator Bump>),
}

#[derive(Debug)]
pub struct ThisMutability {
    pub is_mutable: bool,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StatementAttribute {
    Static,
    Private,
    Suspend,
    Native,
    Acyclic,
    Open,
    Override,
}

#[derive(Debug)]
pub struct WhereClause<'input, 'allocator> {
    pub elements: &'allocator [WhereElement<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct WhereElement<'input, 'allocator> {
    pub target_type: TypeInfo<'input, 'allocator>,
    pub bounds: &'allocator [TypeInfo<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct Block<'input, 'allocator> {
    pub program: &'allocator Program<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct UserTypeDefine<'input, 'allocator> {
    pub name: Literal<'input>,
    pub generics: GenericsDefine<'input, 'allocator>,
    pub super_type: SuperTypeInfo<'input, 'allocator>,
    pub where_clause: WhereClause<'input, 'allocator>,
    pub block: Block<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct SuperTypeInfo<'input, 'allocator> {
    pub types: &'allocator [TypeInfo<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct Implements<'input, 'allocator> {
    pub generics: GenericsDefine<'input, 'allocator>,
    pub interface: TypeInfo<'input, 'allocator>,
    pub target: TypeInfo<'input, 'allocator>,
    pub where_clause: WhereClause<'input, 'allocator>,
    pub block: Block<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct TypeAlias<'input, 'allocator> {
    pub name: Literal<'input>,
    pub generics: GenericsDefine<'input, 'allocator>,
    pub alias_type: TypeInfo<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct ImportStatement<'input, 'allocator> {
    pub path: &'allocator [Literal<'input>],
    pub elements: &'allocator [Literal<'input>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct DropStatement<'input, 'allocator> {
    pub acyclic: Spanned<bool>,
    pub expression: Expression<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct VariableDefine<'input, 'allocator> {
    pub let_var: Spanned<LetVar>,
    pub binding: VariableBinding<'input, 'allocator>,
    pub type_tag: TypeTag<'input, 'allocator>,
    pub expression: Option<Expression<'input, 'allocator>>,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LetVar {
    Let,
    Var,
}

#[derive(Debug)]
pub struct Assignment<'input, 'allocator> {
    pub left: Expression<'input, 'allocator>,
    pub right: Expression<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct SwapStatement<'input, 'allocator> {
    pub left: Expression<'input, 'allocator>,
    pub right: Expression<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum Expression<'input, 'allocator> {
    Return(&'allocator ReturnExpression<'input, 'allocator>),
    Closure(&'allocator Closure<'input, 'allocator>),
    Or(&'allocator OrExpression<'input, 'allocator>),
}

#[derive(Debug)]
pub struct OrExpression<'input, 'allocator> {
    pub left: AndExpression<'input, 'allocator>,
    pub chain: &'allocator [AndExpression<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct AndExpression<'input, 'allocator> {
    pub left: EqualsExpression<'input, 'allocator>,
    pub chain: &'allocator [EqualsExpression<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct EqualsExpression<'input, 'allocator> {
    pub left: LessOrGreaterExpression<'input, 'allocator>,
    pub chain: &'allocator [(
        Spanned<EqualOrNotEqual>,
        LessOrGreaterExpression<'input, 'allocator>,
    )],
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EqualOrNotEqual {
    Equal,
    NotEqual,
}

#[derive(Debug)]
pub struct LessOrGreaterExpression<'input, 'allocator> {
    pub left: AddOrSubExpression<'input, 'allocator>,
    pub chain: &'allocator [(
        Spanned<LessOrGreater>,
        AddOrSubExpression<'input, 'allocator>,
    )],
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LessOrGreater {
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[derive(Debug)]
pub struct AddOrSubExpression<'input, 'allocator> {
    pub left: MulOrDivExpression<'input, 'allocator>,
    pub chain: &'allocator [(Spanned<AddOrSub>, MulOrDivExpression<'input, 'allocator>)],
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddOrSub {
    Add,
    Sub,
}

#[derive(Debug)]
pub struct MulOrDivExpression<'input, 'allocator> {
    pub left: Factor<'input, 'allocator>,
    pub chain: &'allocator [(Spanned<MulOrDiv>, Factor<'input, 'allocator>)],
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MulOrDiv {
    Mul,
    Div,
}

#[derive(Debug)]
pub struct Factor<'input, 'allocator> {
    pub minus: Option<Range<usize>>,
    pub primary: Result<Primary<'input, 'allocator>, ()>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct Primary<'input, 'allocator> {
    pub left: PrimaryLeft<'input, 'allocator>,
    pub chain: &'allocator [PrimaryRight<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct PrimaryLeft<'input, 'allocator> {
    pub first: PrimaryLeftExpr<'input, 'allocator>,
    pub mapping_operator: Option<MappingOperator<'input, 'allocator>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum PrimaryLeftExpr<'input, 'allocator> {
    Simple {
        left: SimplePrimary<'input, 'allocator>,
        generics: Option<GenericsInfo<'input, 'allocator>>,
        function_call: Option<FunctionCall<'input, 'allocator>>,
        span: Range<usize>,
    },
    NewObject {
        new_object: NewObjectExpression<'input, 'allocator>,
    },
    NewArray {
        new_array: NewArrayExpression<'input, 'allocator>,
    },
    NewArrayInit {
        new_array_init: NewArrayInitExpression<'input, 'allocator>,
    },
    If {
        if_expression: IfExpression<'input, 'allocator>,
    },
    Loop {
        loop_expression: LoopExpression<'input, 'allocator>,
    },
}

#[derive(Debug)]
pub enum SimplePrimary<'input, 'allocator> {
    Tuple {
        expressions: Vec<Expression<'input, 'allocator>, &'allocator Bump>,
        span: Range<usize>,
    },
    Literal(Literal<'input>),
    StringLiteral(Literal<'input>),
    Null(Range<usize>),
    True(Range<usize>),
    False(Range<usize>),
    This(Range<usize>),
    LargeThis(Range<usize>),
}

#[derive(Debug)]
pub struct FunctionCall<'input, 'allocator> {
    pub arguments: &'allocator [Expression<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct PrimaryRight<'input, 'allocator> {
    pub separator: Spanned<PrimarySeparator>,
    pub second: Option<PrimaryRightExpr<'input, 'allocator>>,
    pub mapping_operator: Option<MappingOperator<'input, 'allocator>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct PrimaryRightExpr<'input, 'allocator> {
    pub literal: Literal<'input>,
    pub generics: Option<GenericsInfo<'input, 'allocator>>,
    pub function_call: Option<FunctionCall<'input, 'allocator>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimarySeparator {
    Dot,
    DoubleColon,
}

#[derive(Debug)]
pub enum MappingOperator<'input, 'allocator> {
    NullPropagation {
        span: Range<usize>,
    },
    NullUnwrap {
        span: Range<usize>,
    },
    NullElvis {
        block: Result<Block<'input, 'allocator>, ()>,
        span: Range<usize>,
    },
    ResultPropagation {
        span: Range<usize>,
    },
    ResultUnwrap {
        span: Range<usize>,
    },
    ResultElvis {
        block: Result<Block<'input, 'allocator>, ()>,
        span: Range<usize>,
    },
}

#[derive(Debug)]
pub struct IfExpression<'input, 'allocator> {
    pub first: IfStatement<'input, 'allocator>,
    pub chain: &'allocator [ElseChain<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct IfStatement<'input, 'allocator> {
    pub if_keyword: Range<usize>,
    pub condition: Result<Expression<'input, 'allocator>, ()>,
    pub block: Result<Block<'input, 'allocator>, ()>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum ElseChain<'input, 'allocator> {
    ElseIf {
        if_statement: IfStatement<'input, 'allocator>,
    },
    Else {
        block: Block<'input, 'allocator>,
    },
}

#[derive(Debug)]
pub struct LoopExpression<'input, 'allocator> {
    pub loop_keyword: Range<usize>,
    pub block: Block<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct Closure<'input, 'allocator> {
    pub arguments: ClosureArgumentsOrLiteral<'input, 'allocator>,
    pub expression_or_block: ExpressionOrBlock<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum ExpressionOrBlock<'input, 'allocator> {
    Expression(Expression<'input, 'allocator>),
    Block(Block<'input, 'allocator>),
}

#[derive(Debug)]
pub enum ClosureArgumentsOrLiteral<'input, 'allocator> {
    ClosureArgument(ClosureArguments<'input, 'allocator>),
    Literal(Literal<'input>),
}

#[derive(Debug)]
pub struct ClosureArguments<'input, 'allocator> {
    pub arguments: &'allocator [FunctionArgumentOrLiteral<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum FunctionArgumentOrLiteral<'input, 'allocator> {
    FunctionArgument(FunctionArgument<'input, 'allocator>),
    Literal(Literal<'input>),
}

#[derive(Debug)]
pub struct NewObjectExpression<'input, 'allocator> {
    pub new: Range<usize>,
    pub acyclic: Option<Range<usize>>,
    pub path: &'allocator [Literal<'input>],
    pub field_assign: FieldAssign<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct FieldAssign<'input, 'allocator> {
    pub elements: &'allocator [FieldAssignElement<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct FieldAssignElement<'input, 'allocator> {
    pub field: Literal<'input>,
    pub expression: Expression<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct NewArrayExpression<'input, 'allocator> {
    pub new: Range<usize>,
    pub acyclic: Option<Range<usize>>,
    pub elements: &'allocator [Expression<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct NewArrayInitExpression<'input, 'allocator> {
    pub new: Range<usize>,
    pub acyclic: Option<Range<usize>>,
    pub for_keyword: Option<Range<usize>>,
    pub init_expression: Result<Expression<'input, 'allocator>, ()>,
    pub length_expression: Result<Expression<'input, 'allocator>, ()>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct ReturnExpression<'input, 'allocator> {
    pub return_keyword: Range<usize>,
    pub expression: Option<Expression<'input, 'allocator>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct GenericsDefine<'input, 'allocator> {
    pub elements: &'allocator [GenericsElement<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct GenericsElement<'input, 'allocator> {
    pub name: Literal<'input>,
    pub bounds: &'allocator [TypeInfo<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum TypeInfo<'input, 'allocator> {
    Array(ArrayTypeInfo<'input, 'allocator>),
    Base(BaseTypeInfo<'input, 'allocator>),
    Tuple(TupleTypeInfo<'input, 'allocator>),
}

#[derive(Debug)]
pub struct ArrayTypeInfo<'input, 'allocator> {
    pub base_type: &'allocator TypeInfo<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct BaseTypeInfo<'input, 'allocator> {
    pub path: &'allocator [Literal<'input>],
    pub generics: GenericsInfo<'input, 'allocator>,
    pub attribute: TypeAttribute<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct GenericsInfo<'input, 'allocator> {
    pub types: &'allocator [TypeInfo<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum TypeAttribute<'input, 'allocator> {
    Optional {
        span: Range<usize>,
    },
    Result {
        generics: GenericsInfo<'input, 'allocator>,
        span: Range<usize>,
    },
}

#[derive(Debug)]
pub struct TupleTypeInfo<'input, 'allocator> {
    pub types: &'allocator [TypeInfo<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct TypeTag<'input, 'allocator> {
    pub type_info: TypeInfo<'input, 'allocator>,
    pub span: Range<usize>,
}
