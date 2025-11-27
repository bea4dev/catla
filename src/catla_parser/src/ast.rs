use std::{any::TypeId, mem::transmute, ops::Range};

pub trait AST {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EntityID((TypeId, usize));

impl<T: AST + Sized> From<&T> for EntityID {
    fn from(value: &T) -> Self {
        Self((typeid::of::<T>(), unsafe { transmute(value) }))
    }
}

macro_rules! impl_ast {
    (for <$($gen:tt),+> $ty:ty where $($w:tt)*) => {
        impl<$($gen),+> AST for $ty where $($w)* {}
    };
    (for <$($gen:tt),+> $ty:ty) => {
        impl<$($gen),+> AST for $ty {}
    };
    ($ty:ty) => {
        impl AST for $ty {}
    };
}

impl_ast!(for<T> Spanned<T>);
impl_ast!(for<'input, 'allocator> Program<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> StatementWithTagAndDocs<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> Documents<'input, 'allocator>);
impl_ast!(for<'input> CompilerTag<'input>);
impl_ast!(for<'input, 'allocator> Statement<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> DefineWithAttribute<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> Define<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> FunctionDefine<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> FunctionArguments<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> FunctionArgument<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> VariableBinding<'input, 'allocator>);
impl_ast!(ThisMutability);
impl_ast!(StatementAttribute);
impl_ast!(for<'input, 'allocator> WhereClause<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> WhereElement<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> Block<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> UserTypeDefine<'input, 'allocator>);
impl_ast!(UserTypeKind);
impl_ast!(for<'input, 'allocator> SuperTypeInfo<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> Implements<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> TypeAlias<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> ImportStatement<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> DropStatement<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> VariableDefine<'input, 'allocator>);
impl_ast!(LetVar);
impl_ast!(for<'input, 'allocator> Assignment<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> SwapStatement<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> Expression<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> OrExpression<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> AndExpression<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> EqualsExpression<'input, 'allocator>);
impl_ast!(EqualOrNotEqual);
impl_ast!(for<'input, 'allocator> LessOrGreaterExpression<'input, 'allocator>);
impl_ast!(LessOrGreater);
impl_ast!(for<'input, 'allocator> AddOrSubExpression<'input, 'allocator>);
impl_ast!(AddOrSub);
impl_ast!(for<'input, 'allocator> MulOrDivExpression<'input, 'allocator>);
impl_ast!(MulOrDiv);
impl_ast!(for<'input, 'allocator> Factor<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> Primary<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> PrimaryLeft<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> PrimaryLeftExpr<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> SimplePrimary<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> FunctionCall<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> PrimaryRight<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> PrimaryRightExpr<'input, 'allocator>);
impl_ast!(PrimarySeparator);
impl_ast!(for<'input, 'allocator> MappingOperator<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> IfExpression<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> IfStatement<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> ElseChain<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> LoopExpression<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> Closure<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> ExpressionOrBlock<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> ClosureArgumentsOrLiteral<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> ClosureArguments<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> FunctionArgumentOrVariableBinding<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> NewObjectExpression<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> FieldAssign<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> FieldAssignElement<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> NewArrayExpression<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> NewArrayInitExpression<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> ReturnExpression<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> GenericsDefine<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> GenericsElement<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> TypeInfo<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> TypeInfoBase<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> ArrayTypeInfo<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> BaseTypeInfo<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> GenericsInfo<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> TypeAttribute<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> TupleTypeInfo<'input, 'allocator>);
impl_ast!(for<'input, 'allocator> TypeTag<'input, 'allocator>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

pub trait WithSpan: Sized {
    fn with_span(self, span: Range<usize>) -> Spanned<Self>;
}

impl<T: Sized> WithSpan for T {
    fn with_span(self, span: Range<usize>) -> Spanned<Self> {
        Spanned::new(self, span)
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
    pub compiler_tags: &'allocator [CompilerTag<'input>],
    pub statement: Statement<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct CompilerTag<'input> {
    pub literal: Literal<'input>,
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
}

#[derive(Debug)]
pub struct DefineWithAttribute<'input, 'allocator> {
    pub attribute: &'allocator [Spanned<StatementAttribute>],
    pub define: Result<Define<'input, 'allocator>, ()>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum Define<'input, 'allocator> {
    Function(FunctionDefine<'input, 'allocator>),
    UserType(UserTypeDefine<'input, 'allocator>),
    Variable(VariableDefine<'input, 'allocator>),
    TypeAlias(TypeAlias<'input, 'allocator>),
}

#[derive(Debug)]
pub struct FunctionDefine<'input, 'allocator> {
    pub function: Range<usize>,
    pub generics: Option<GenericsDefine<'input, 'allocator>>,
    pub name: Result<Literal<'input>, ()>,
    pub arguments: Result<FunctionArguments<'input, 'allocator>, ()>,
    pub return_type: Option<TypeTag<'input, 'allocator>>,
    pub where_clause: Option<WhereClause<'input, 'allocator>>,
    pub block: Option<Block<'input, 'allocator>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct FunctionArguments<'input, 'allocator> {
    pub this_mutability: Option<ThisMutability>,
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
    Binding {
        bindings: &'allocator [VariableBinding<'input, 'allocator>],
        span: Range<usize>,
    },
}

impl VariableBinding<'_, '_> {
    pub fn span(&self) -> Range<usize> {
        match self {
            VariableBinding::Literal(literal) => literal.span.clone(),
            VariableBinding::Binding { bindings: _, span } => span.clone(),
        }
    }
}

#[derive(Debug)]
pub struct ThisMutability {
    pub is_mutable: Spanned<bool>,
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
    pub kind: Spanned<UserTypeKind>,
    pub name: Result<Literal<'input>, ()>,
    pub generics: Option<GenericsDefine<'input, 'allocator>>,
    pub super_type: Option<SuperTypeInfo<'input, 'allocator>>,
    pub where_clause: Option<WhereClause<'input, 'allocator>>,
    pub block: Result<Block<'input, 'allocator>, ()>,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UserTypeKind {
    Class,
    Struct,
    Interface,
}

#[derive(Debug)]
pub struct SuperTypeInfo<'input, 'allocator> {
    pub types: &'allocator [TypeInfo<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct Implements<'input, 'allocator> {
    pub implements: Range<usize>,
    pub generics: Option<GenericsDefine<'input, 'allocator>>,
    pub interface: Result<TypeInfo<'input, 'allocator>, ()>,
    pub concrete: Result<TypeInfo<'input, 'allocator>, ()>,
    pub where_clause: Option<WhereClause<'input, 'allocator>>,
    pub block: Result<Block<'input, 'allocator>, ()>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct TypeAlias<'input, 'allocator> {
    pub type_keyword: Range<usize>,
    pub name: Result<Literal<'input>, ()>,
    pub generics: Option<GenericsDefine<'input, 'allocator>>,
    pub alias_type: Result<TypeInfo<'input, 'allocator>, ()>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct ImportStatement<'input, 'allocator> {
    pub import: Range<usize>,
    pub path: &'allocator [Literal<'input>],
    pub elements_or_wild_card: Option<ElementsOrWildCard<'input, 'allocator>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum ElementsOrWildCard<'input, 'allocator> {
    Elements(&'allocator [Literal<'input>]),
    WildCard(Literal<'input>),
}

#[derive(Debug)]
pub struct DropStatement<'input, 'allocator> {
    pub drop: Range<usize>,
    pub acyclic: Option<Range<usize>>,
    pub expression: Result<Expression<'input, 'allocator>, ()>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct VariableDefine<'input, 'allocator> {
    pub let_var: Spanned<LetVar>,
    pub binding: Result<VariableBinding<'input, 'allocator>, ()>,
    pub type_tag: Option<TypeTag<'input, 'allocator>>,
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
    pub right: Result<Expression<'input, 'allocator>, ()>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct SwapStatement<'input, 'allocator> {
    pub left: Expression<'input, 'allocator>,
    pub right: Result<Expression<'input, 'allocator>, ()>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum Expression<'input, 'allocator> {
    Return(&'allocator ReturnExpression<'input, 'allocator>),
    Closure(&'allocator Closure<'input, 'allocator>),
    Or(&'allocator OrExpression<'input, 'allocator>),
}

impl Expression<'_, '_> {
    pub fn span(&self) -> Range<usize> {
        match self {
            Expression::Return(return_expression) => return_expression.span.clone(),
            Expression::Closure(closure) => closure.span.clone(),
            Expression::Or(or_expression) => or_expression.span.clone(),
        }
    }
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
        expressions: &'allocator [Expression<'input, 'allocator>],
        span: Range<usize>,
    },
    Literal(Literal<'input>),
    StringLiteral(Literal<'input>),
    NumericLiteral(Literal<'input>),
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
    pub block: Result<Block<'input, 'allocator>, ()>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct Closure<'input, 'allocator> {
    pub arguments: ClosureArgumentsOrLiteral<'input, 'allocator>,
    pub expression_or_block: Result<ExpressionOrBlock<'input, 'allocator>, ()>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum ExpressionOrBlock<'input, 'allocator> {
    Expression(Expression<'input, 'allocator>),
    Block(Block<'input, 'allocator>),
}

#[derive(Debug)]
pub enum ClosureArgumentsOrLiteral<'input, 'allocator> {
    ClosureArguments(ClosureArguments<'input, 'allocator>),
    Literal(Literal<'input>),
}

#[derive(Debug)]
pub struct ClosureArguments<'input, 'allocator> {
    pub arguments: &'allocator [FunctionArgumentOrVariableBinding<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum FunctionArgumentOrVariableBinding<'input, 'allocator> {
    FunctionArgument(FunctionArgument<'input, 'allocator>),
    VariableBinding(VariableBinding<'input, 'allocator>),
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
pub struct TypeInfo<'input, 'allocator> {
    pub base: TypeInfoBase<'input, 'allocator>,
    pub attributes: &'allocator [TypeAttribute<'input, 'allocator>],
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum TypeInfoBase<'input, 'allocator> {
    Array(ArrayTypeInfo<'input, 'allocator>),
    Base(BaseTypeInfo<'input, 'allocator>),
    Tuple(TupleTypeInfo<'input, 'allocator>),
    This(Literal<'input>),
}

#[derive(Debug)]
pub struct ArrayTypeInfo<'input, 'allocator> {
    pub base_type: Result<&'allocator TypeInfo<'input, 'allocator>, ()>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct BaseTypeInfo<'input, 'allocator> {
    pub path: &'allocator [Literal<'input>],
    pub generics: Option<GenericsInfo<'input, 'allocator>>,
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
        generics: Option<GenericsInfo<'input, 'allocator>>,
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
    pub type_info: Result<TypeInfo<'input, 'allocator>, ()>,
    pub span: Range<usize>,
}
