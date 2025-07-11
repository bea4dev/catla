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
    pub statements: Vec<StatementWithTagAndDocs<'input, 'allocator>, &'allocator Bump>,
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
    pub documents: Vec<&'input str, &'allocator Bump>,
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
    pub attribute: Vec<Spanned<StatementAttribute>, &'allocator Bump>,
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
    pub arguments: Vec<FunctionArgument<'input, 'allocator>, &'allocator Bump>,
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
    pub elements: Vec<WhereElement<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct WhereElement<'input, 'allocator> {
    pub target_type: TypeInfo<'input, 'allocator>,
    pub bounds: Vec<TypeInfo<'input, 'allocator>, &'allocator Bump>,
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
    pub types: Vec<TypeInfo<'input, 'allocator>, &'allocator Bump>,
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
    pub path: Vec<Literal<'input>, &'allocator Bump>,
    pub elements: Vec<Literal<'input>, &'allocator Bump>,
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
pub enum Expression<'input, 'allocator> {}

#[derive(Debug)]
pub struct GenericsDefine<'input, 'allocator> {
    pub elements: Vec<GenericsElement<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct GenericsElement<'input, 'allocator> {
    pub name: Literal<'input>,
    pub bounds: Vec<TypeInfo<'input, 'allocator>, &'allocator Bump>,
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
    pub path: Vec<Literal<'input>, &'allocator Bump>,
    pub generics: GenericsInfo<'input, 'allocator>,
    pub attribute: TypeAttribute<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct GenericsInfo<'input, 'allocator> {
    pub types: Vec<TypeInfo<'input, 'allocator>, &'allocator Bump>,
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
    pub types: Vec<TypeInfo<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct TypeTag<'input, 'allocator> {
    pub type_info: TypeInfo<'input, 'allocator>,
    pub span: Range<usize>,
}
