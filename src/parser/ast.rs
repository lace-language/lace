use crate::parser::span::Spanned;

pub type Expr<'s, 'a> = Spanned<ExprKind<'s, 'a>>;

/// Expressions
#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind<'s, 'a> {
    Lit(Lit<'s>),
    If(
        &'a Spanned<Self>,
        &'a Spanned<Block<'s, 'a>>,
        Option<&'a Spanned<Block<'s, 'a>>>,
    ),
    Block(&'a Spanned<Block<'s, 'a>>),
    Ident(Ident<'s>),
    Paren(&'a Spanned<Self>),
    BinOp(BinOp, &'a Spanned<Self>, &'a Spanned<Self>),
    UnaryOp(UnaryOp, &'a Spanned<Self>),
    Tuple(&'a [Spanned<Self>]),
    Call(&'a Spanned<Self>, Spanned<&'a [Spanned<Self>]>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinOp {
    Mul,
    Div,
    Add,
    Sub,
    LogicalAnd,
    LogicalOr,
    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    Neq,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Ident<'s> {
    pub string: &'s str,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeSpec<'s> {
    Name(Spanned<Ident<'s>>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Parameter<'s> {
    pub name: Spanned<Ident<'s>>,
    pub type_spec: Spanned<TypeSpec<'s>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Function<'s, 'a> {
    pub name: Spanned<Ident<'s>>,

    pub parameters: &'a [Parameter<'s>],
    pub ret: Option<Spanned<TypeSpec<'s>>>,

    pub block: &'a Spanned<Block<'s, 'a>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block<'s, 'a> {
    pub stmts: &'a [Statement<'s, 'a>],
    pub last: Option<Expr<'s, 'a>>,
}

/// Literal values
#[derive(Debug, PartialEq, Eq)]
pub enum Lit<'s> {
    Bool(bool),
    Int(&'s str),
    String(&'s str),
}

/// Statements
#[derive(Debug, PartialEq, Eq)]
pub enum Statement<'s, 'a> {
    Expr(&'a Expr<'s, 'a>),
    Let(
        Spanned<Ident<'s>>,
        Option<Spanned<TypeSpec<'s>>>,
        &'a Expr<'s, 'a>,
    ),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Item<'s, 'a> {
    Function(Spanned<Function<'s, 'a>>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct File<'s, 'a> {
    pub items: &'a [Item<'s, 'a>],
}

pub type Ast<'s, 'a> = File<'s, 'a>;
