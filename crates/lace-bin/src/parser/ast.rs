use crate::ast_metadata::Metadata;
use derive_more::Display;

pub type Expr<'s, 'a> = Metadata<ExprKind<'s, 'a>>;

/// Expressions
#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind<'s, 'a> {
    Lit(Lit<'s>),
    If(
        &'a Metadata<Self>,
        &'a Metadata<Block<'s, 'a>>,
        Option<&'a Metadata<Block<'s, 'a>>>,
    ),
    Block(&'a Metadata<Block<'s, 'a>>),
    Ident(Metadata<Ident<'s>>),
    Paren(&'a Metadata<Self>),
    BinaryOp(Metadata<BinaryOp>, &'a Metadata<Self>, &'a Metadata<Self>),
    UnaryOp(Metadata<UnaryOp>, &'a Metadata<Self>),
    Tuple(&'a [Metadata<Self>]),
    Call(&'a Metadata<Self>, Metadata<&'a [Metadata<Self>]>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Display)]
pub enum BinaryOp {
    #[display(fmt = "*")]
    Mul,
    #[display(fmt = "/")]
    Div,
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "-")]
    Sub,
    #[display(fmt = "&&")]
    LogicalAnd,
    #[display(fmt = "||")]
    LogicalOr,
    #[display(fmt = ">")]
    Gt,
    #[display(fmt = ">=")]
    Gte,
    #[display(fmt = "<")]
    Lt,
    #[display(fmt = "<=")]
    Lte,
    #[display(fmt = "==")]
    Eq,
    #[display(fmt = "!=")]
    Neq,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Display)]
pub enum UnaryOp {
    #[display(fmt = "!")]
    Not,
    #[display(fmt = "-")]
    Neg,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Ident<'s> {
    pub string: &'s str,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeSpec<'s> {
    Name(Metadata<Ident<'s>>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Parameter<'s> {
    pub name: Metadata<Ident<'s>>,
    pub type_spec: Metadata<TypeSpec<'s>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Function<'s, 'a> {
    pub name: Metadata<Ident<'s>>,

    pub parameters: &'a [Parameter<'s>],
    pub ret: Option<Metadata<TypeSpec<'s>>>,

    pub block: &'a Metadata<Block<'s, 'a>>,
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
        Metadata<Ident<'s>>,
        Option<Metadata<TypeSpec<'s>>>,
        &'a Expr<'s, 'a>,
    ),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Item<'s, 'a> {
    Function(Metadata<Function<'s, 'a>>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct File<'s, 'a> {
    pub items: &'a [Item<'s, 'a>],
}

pub type Ast<'s, 'a> = File<'s, 'a>;
