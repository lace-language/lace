use crate::syntax_id::Identified;
use derive_more::Display;

pub type Expr<'s, 'a> = Identified<ExprKind<'s, 'a>>;

/// Expressions
#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind<'s, 'a> {
    Lit(Lit<'s>),
    If(
        &'a Identified<Self>,
        &'a Identified<Block<'s, 'a>>,
        Option<&'a Identified<Block<'s, 'a>>>,
    ),
    Block(&'a Identified<Block<'s, 'a>>),
    Ident(Identified<Ident<'s>>),
    Paren(&'a Identified<Self>),
    BinaryOp(
        Identified<BinaryOp>,
        &'a Identified<Self>,
        &'a Identified<Self>,
    ),
    UnaryOp(Identified<UnaryOp>, &'a Identified<Self>),
    Tuple(&'a [Identified<Self>]),
    Call(&'a Identified<Self>, Identified<&'a [Identified<Self>]>),
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
    Name(Identified<Ident<'s>>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Parameter<'s> {
    pub name: Identified<Ident<'s>>,
    pub type_spec: Identified<TypeSpec<'s>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Function<'s, 'a> {
    pub name: Identified<Ident<'s>>,

    pub parameters: &'a [Parameter<'s>],
    pub ret: Option<Identified<TypeSpec<'s>>>,

    pub block: &'a Identified<Block<'s, 'a>>,
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
        Identified<Ident<'s>>,
        Option<Identified<TypeSpec<'s>>>,
        &'a Expr<'s, 'a>,
    ),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Item<'s, 'a> {
    Function(Identified<Function<'s, 'a>>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct File<'s, 'a> {
    pub items: &'a [Item<'s, 'a>],
}

pub type Ast<'s, 'a> = File<'s, 'a>;
