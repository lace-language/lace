use crate::parser::span::{Span, Spanned};

pub type Expr<'s, 'a> = Spanned<ExprKind<'s, 'a>>;

/// Expressions
#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind<'s, 'a> {
    Lit(Lit<'s>),
    Block(&'a Block<'s, 'a>),
    Ident(Ident<'s>),
    Neg(&'a Spanned<Self>),
    Mul(&'a Spanned<Self>, &'a Spanned<Self>),
    Div(&'a Spanned<Self>, &'a Spanned<Self>),
    Add(&'a Spanned<Self>, &'a Spanned<Self>),
    Sub(&'a Spanned<Self>, &'a Spanned<Self>),
    Not(&'a Spanned<Self>),
    LogicalAnd(&'a Spanned<Self>, &'a Spanned<Self>),
    LogicalOr(&'a Spanned<Self>, &'a Spanned<Self>),
    Tuple(&'a [Spanned<Self>]),
    Gt(&'a Spanned<Self>, &'a Spanned<Self>),
    Gte(&'a Spanned<Self>, &'a Spanned<Self>),
    Lt(&'a Spanned<Self>, &'a Spanned<Self>),
    Lte(&'a Spanned<Self>, &'a Spanned<Self>),
    Eq(&'a Spanned<Self>, &'a Spanned<Self>),
    Neq(&'a Spanned<Self>, &'a Spanned<Self>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Ident<'s> {
    pub string: &'s str,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block<'s, 'a> {
    pub stmts: &'a [Statement<'s, 'a>],
    pub last: Option<Expr<'s, 'a>>,
    pub span: Span,
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
    Let(Ident<'s>, &'a Expr<'s, 'a>),
}
