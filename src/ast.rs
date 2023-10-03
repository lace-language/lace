/// Expressions
#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'source, 'arena> {
    Lit(Lit<'source>),
    If(
        &'arena Self,
        &'arena Block<'source, 'arena>,
        Option<&'arena Block<'source, 'arena>>,
    ),
    Block(&'arena Block<'source, 'arena>),
    Ident(Ident<'source>),
    Neg(&'arena Self),
    Mul(&'arena Self, &'arena Self),
    Div(&'arena Self, &'arena Self),
    Add(&'arena Self, &'arena Self),
    Sub(&'arena Self, &'arena Self),
    Not(&'arena Self),
    LogicalAnd(&'arena Self, &'arena Self),
    LogicalOr(&'arena Self, &'arena Self),
    Tuple(&'arena [Self]),
    Gt(&'arena Self, &'arena Self),
    Gte(&'arena Self, &'arena Self),
    Lt(&'arena Self, &'arena Self),
    Lte(&'arena Self, &'arena Self),
    Eq(&'arena Self, &'arena Self),
    Neq(&'arena Self, &'arena Self),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Ident<'source> {
    pub string: &'source str,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block<'source, 'arena> {
    pub stmts: &'arena [Statement<'source, 'arena>],
    pub last: Option<Expr<'source, 'arena>>,
}

/// Literal values
#[derive(Debug, PartialEq, Eq)]
pub enum Lit<'source> {
    Bool(bool),
    Int(&'source str),
    String(&'source str),
}

/// Statements
#[derive(Debug, PartialEq, Eq)]
pub enum Statement<'source, 'arena> {
    Expr(&'arena Expr<'source, 'arena>),
    Let(Ident<'source>, &'arena Expr<'source, 'arena>),
}
