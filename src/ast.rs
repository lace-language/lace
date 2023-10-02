/// Expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<'source, 'arena> {
    Lit(Lit<'source>),
    Fn,
    Block(&'arena [&'arena Statement<'source, 'arena>]),
    Ident(&'source str),
    UnaryMinus(&'arena Self),
    Mul(&'arena Self, &'arena Self),
    Div(&'arena Self, &'arena Self),
    Add(&'arena Self, &'arena Self),
    Sub(&'arena Self, &'arena Self),
    UnaryNot(&'arena Self),
    LogicalAnd(&'arena Self, &'arena Self),
    LogicalOr(&'arena Self, &'arena Self),
    Tuple(&'arena [&'arena Self]),
}

/// Literal values
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lit<'source> {
    Bool(bool),
    Int(u64),
    String(&'source str),
}

/// Statements
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement<'source, 'arena> {
    Expr(Expr<'source, 'arena>),
    Let,
}
