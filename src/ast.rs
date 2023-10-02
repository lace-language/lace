/// Expressions
#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'source, 'arena> {
    Lit(Lit<'source>),
    Fn,
    Block(&'arena Block<'source, 'arena>),
    Ident(Ident<'source>),
    UnaryMinus(&'arena Self),
    Mul(&'arena Self, &'arena Self),
    Div(&'arena Self, &'arena Self),
    Add(&'arena Self, &'arena Self),
    Sub(&'arena Self, &'arena Self),
    UnaryNot(&'arena Self),
    LogicalAnd(&'arena Self, &'arena Self),
    LogicalOr(&'arena Self, &'arena Self),
    Tuple(&'arena [Self]),
    GreaterThan(&'arena Self, &'arena Self),
    GreaterEquals(&'arena Self, &'arena Self),
    LessThan(&'arena Self, &'arena Self),
    LessEquals(&'arena Self, &'arena Self),
    Equals(&'arena Self, &'arena Self),
    NotEquals(&'arena Self, &'arena Self),
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
    Int(u64),
    String(&'source str),
}

/// Statements
#[derive(Debug, PartialEq, Eq)]
pub enum Statement<'source, 'arena> {
    Expr(&'arena Expr<'source, 'arena>),
    Let(Ident<'source>, &'arena Expr<'source, 'arena>),
}
