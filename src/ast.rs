#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StatementId(pub usize);

/// Binary Operations
// TODO: Describe the symbol or the semantics?
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Plus,
    Minus,
    Slash,
    Star,
}

/// Expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Lit(Lit),
    BinOp {
        op: BinOp,
        left: ExprId,
        right: ExprId,
    },
    Fn,
    Block(Vec<Statement>),
    Ident(String),
}

/// Literal values
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lit {
    Bool(bool),
    Int(i64),
    String(String),
}

/// Statements
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Expr(Expr),
    Let,
}
