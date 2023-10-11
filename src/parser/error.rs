use miette::Diagnostic;
use thiserror::Error;

use super::span::Span;

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic()]
pub enum ParseError {
    #[error("Unexpected end of input")]
    EndOfInput,
    #[error("Expected {expected}, but found `{got}`")]
    Expected {
        expected: String,
        got: String,
        #[label("expected {expected}")]
        span: Span,
    },
    #[error("The chained binary operators `{lhs}` and `{rhs}` are incompatible")]
    IncompatBinOp {
        lhs: String,
        rhs: String,
        #[label("the first operator")]
        lhs_span: Span,
        #[label("the second operator")]
        rhs_span: Span,
    },
    // TODO: Figure out how to get the error from logos
    #[error("Unrecognized token")]
    UnrecognizedToken {
        #[label("unrecognized token")]
        span: Span,
    },
}
