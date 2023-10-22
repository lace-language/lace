use miette::Diagnostic;
use thiserror::Error;

use super::span::Span;

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
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
    #[error(
        "The chained binary operators `{left_operator}` and `{right_operator}` are incompatible"
    )]
    IncompatibleBinaryOp {
        left_operator: String,
        right_operator: String,
        #[label("the first operator")]
        left_operator_span: Span,
        #[label("the second operator")]
        right_operator_span: Span,
    },
}
