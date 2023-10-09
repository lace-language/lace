use miette::Diagnostic;
use thiserror::Error;
use crate::error::CompilerError;

use super::span::Span;

pub type ParseResult<'s, T> = Result<T, CompilerError<'s, ParseError>>;

#[derive(Error, Diagnostic, Debug, Clone)]
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
}
