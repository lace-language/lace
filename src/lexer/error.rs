use crate::parser::span::Span;
use miette::Diagnostic;
use thiserror::Error;

pub type LexResult<T> = Result<T, LexError>;

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
pub enum LexError {
    #[error("Unrecognized token")]
    UnrecognizedToken {
        #[label("unrecognized token")]
        span: Span,
    },
}
