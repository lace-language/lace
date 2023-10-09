use miette::Diagnostic;
use thiserror::Error;
use crate::parser::span::Span;

pub type PreparseResult<T> = Result<T, PreparseError>;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic()]
pub enum PreparseError {

    // TODO: Figure out how to get the error from logos
    #[error("Unrecognized token")]
    UnrecognizedToken {
        #[label("unrecognized token")]
        span: Span,
    },
}