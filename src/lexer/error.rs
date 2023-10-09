use miette::Diagnostic;
use thiserror::Error;
use crate::error::CompilerResult;
use crate::parser::span::Span;

pub type LexResult<'s, T> = CompilerResult<'s, T, LexError>;

#[derive(Error, Diagnostic, Debug, Clone)]
#[diagnostic()]
pub enum LexError {

    // TODO: Figure out how to get the error from logos
    #[error("Unrecognized token")]
    UnrecognizedToken {
        #[label("unrecognized token")]
        span: Span,
    },
}