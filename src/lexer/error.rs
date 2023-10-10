use crate::error::CompilerResult;
use crate::lexer::token_buffer::BracketError;
use crate::parser::span::Span;
use miette::Diagnostic;
use thiserror::Error;

pub type LexResult<'s, T> = CompilerResult<'s, T, LexError>;

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
pub enum LexError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    BracketError(#[from] BracketError),

    // TODO: Figure out how to get the error from logos
    #[error("Unrecognized token")]
    UnrecognizedToken {
        #[label("unrecognized token")]
        span: Span,
    },
}
