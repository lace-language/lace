use crate::typechecking::solved::ResolveTypeError;
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic, Clone, PartialEq)]
pub enum FunctionLoweringError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Resolve(#[from] ResolveTypeError),
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq)]
pub enum LoweringError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Function(#[from] FunctionLoweringError),
}
