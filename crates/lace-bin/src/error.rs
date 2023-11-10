use crate::lexer::error::LexError;
use crate::lowering::error::LoweringError;
use crate::parser::error::ParseError;
use crate::source_file::SourceFile;
use crate::typechecking::error::TypeError;
use crate::typechecking::solved::ResolveTypeError;
use derive_more::From;
use miette::{Diagnostic, Report};
use thiserror::Error;

/// The toplevel compiler error enum. This is what all errors finally turn in to, usually
/// with transparent wrappers to an actual Diagnostic.
#[derive(Diagnostic, Error, Debug, Clone, PartialEq)]
pub enum CompilerError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Lex(#[from] LexError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    Parse(#[from] ParseError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    Type(#[from] TypeErrors),

    #[error(transparent)]
    #[diagnostic(transparent)]
    SingleType(#[from] TypeError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    ResolveType(#[from] ResolveTypeError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    Lowering(#[from] LoweringError),
}

#[derive(Debug, Error, Diagnostic, From, Clone, PartialEq)]
#[error("type errors")]
pub struct TypeErrors {
    #[related]
    errors: Vec<TypeError>,
}

pub trait ResultExt<T> {
    fn map_err_miette(self, source: SourceFile) -> Result<T, Report>
    where
        Self: Sized;
}

impl<T, E> ResultExt<T> for Result<T, E>
where
    E: Diagnostic + Clone + Send + Sync + 'static,
{
    fn map_err_miette(self, source: SourceFile) -> Result<T, Report> {
        self.map_err(|e| Report::new(e).with_source_code(source.named_source()))
    }
}
