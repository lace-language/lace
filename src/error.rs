use crate::lexer::error::LexError;
use crate::parser::error::ParseError;
use crate::source_file::SourceFile;
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
