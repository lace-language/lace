use crate::lexer::error::LexError;
use crate::parser::error::ParseError;
use crate::source_file::SourceFile;
use miette::Diagnostic;
use thiserror::Error;

pub trait ResultExt<T> {
    fn unwrap_miette(self, source: SourceFile) -> T
    where
        Self: Sized;
}

impl<T, E> ResultExt<T> for Result<T, E>
where
    E: Diagnostic + Clone + Send + Sync + 'static,
{
    fn unwrap_miette(self, source: SourceFile) -> T {
        match self {
            Ok(i) => i,
            Err(e) => {
                print_error(e, source);
                panic!();
            }
        }
    }
}

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

pub fn print_error(e: impl Diagnostic + Clone + Send + Sync + 'static, source: SourceFile) {
    let report = miette::Report::new(e).with_source_code(source.named_source());
    println!("{report:?}")
}
