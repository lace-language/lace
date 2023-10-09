use std::fmt::{Debug, Formatter};
use std::mem;
use miette::Diagnostic;
use thiserror::Error;
use crate::lexer::error::LexError;
use crate::parser::error::ParseError;
use crate::source_file::SourceFile;

/// Determines how many recoverable errors may occur until errors are fatal anyway.
pub const MAX_RECOVERABLE: usize = 10;

pub type CompilerResult<'s, T, E> = Result<T, CompilerError<'s, E>>;

pub trait CompilerResultExt<'s, T, E>
    where E: Into<CompilerErrorKind>
{
    fn map_into<X>(self) -> Result<T, CompilerError<'s, X>> where E: Into<X>, X: Into<CompilerErrorKind>;

    fn map_make_generic(self) -> Result<T, CompilerError<'s, CompilerErrorKind>>;
}

impl<'s, T, E> CompilerResultExt<'s, T, E> for CompilerResult<'s, T, E>
    where E: Into<CompilerErrorKind>
{
    /// Converts any `Result<_, CompilerError<A>>` to a `Result<_, CompilerError<B>>` when `A` can be converted into `B`.
    fn map_into<X>(self) -> Result<T, CompilerError<'s, X>>
        where E: Into<X>, X: Into<CompilerErrorKind>
    {
        self.map_err(|e| {
            let CompilerError(errs, fatal) = e;
            CompilerError(errs, (fatal.0.into(), fatal.1))
        })
    }

    /// Makes a result of any lower level error type into a result of [`CompilerErrorKind`](CompilerErrorKind)
    fn map_make_generic(self) -> Result<T, CompilerError<'s, CompilerErrorKind>> {
        self.map_err(|e| e.make_generic())
    }
}

pub trait ResultExt<T, E>
    where E: Into<CompilerErrorKind>
{
    /// maps any Result to a fatal error in the error context. Nice for chaining `map_err()` calls, but often less useful than [`map_err_fatal`](ErrorContext::fatal)
    fn map_err_fatal<'s>(self, ectx: &mut ErrorContext<'s>, source: SourceFile<'s>) -> Result<T, CompilerError<'s, E>>;
}

impl<T, E> ResultExt<T, E> for Result<T, E>
    where E: Into<CompilerErrorKind>
{
    fn map_err_fatal<'s>(self, ectx: &mut ErrorContext<'s>, source: SourceFile<'s>) -> Result<T, CompilerError<'s, E>>{
        match self {
            Ok(i) => Ok(i),
            Err(e) => ectx.fatal(e, source)?
        }
    }
}

/// The toplevel compiler error enum. This is what all errors finally turn in to, usually
/// with transparent wrappers to an actual Diagnostic.
#[derive(Diagnostic, Error, Debug, Clone)]
pub enum CompilerErrorKind {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Lex(#[from] LexError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    Parse(#[from] ParseError)
}

impl CompilerErrorKind {
    pub fn to_miette(&self, source: &SourceFile) -> miette::Report {
        miette::Report::new(self.clone()).with_source_code(source.named_source())
    }
}

/// The error type returned out of most functions in the compiler.
/// Contains a number of "recoverable errors" in the error context, and then one last which was fatal.
pub struct CompilerError<'s, E: Into<CompilerErrorKind>>(Vec<(CompilerErrorKind, SourceFile<'s>)>, (E, SourceFile<'s>));
impl<'s, E> CompilerError<'s, E>
    where E: Into<CompilerErrorKind>
{
    pub fn get_fatal(&self) -> &E {
        &self.1.0
    }

    pub fn make_generic(self) -> CompilerError<'s, CompilerErrorKind> {
        CompilerError(self.0, (self.1.0.into(), self.1.1))
    }
}

impl<'s, E> Debug for CompilerError<'s, E>
    where E: Into<CompilerErrorKind> + Diagnostic + Clone
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (e, source) in &self.0 {
            writeln!(f, "{:?}", e.clone().to_miette(source))?;
        }

        let (e, source) = &self.1;
        write!(f, "{:?}", e.clone().into().to_miette(source))
    }
}

/// Holds the current compiler state of recoverable errors until a fatal error is thrown.
pub struct ErrorContext<'s> {
    errors: Vec<(CompilerErrorKind, SourceFile<'s>)>
}

impl<'s> ErrorContext<'s> {
    pub fn new() -> Self {
        Self {
            errors: vec![],
        }
    }

    /// Used after compilation. Even recoverable errors are errors, not warnings.
    /// Therefore, after all compiler stages have passed, this should be called.
    /// Since most errors are fatal, this method almost never actually returns `Err()`.
    /// Either one error was fatal somewhere down the compiler line, or compilation went well.
    pub fn finish_compile_make_recoverable_fatal<T>(mut self, v: T) -> Result<T, CompilerError<'s, CompilerErrorKind>> {
        if let Some(i) = self.errors.pop() {
            // make the last error the fatal error. All other errors are just "nice to have"
            Err(CompilerError(mem::take(&mut self.errors), i))
        } else {
            Ok(v)
        }
    }

    /// Registering a fatal error always returns `Err()` immediately, but also displays all the recoverable errors
    /// registered before it.
    pub fn fatal<T, E>(&mut self, error: E, source: SourceFile<'s>) -> Result<T, CompilerError<'s, E>>
        where E: Into<CompilerErrorKind>
    {
        Err(CompilerError(mem::take(&mut self.errors), (error, source)))
    }

    /// Registering a recoverable may or may not return `Err()` based on how many errors
    /// have been registered previously. After the number of previously registered errors
    /// exceeds [MAX_RECOVERABLE](MAX_RECOVERABLE), a registered recoverable error becomes
    /// fatal.
    pub fn recoverable<E>(&mut self, error: E, source: SourceFile<'s>) -> Result<(), CompilerError<'s, E>>
        where E: Into<CompilerErrorKind>
    {
        if self.errors.len() > MAX_RECOVERABLE {
            Err(CompilerError(mem::take(&mut self.errors), (error, source)))
        } else {
            self.errors.push((error.into(), source));
            Ok(())
        }
    }
}