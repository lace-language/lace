use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic, Clone, PartialEq)]
pub enum LoweringError {}
