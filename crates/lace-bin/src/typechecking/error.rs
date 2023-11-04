use crate::parser::ast::{BinaryOp, UnaryOp};
use crate::parser::span::Span;
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Clone, Diagnostic)]
#[diagnostic()]
pub enum InnerTypeError {
    #[error("no implementation of {left_ty} {op} {right_ty}")]
    BinaryOp {
        op: BinaryOp,

        left_ty: String,
        right_ty: String,

        // #[label("note: assigned here")]
        // left_related: Option<Span>,
        //
        // #[label("note: assigned here")]
        // right_related: Option<Span>,
        #[label]
        left: Span,

        #[label]
        right: Span,
    },
    #[error("no implementation of {op} {ty}")]
    UnaryOp {
        op: UnaryOp,
        ty: String,
        #[label]
        expr: Span,
    },
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq)]
#[error("type errors")]
pub struct TypeError {
    #[related]
    pub errors: Vec<InnerTypeError>,
}
