use crate::parser::ast::{BinaryOp, UnaryOp};
use crate::parser::span::Span;
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Clone, Diagnostic)]
#[diagnostic()]
pub enum TypeError {
    #[error("no implementation of {left_ty} {op} {right_ty}")]
    BinaryOp {
        op: BinaryOp,

        left_ty: String,
        right_ty: String,

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

    #[error("type was never constrained")]
    Unresolved,
}
