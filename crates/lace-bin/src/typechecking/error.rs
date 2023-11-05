use crate::parser::ast::{BinaryOp, UnaryOp};
use crate::parser::span::Span;
use miette::{Diagnostic, LabeledSpan};
use thiserror::Error;

#[derive(Debug, PartialEq, Clone)]
pub struct FailedUnification {
    pub expected: String,
    pub was: String,
    pub was_span: Span,
}

pub trait ResultExt {
    fn on_failed_unification(self, f: impl FnOnce(FailedUnification) -> TypeError) -> Self;
}

/// Should be put whenever a concrete type (so [`PartialType`](crate::typechecking::PartialType)) is passed down
/// into a lower level expression to own up to the error and raise it according to local context. See how this is done
/// for binary operators.
impl<T> ResultExt for Result<T, TypeError> {
    fn on_failed_unification(self, f: impl FnOnce(FailedUnification) -> TypeError) -> Self {
        match self {
            Err(TypeError::FailedUnification(e)) => Err(f(e)),
            other => other,
        }
    }
}

#[derive(Debug, Error, PartialEq, Clone)]
pub enum TypeError {
    #[error("expected {side} operand of {op} to be of type {expected_ty}")]
    BinaryOp {
        op: BinaryOp,
        value_ty: String,
        side: &'static str,

        expected_ty: String,
        value_span: Span,
        op_span: Span,
    },
    #[error("expected expression to be of type {expected_ty} because of {op} operator")]
    UnaryOp {
        op: UnaryOp,
        expected_ty: String,
        was_ty: String,
        expr: Span,
        op_span: Span,
    },

    #[error("type was never constrained")]
    Unresolved,

    /// Should be raised when unifying a concrete type with some type variable passed in fails.
    /// The place where that type variable was passed in is responsible for the error
    #[error("expected {} but got unified with {}", _0.expected, _0.was)]
    FailedUnification(FailedUnification),

    #[error("comparison `{op}` requires these types to be the same")]
    Comparison {
        op: BinaryOp,

        left_ty: String,
        right_ty: String,

        left: Span,
        right: Span,
    },

    #[error("cannot return value of type {was_ret_ty}")]
    ImplicitFunctionReturn {
        expected_ret_ty: String,
        expected_ret_ty_spec_span: Span,
        was_ret_ty: String,
        was_expr_span: Span,
    },
    #[error("expected return expression of type {expected_ret_ty}")]
    ImplicitReturn {
        expected_ret_ty: String,
        was_ret_ty: String,
        was_expr_span: Span,
    },
    #[error("expected return expression in this block which should return {expected_ret_ty}")]
    ExpectedFunctionReturn {
        expected_ret_ty: String,
        expected_ret_ty_spec_span: Span,
    },
    #[error("expected return expression in this block which should return {expected_ret_ty}")]
    ExpectedReturn { expected_ret_ty: String },

    #[error("unexpected return expression in block returning unit")]
    UnexpectedReturn {
        was_ret_ty: String,
        was_expr_span: Span,
    },
    #[error("expected the type of this if condition to be a bool")]
    IfCondition {
        condition_span: Span,
        condition_ty: String,
        if_span: Span,
    },
    #[error("this if block should not have a return expression")]
    IfWithUnexpectedReturn {
        return_span: Span,
        return_ty: String,
        if_block_span: Span,
    },
    #[error("expected the return types of if and else blocks to be equal")]
    IfElseEqual {
        if_return_span: Span,
        else_return_span: Span,
        if_ty: String,
        else_ty: String,
        if_block_span: Span,
    },
}

impl Diagnostic for TypeError {
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        match self {
            TypeError::BinaryOp {
                value_ty,
                value_span,
                op_span,
                ..
            } => Some(Box::new(
                [
                    LabeledSpan::new(
                        Some(format!("got {value_ty}")),
                        value_span.offset(),
                        value_span.length(),
                    ),
                    LabeledSpan::new(
                        Some("this operator".to_string()),
                        op_span.offset(),
                        op_span.length(),
                    ),
                ]
                .into_iter(),
            )),
            TypeError::UnaryOp {
                expected_ty,
                was_ty,
                expr,
                op_span,
                ..
            } => Some(Box::new(
                [
                    LabeledSpan::new(Some(format!("got {was_ty}")), expr.offset(), expr.length()),
                    LabeledSpan::new(
                        Some(format!("expected {expected_ty} because of this operator")),
                        op_span.offset(),
                        op_span.length(),
                    ),
                ]
                .into_iter(),
            )),
            TypeError::Unresolved => None,
            TypeError::Comparison {
                left_ty,
                right_ty,
                left,
                right,
                ..
            } => Some(Box::new(
                [
                    LabeledSpan::new(Some(left_ty.clone()), left.offset(), left.length()),
                    LabeledSpan::new(Some(right_ty.clone()), right.offset(), right.length()),
                ]
                .into_iter(),
            )),
            TypeError::FailedUnification { .. } => None,
            TypeError::ImplicitFunctionReturn {
                expected_ret_ty,
                expected_ret_ty_spec_span,
                was_ret_ty,
                was_expr_span,
            } => Some(Box::new(
                [
                    LabeledSpan::new(
                        Some(format!("expected {expected_ret_ty}")),
                        expected_ret_ty_spec_span.offset(),
                        expected_ret_ty_spec_span.length(),
                    ),
                    LabeledSpan::new(
                        Some(format!("but got {was_ret_ty}")),
                        was_expr_span.offset(),
                        was_expr_span.length(),
                    ),
                ]
                .into_iter(),
            )),
            TypeError::ImplicitReturn {
                was_ret_ty,
                was_expr_span,
                ..
            } => Some(Box::new(
                [LabeledSpan::new(
                    Some(format!("but got {was_ret_ty}")),
                    was_expr_span.offset(),
                    was_expr_span.length(),
                )]
                .into_iter(),
            )),
            TypeError::ExpectedFunctionReturn {
                expected_ret_ty_spec_span,
                ..
            } => Some(Box::new(
                [LabeledSpan::new(
                    Some(format!("because of this type specification")),
                    expected_ret_ty_spec_span.offset(),
                    expected_ret_ty_spec_span.length(),
                )]
                .into_iter(),
            )),
            TypeError::ExpectedReturn { .. } => None,
            TypeError::UnexpectedReturn {
                was_ret_ty,
                was_expr_span,
            } => Some(Box::new(
                [LabeledSpan::new(
                    Some(format!("got {was_ret_ty}")),
                    was_expr_span.offset(),
                    was_expr_span.length(),
                )]
                .into_iter(),
            )),
            TypeError::IfCondition {
                condition_span,
                condition_ty,
                ..
            } => Some(Box::new(
                [LabeledSpan::new(
                    Some(format!("has type {condition_ty}")),
                    condition_span.offset(),
                    condition_span.length(),
                )]
                .into_iter(),
            )),
            TypeError::IfWithUnexpectedReturn {
                return_span,
                return_ty,
                if_block_span: _,
            } => Some(Box::new(
                [
                    LabeledSpan::new(
                        Some(format!("returns a value here of type {return_ty}")),
                        return_span.offset(),
                        return_span.length(),
                    ),
                    // TODO: breaks miette
                    // LabeledSpan::new(
                    //     Some(format!("should not return or return unit")),
                    //     if_block_span.offset(),
                    //     if_block_span.length(),
                    // ),
                ]
                .into_iter(),
            )),
            TypeError::IfElseEqual {
                if_return_span,
                else_return_span,
                if_ty,
                else_ty,
                if_block_span: _,
            } => Some(Box::new(
                [
                    LabeledSpan::new(
                        Some(format!("returns a value here of type {if_ty}")),
                        if_return_span.offset(),
                        if_return_span.length(),
                    ),
                    LabeledSpan::new(
                        Some(format!("returns a value here of type {else_ty}")),
                        else_return_span.offset(),
                        else_return_span.length(),
                    ),
                ]
                .into_iter(),
            )),
        }
    }
}
