use crate::name_resolution::ResolvedNames;
use crate::parser::ast::{BinaryOp, UnaryOp};
use crate::parser::span::{Span, Spans};
use crate::typechecking::constraint_metadata::ConstraintMetadata;
use crate::typechecking::solve::SolvedTypes;
use crate::typechecking::ty::PartialType;
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
    errors: Vec<InnerTypeError>,
}

pub struct ErrorContext<'a, 'n, 'sp> {
    pub unification_failures: Vec<(PartialType<'a>, PartialType<'a>, ConstraintMetadata<'a>)>,
    pub names: &'n ResolvedNames,
    pub spans: &'sp Spans,
    pub solved_types: SolvedTypes<'a>,
}

impl<'a, 'n, 'sp> ErrorContext<'a, 'n, 'sp> {
    pub fn unification_failure_to_type_error(
        &self,
        left: PartialType,
        right: PartialType,
        meta: ConstraintMetadata,
    ) -> InnerTypeError {
        match meta {
            ConstraintMetadata::NoConstraintMetadata => lice!("no constrain metadata"),
            ConstraintMetadata::BinaryOp(left_id, right_id, op) => {
                // let left_related = self.name_mapping.get(&left)
                //     .map(|i| self.)

                InnerTypeError::BinaryOp {
                    op,
                    left_ty: left.to_string(),
                    right_ty: right.to_string(),
                    // left_related,
                    // right_related,
                    left: self.spans.get(left_id),
                    right: self.spans.get(right_id),
                }
            }
            ConstraintMetadata::UnaryOp(expr, op) => InnerTypeError::UnaryOp {
                op,
                ty: left.to_string(),
                expr: self.spans.get(expr),
            },
            ConstraintMetadata::BlockCondition(_) => todo!(),
            ConstraintMetadata::IfReturn(_, _) => todo!(),
            ConstraintMetadata::Call { .. } => todo!(),
            ConstraintMetadata::TypeSpec { .. } => todo!(),
            ConstraintMetadata::Assignment { .. } => todo!(),
            ConstraintMetadata::FunctionDefinition { .. } => todo!(),
            ConstraintMetadata::FunctionReturn { .. } => todo!(),
            ConstraintMetadata::NameRef => todo!(),
            ConstraintMetadata::TupleUnify { .. } => todo!(),
            ConstraintMetadata::FunctionParamUnify { .. } => todo!(),
            ConstraintMetadata::FunctionReturnUnify { .. } => todo!(),
            ConstraintMetadata::ParamLength => {
                todo!()
                // self.errors.push(TypeError::ParameterLength {
                //     op,
                //     ty: a.to_string(),
                //     expr: self.spans.get(expr),
                // })
            }
            ConstraintMetadata::TupleLength => todo!(),
        }
    }

    pub fn into_type_error(mut self) -> TypeError {
        let mut errors = Vec::new();

        while let Some(i) = self.search_for_type_error() {
            errors.push(i);
        }

        assert!(
            self.unification_failures.is_empty(),
            "some unification failures did not turn into type errors"
        );

        TypeError { errors: vec![] }
    }
    fn search_for_type_error(&mut self) -> Option<InnerTypeError> {
        let (left, right, meta) = self.unification_failures.pop()?;

        Some(self.unification_failure_to_type_error(left, right, meta))
    }
}
