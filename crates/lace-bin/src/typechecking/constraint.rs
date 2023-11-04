use crate::typechecking::constraint_metadata::ConstraintMetadata;
use crate::typechecking::context::TypeContext;
use crate::typechecking::ty::TypeOrVariable;

pub trait TypeConstraintGenerator<'a, 'sp> {
    type TypeResult;

    fn generate_constraints(&self, ctx: &mut TypeContext<'a, 'sp>) -> Self::TypeResult;
}

#[derive(Debug)]
pub enum Constraint<'a> {
    Equal(TypeOrVariable<'a>, TypeOrVariable<'a>),
}
