use crate::typechecking::context::TypeContext;
use crate::typechecking::ty::TypeOrVariable;

pub trait TypeConstraintGenerator<'a> {
    type TypeResult;

    fn generate_constraints(&self, ctx: &mut TypeContext<'a>) -> Self::TypeResult;
}

pub enum Constraint<'a> {
    Equal(TypeOrVariable<'a>, TypeOrVariable<'a>)
}