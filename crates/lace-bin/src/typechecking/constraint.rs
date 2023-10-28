use crate::typechecking::context::TypeContext;
use crate::typechecking::ty::TypeVariable;

pub trait TypeConstraintGenerator<'a> {
    type TypeResult;

    fn generate_constraints(&self, ctx: &mut TypeContext<'a>) -> Self::TypeResult;
}

pub enum Constraint {
    Equal(TypeVariable, TypeVariable),
}
