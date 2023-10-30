use crate::typechecking::context::TypeContext;
use crate::typechecking::ty::TypeVariable;

pub trait TypeConstraintGenerator<'a, 'sp> {
    type TypeResult;

    fn generate_constraints(&self, ctx: &mut TypeContext<'a, 'sp>) -> Self::TypeResult;
}

pub enum Constraint {
    Equal(TypeVariable, TypeVariable),
}
