use crate::typechecking::TypeContext;

pub trait TypeConstraintGenerator {
    fn generate_constraints(&self, ctx: &mut TypeContext);
}

pub enum Constraint {
    Equal()
}