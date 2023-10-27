use bumpalo::Bump;
use crate::parser::ast::Ast;
use crate::typechecking::constraint::TypeConstraintGenerator;
use crate::typechecking::context::TypeContext;

pub mod constraint;
pub mod ty;
pub mod constraint_generation;
pub mod context;

pub fn typecheck<'s, 'a>(ast: &Ast<'s, 'a>, arena: &'a Bump) {
    let mut type_context = TypeContext::new(arena);

    ast.generate_constraints(&mut type_context);

    type_context.add_name_resolution_result();
}