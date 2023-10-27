use bumpalo::Bump;
use crate::name_resolution::NameResolutions;
use crate::parser::ast::Ast;
use crate::typechecking::constraint::TypeConstraintGenerator;
use crate::typechecking::context::TypeContext;

pub mod constraint;
pub mod ty;
pub mod constraint_generation;
pub mod context;

pub fn typecheck<'s, 'a>(
    ast: &Ast<'s, 'a>,
    name_resolutions: &NameResolutions,
    arena: &'a Bump
) {
    let mut type_context = TypeContext::new(arena);

    ast.generate_constraints(&mut type_context);
    type_context.add_name_resolutions(name_resolutions);
}