use crate::ice::Ice;
use crate::name_resolution::NameResolutions;
use crate::parser::ast::Ast;
use crate::typechecking::constraint::TypeConstraintGenerator;
use crate::typechecking::context::TypeContext;
use bumpalo::Bump;

pub mod constraint;
pub mod constraint_generation;
pub mod context;
pub mod solve;
pub mod ty;
pub mod unify;

fn generate_constraints<'s, 'a>(ast: &Ast<'s, 'a>, name_resolutions: &NameResolutions, arena: &'a Bump) -> TypeContext<'a> {
    let mut type_context = TypeContext::new(arena);

    ast.generate_constraints(&mut type_context);
    type_context.add_name_resolutions(name_resolutions);

    type_context
}

pub fn typecheck<'s, 'a>(ast: &Ast<'s, 'a>, name_resolutions: &NameResolutions, arena: &'a Bump) {
    let TypeContext {
        variable_generator,
        constraints,
        name_mapping,
        type_mapping,
        ..
    } = generate_constraints(ast, name_resolutions, arena);



    for (i, j) in &name_mapping {
        println!("name {i:?} has typevar {j:?}")
    }
}
