use crate::name_resolution::NameResolutions;
use crate::parser::ast::Ast;
use crate::typechecking::constraint::TypeConstraintGenerator;
use crate::typechecking::context::TypeContext;
use bumpalo::Bump;
use crate::typechecking::error::TypeError;
use crate::typechecking::solver::{SolvedTypes, Solver};

pub mod constraint;
pub mod constraint_generation;
pub mod context;
pub mod solver;
pub mod ty;
pub mod error;

pub fn typecheck<'s, 'a>(ast: &Ast<'s, 'a>, name_resolutions: &NameResolutions, arena: &'a Bump) -> Result<SolvedTypes<'a>, TypeError> {
    let mut type_context = TypeContext::new(arena);

    ast.generate_constraints(&mut type_context);
    type_context.add_name_resolutions(name_resolutions);

    let TypeContext {
        variable_generator,
        constraints,
        name_mapping,
        type_mapping,
        ..
    } = type_context;

    let mut solver = Solver::new(variable_generator, type_mapping);
    solver.apply_constraints(constraints);

    solver.generate_type_errors()
}
