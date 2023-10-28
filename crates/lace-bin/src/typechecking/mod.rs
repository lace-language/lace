use crate::name_resolution::NameResolutions;
use crate::parser::ast::Ast;
use crate::parser::span::Spans;
use crate::source_file::SourceFile;
use crate::typechecking::constraint::TypeConstraintGenerator;
use crate::typechecking::context::TypeContext;
use crate::typechecking::error::TypeError;
use crate::typechecking::solver::{SolvedTypes, Solver};
use bumpalo::Bump;

pub mod constraint;
pub mod constraint_generation;
pub mod context;
pub mod error;
pub mod solver;
pub mod ty;

pub fn typecheck<'a, 'newa>(
    ast: &Ast<'_, 'a>,
    name_resolutions: &NameResolutions,
    spans: &Spans,
    source: SourceFile<'a>,
    arena: &'newa Bump,
) -> Result<SolvedTypes<'newa>, TypeError> {
    let mut type_context = TypeContext::new(arena);

    ast.generate_constraints(&mut type_context);
    type_context.add_name_resolutions(name_resolutions);
    type_context.save_debug(spans, source);

    let TypeContext {
        variable_generator,
        constraints,
        name_mapping,
        type_mapping,
        ..
    } = type_context;

    let solver = Solver::new(variable_generator);
    solver.apply_constraints(constraints, type_mapping, name_mapping)
}
