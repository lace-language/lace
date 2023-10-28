use crate::name_resolution::NameResolutions;
use crate::parser::ast::Ast;
use crate::typechecking::constraint::TypeConstraintGenerator;
use crate::typechecking::context::TypeContext;
use bumpalo::Bump;
use crate::parser::span::Spans;
use crate::source_file::SourceFile;
use crate::typechecking::error::TypeError;
use crate::typechecking::solver::{SolvedTypes, Solver};

pub mod constraint;
pub mod constraint_generation;
pub mod context;
pub mod solver;
pub mod ty;
pub mod error;

pub fn typecheck<'s, 'a, 'newa>(
    ast: &Ast<'s, 'a>,
    name_resolutions: &NameResolutions,
    spans: &Spans,
    source: SourceFile<'a>,
    arena: &'newa Bump
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

    let mut solver = Solver::new(variable_generator, type_mapping);
    solver.apply_constraints(constraints);

    solver.generate_type_errors()
}