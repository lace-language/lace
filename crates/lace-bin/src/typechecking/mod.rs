use crate::name_resolution::ResolvedNames;
use crate::parser::ast::Ast;
use crate::parser::span::Spans;
use crate::source_file::SourceFile;
use crate::typechecking::constraint::TypeConstraintGenerator;
use crate::typechecking::context::TypeContext;
use crate::typechecking::error::TypeError;
use crate::typechecking::solve::SolvedTypes;
use bumpalo::Bump;

pub mod constraint;
pub mod constraint_generation;
pub mod constraint_metadata;
pub mod context;
pub mod error;
pub mod solve;
pub mod ty;

pub fn typecheck<'ast, 'types>(
    ast: &Ast<'_, 'ast>,
    name_resolutions: &ResolvedNames,
    spans: &Spans,
    source: SourceFile<'ast>,
    arena: &'types Bump,
) -> Result<SolvedTypes<'types>, Vec<TypeError>> {
    let mut type_context = TypeContext::new(arena, spans);

    ast.generate_constraints(&mut type_context);
    type_context.add_name_resolutions(name_resolutions);
    type_context.save_debug(spans, source);
    type_context.solve()
}
