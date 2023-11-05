use crate::name_resolution::ResolvedNames;
use crate::parser::ast::{Ast, TypeSpec};
use crate::parser::span::Spans;
use crate::typechecking::ctx::TypeContext;
use crate::typechecking::error::TypeError;
use crate::typechecking::solved::SolvedTypes;
use crate::typechecking::ty::{PartialType, TypeVariableGenerator};
use bumpalo::Bump;

pub mod check_pass;
pub mod ctx;
pub mod error;
pub mod solved;
pub mod static_pass;
pub mod ty;

fn type_spec_to_partial_type<'a>(spec: &TypeSpec) -> PartialType<'a> {
    match spec {
        TypeSpec::Name(m) if m.value.string == "int" => PartialType::Int,
        TypeSpec::Name(m) if m.value.string == "string" => PartialType::String,
        TypeSpec::Name(m) if m.value.string == "bool" => PartialType::Bool,
        TypeSpec::Name(m) => unimplemented!("{m:?}"),
    }
}

pub fn typecheck<'ast, 'types, 'names>(
    ast: &Ast<'_, 'ast>,
    resolved_names: &'names ResolvedNames,
    spans: &'names Spans,
    arena: &'types Bump,
) -> Result<SolvedTypes<'types, 'names>, Vec<TypeError>> {
    let mut errs = Vec::new();

    let mut variable_generator = TypeVariableGenerator::new();
    let mut ctx = TypeContext::new(resolved_names, arena, spans, &mut variable_generator);
    static_pass::find_statics_ast(ast, &mut ctx, &mut errs);

    if !errs.is_empty() {
        return Err(errs);
    }

    let solved = check_pass::typecheck_ast(ast, resolved_names, ctx, &mut errs);
    if !errs.is_empty() {
        Err(errs)
    } else {
        Ok(solved)
    }
}
