use crate::ids::IdGenerator;
use crate::name_resolution::ResolvedNames;
use crate::parser::ast::{Ast, TypeSpec};
use crate::parser::span::Spans;
use crate::typechecking::ctx::TypeContext;
use crate::typechecking::error::TypeError;
use crate::typechecking::solved::SolvedTypes;
use crate::typechecking::ty::PartialType;
use bumpalo::Bump;
use std::num::NonZeroU16;

pub mod check_pass;
pub mod ctx;
pub mod error;
pub mod solved;
pub mod static_pass;
#[cfg(test)]
pub mod test;
pub mod ty;

fn type_spec_to_partial_type<'a>(spec: &TypeSpec) -> PartialType<'a> {
    match spec {
        // TODO: proper type parsing
        TypeSpec::Name(m) if m.value.string == "int" => PartialType::Int {
            bits: NonZeroU16::new(32).unwrap(),
            signed: true,
        },
        TypeSpec::Name(m) if m.value.string == "string" => PartialType::String,
        TypeSpec::Name(m) if m.value.string == "bool" => PartialType::Bool,
        TypeSpec::Name(m) => unimplemented!("{m:?}"),
    }
}

pub fn typecheck<'types, 'names>(
    ast: &Ast<'_, '_>,
    resolved_names: &'names ResolvedNames,
    spans: &'names Spans,
    arena: &'types Bump,
) -> Result<SolvedTypes<'types, 'names>, Vec<TypeError>> {
    let mut errs = Vec::new();

    let variable_generator = IdGenerator::new();
    let function_name_generator = IdGenerator::new();
    let mut ctx = TypeContext::new(
        resolved_names,
        arena,
        spans,
        &variable_generator,
        &function_name_generator,
    );
    static_pass::find_statics_ast(ast, &mut ctx);

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
