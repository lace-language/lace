use crate::lice::Lice;
use crate::name_resolution::ResolvedNames;
use crate::parser::ast::{Ast, TypeSpec};
use crate::parser::span::Spans;
use crate::source_file::SourceFile;
use crate::typechecking::ctx::{TypeContext, Types};
use crate::typechecking::error::TypeError;
use crate::typechecking::solved::SolvedTypes;
use crate::typechecking::ty::PartialType;
use bumpalo::Bump;
use itertools::Itertools;
use std::collections::HashMap;

pub mod ctx;
pub mod error;
pub mod implementation;
pub mod solved;
pub mod ty;

fn type_spec_to_partial_type<'a>(
    spec: &TypeSpec,
    _ctx: &mut TypeContext<'a, '_>,
) -> PartialType<'a> {
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
    spans: &Spans,
    source: SourceFile<'ast>,
    arena: &'types Bump,
) -> Result<SolvedTypes<'types, 'names>, Vec<TypeError>> {
    let mut errs = Vec::new();
    let mut global_name_mapping = HashMap::new();
    let mut global_node_types = HashMap::new();
    let mut global_types = Types::new();

    for item in ast.items {
        let mut ctx = TypeContext::new(resolved_names, arena);
        if let Err(e) = implementation::typecheck_item(item, &mut ctx) {
            errs.push(e);
        }

        let TypeContext {
            name_mapping,
            node_types,
            types,
            ..
        } = ctx;

        global_name_mapping.extend(name_mapping);
        global_node_types.extend(node_types);
        global_types.extend(types);
    }

    if !errs.is_empty() {
        Err(errs)
    } else {
        Ok(SolvedTypes::new(
            global_types,
            global_node_types,
            global_name_mapping,
            resolved_names,
        ))
    }
}
