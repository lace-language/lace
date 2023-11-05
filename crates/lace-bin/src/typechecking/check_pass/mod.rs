use crate::name_resolution::ResolvedNames;
use crate::parser::ast::Ast;
use crate::typechecking::ctx::{TypeContext, Types};
use crate::typechecking::error::TypeError;
use crate::typechecking::solved::SolvedTypes;
use std::collections::HashMap;

pub mod check;

pub fn typecheck_ast<'a, 'n, 't>(
    ast: &Ast,
    resolved_names: &'n ResolvedNames,
    global_ctx: TypeContext<'a, 'n, 't>,
    errs: &mut Vec<TypeError>,
) -> SolvedTypes<'a, 'n> {
    let mut global_name_mapping = HashMap::new();
    let mut global_node_types = HashMap::new();
    let mut global_types = Types::new();

    for item in ast.items {
        let mut ctx = global_ctx.clone();
        if let Err(e) = check::typecheck_item(item, &mut ctx) {
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

    SolvedTypes::new(
        global_types,
        global_node_types,
        global_name_mapping,
        resolved_names,
    )
}
