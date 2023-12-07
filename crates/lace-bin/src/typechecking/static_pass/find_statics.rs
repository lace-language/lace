use crate::ast_metadata::Metadata;
use crate::parser::ast::Function;
use crate::parser::ast::Item;
use crate::typechecking::ctx::TypeContext;
use crate::typechecking::ty::PartialType;
use crate::typechecking::type_spec_to_partial_type;
use bumpalo::collections::Vec as BumpVec;

pub fn find_statics_function(f: &Metadata<Function>, ctx: &mut TypeContext) {
    let return_type = f
        .value
        .ret
        .as_ref()
        .map(|i| type_spec_to_partial_type(&i.value))
        .unwrap_or(PartialType::Unit);

    let mut params = BumpVec::new_in(ctx.arena);
    for i in f.value.parameters {
        let ty = type_spec_to_partial_type(&i.type_spec.value);
        let tyv = ctx.type_variable_for_identifier(&i.name);
        if ctx.unify(ty, tyv).is_err() {
            lice!("should never fail because this is the first usage of this type variable, as we're declaring the parameter it describes here.");
        }

        params.push(tyv.into())
    }

    let func_ty = PartialType::Function {
        params: params.into_bump_slice(),
        ret: ctx.alloc(return_type),
        function_name: Some(ctx.fresh_function_name()),
    };
    let ty_var = ctx.type_variable_for_identifier(&f.value.name);

    ctx.store_type_info_for_node(f, func_ty);

    if ctx.unify(ty_var, func_ty).is_err() {
        lice!("should never fail because this is the first usage of this type variable, as we're declaring the function it describes here.");
    }
}

pub fn find_statics_item(item: &Item, ctx: &mut TypeContext) {
    match item {
        Item::Function(f) => find_statics_function(f, ctx),
    }
}
