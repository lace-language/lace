use crate::ast_metadata::Metadata;
use crate::parser::ast::Function;
use crate::parser::ast::Item;
use crate::typechecking::ctx::TypeContext;
use crate::typechecking::ty::PartialType;
use crate::typechecking::type_spec_to_partial_type;
use bumpalo::collections::Vec as BumpVec;

pub fn find_statics_function<'a>(f: &Metadata<Function>, ctx: &mut TypeContext<'a, '_, '_>) {
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
        if let Err(_) = ctx.unify(ty, tyv) {
            lice!("should never fail because this is the first usage of this type variable, as we're declaring the parameter it describes here.");
        }

        params.push(tyv.into())
    }

    let func_ty = PartialType::Function {
        params: params.into_bump_slice(),
        ret: ctx.alloc(return_type),
    };
    let ty_var = ctx.type_variable_for_identifier(&f.value.name);

    if let Err(_) = ctx.unify(ty_var, func_ty) {
        lice!("should never fail because this is the first usage of this type variable, as we're declaring the function it describes here.");
    }
}

pub fn find_statics_item<'a>(item: &Item, ctx: &mut TypeContext<'a, '_, '_>) {
    match item {
        Item::Function(f) => find_statics_function(f, ctx),
    }
}
