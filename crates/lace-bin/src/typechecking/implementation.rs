use crate::ast_metadata::Metadata;
use crate::parser::ast::{
    BinaryOp, Block, Expr, ExprKind, Function, Item, Lit, Statement, UnaryOp,
};
use crate::typechecking;
use crate::typechecking::ctx::TypeContext;
use crate::typechecking::error::TypeError;
use crate::typechecking::ty::PartialType;
use crate::typechecking::type_spec_to_partial_type;
use bumpalo::collections::Vec as BumpVec;

fn typecheck_lit<'a>(
    lit: &Metadata<&Lit>,
    ctx: &mut TypeContext<'a, '_>,
    expected_ty: PartialType<'a>,
) -> Result<(), TypeError> {
    let res = match lit.value {
        Lit::Bool(_) => ctx.unify(PartialType::Bool, expected_ty),
        Lit::Int(_) => ctx.unify(PartialType::Int, expected_ty),
        Lit::String(_) => ctx.unify(PartialType::String, expected_ty),
    };

    if let Err(e) = res {
        todo!()
    }

    Ok(())
}

fn typecheck_expr<'a>(
    expr: &Expr,
    ctx: &mut TypeContext<'a, '_>,
    expected_ty: PartialType<'a>,
) -> Result<(), TypeError> {
    match &expr.value {
        ExprKind::Lit(lit) => typecheck_lit(
            &Metadata {
                value: lit,
                metadata: expr.metadata,
            },
            ctx,
            expected_ty,
        ),
        ExprKind::If(_, _, _) => {
            todo!()
        }
        ExprKind::Block(b) => typecheck_block(b, ctx, expected_ty),
        ExprKind::Ident(v) => {
            let var = ctx.type_variable_for_identifier(&v);

            if let Err(e) = ctx.unify(expected_ty, var) {
                todo!()
            }

            Ok(())
        }
        ExprKind::Paren(e) => typecheck_expr(e, ctx, expected_ty),
        ExprKind::BinaryOp(op, l, r) => match op.value {
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Add | BinaryOp::Sub => {
                typecheck_expr(l, ctx, PartialType::Int)?;
                typecheck_expr(l, ctx, PartialType::Int)?;

                if let Err(e) = ctx.unify(PartialType::Int, expected_ty) {
                    todo!()
                }

                Ok(())
            }
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                typecheck_expr(l, ctx, PartialType::Bool)?;
                typecheck_expr(l, ctx, PartialType::Bool)?;

                if let Err(e) = ctx.unify(PartialType::Bool, expected_ty) {
                    todo!()
                }

                Ok(())
            }
            BinaryOp::Gt
            | BinaryOp::Gte
            | BinaryOp::Lt
            | BinaryOp::Lte
            | BinaryOp::Eq
            | BinaryOp::Neq => {
                let var = ctx.fresh();

                typecheck_expr(l, ctx, PartialType::Variable(var))?;
                typecheck_expr(l, ctx, PartialType::Variable(var))?;

                if let Err(e) = ctx.unify(PartialType::Bool, var) {
                    todo!()
                }

                Ok(())
            }
        },
        ExprKind::UnaryOp(op, expr) => match op.value {
            UnaryOp::Not => {
                typecheck_expr(expr, ctx, PartialType::Bool)?;

                if let Err(e) = ctx.unify(PartialType::Bool, expected_ty) {
                    todo!()
                }

                Ok(())
            }
            UnaryOp::Neg => {
                typecheck_expr(expr, ctx, PartialType::Int)?;

                if let Err(e) = ctx.unify(PartialType::Int, expected_ty) {
                    todo!()
                }

                Ok(())
            }
        },
        ExprKind::Tuple(t) => {
            todo!()
        }
        ExprKind::Call(expr, params) => {
            let f_ty = ctx.fresh();
            typecheck_expr(expr, ctx, PartialType::Variable(f_ty))?;

            let ret_var = ctx.fresh();

            let expected_f_ty = PartialType::Function {
                params: &[],
                ret: ctx.alloc(PartialType::Variable(ret_var)),
            };

            if let Err(e) = ctx.unify(f_ty, expected_f_ty) {
                todo!()
            }

            if let Err(e) = ctx.unify(PartialType::Int, expected_ty) {
                todo!()
            }

            Ok(())
        }
    }
}

fn typecheck_statement<'a>(
    stmt: &Statement,
    ctx: &mut TypeContext<'a, '_>,
    // TODO: for `return x` statements
    _return_type: PartialType<'a>,
) -> Result<(), TypeError> {
    match stmt {
        Statement::Expr(e) => {
            let var = ctx.fresh();
            typecheck_expr(e, ctx, PartialType::Variable(var))
        }
        Statement::Let(name, type_spec, expr) => {
            let expected_ty = type_spec
                .as_ref()
                .map(|i| typechecking::type_spec_to_partial_type(&i.value, ctx))
                .unwrap_or_else(|| PartialType::Variable(ctx.fresh()));

            typecheck_expr(expr, ctx, expected_ty)?;

            let tv = ctx.type_variable_for_identifier(name);
            if let Err(e) = ctx.unify(tv, expected_ty) {
                todo!()
            }

            Ok(())
        }
    }
}

fn typecheck_block<'a>(
    block: &Metadata<Block>,
    ctx: &mut TypeContext<'a, '_>,
    return_type: PartialType<'a>,
) -> Result<(), TypeError> {
    for i in block.value.stmts {
        typecheck_statement(i, ctx, return_type)?;
    }

    if let Some(ref expr) = block.value.last {
        let expected_ty = ctx.fresh();
        typecheck_expr(expr, ctx, PartialType::Variable(expected_ty))?;

        let Err(e) = ctx.unify(return_type, expected_ty) else {
            return Ok(());
        };

        todo!()
    } else {
        let Err(e) = ctx.unify(return_type, PartialType::Unit) else {
            return Ok(());
        };

        todo!("{e:?}")
    }
}

fn typecheck_function(f: &Function, ctx: &mut TypeContext) -> Result<(), TypeError> {
    let return_type = f
        .ret
        .as_ref()
        .map(|i| typechecking::type_spec_to_partial_type(&i.value, ctx))
        .unwrap_or(PartialType::Unit);

    let ty_var = ctx.type_variable_for_identifier(&f.name);
    let mut params = BumpVec::new_in(ctx.arena);
    for i in f.parameters {
        let ty = type_spec_to_partial_type(&i.type_spec.value, ctx);
        let tyv = ctx.type_variable_for_identifier(&i.name);
        if let Err(e) = ctx.unify(ty, tyv) {
            todo!()
        }

        params.push(tyv.into())
    }

    let func_ty = PartialType::Function {
        params: params.into_bump_slice(),
        ret: ctx.alloc(return_type),
    };

    if let Err(e) = ctx.unify(ty_var, func_ty) {
        todo!()
    }

    typecheck_block(f.block, ctx, return_type)
}

pub(super) fn typecheck_item(item: &Item, ctx: &mut TypeContext) -> Result<(), TypeError> {
    match item {
        Item::Function(f) => typecheck_function(&f.value, ctx),
    }
}
