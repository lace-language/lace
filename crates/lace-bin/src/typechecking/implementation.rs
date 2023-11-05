use crate::ast_metadata::{Metadata, MetadataId};
use crate::lice::Lice;
use crate::parser::ast::{
    BinaryOp, Block, Expr, ExprKind, Function, Item, Lit, Statement, UnaryOp,
};
use crate::typechecking::ctx::TypeContext;
use crate::typechecking::error::{FailedUnification, ResultExt, TypeError};
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

    if let Err((l, r)) = res {
        return Err(TypeError::FailedUnification(FailedUnification {
            expected: r.to_string(),
            was: l.to_string(),
            was_span: ctx.span_for(lit.metadata),
        }));
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
        ExprKind::If(condition, l, r) => {
            typecheck_expr(condition, ctx, PartialType::Bool).on_failed_unification(|uni| {
                TypeError::IfCondition {
                    condition_span: ctx.span_for(condition.metadata),
                    condition_ty: uni.was,
                    if_span: ctx.span_for(expr.metadata),
                }
            })?;

            if let Some(r) = r {
                let lvar = ctx.fresh();
                let rvar = ctx.fresh();

                typecheck_block(l, ctx, (lvar.into(), None))?;
                typecheck_block(r, ctx, (rvar.into(), None))?;

                if let Err((lty, rty)) = ctx.unify(lvar, rvar) {
                    return Err(TypeError::IfElseEqual {
                        if_return_span: ctx.span_for(
                            l.value
                                .last
                                .as_ref()
                                .unwrap_or_lice("should have a return expr")
                                .metadata,
                        ),
                        else_return_span: ctx.span_for(
                            r.value
                                .last
                                .as_ref()
                                .unwrap_or_lice("should have a return expr")
                                .metadata,
                        ),
                        if_ty: lty.to_string(),
                        else_ty: rty.to_string(),
                        if_block_span: ctx.span_for(l.metadata),
                    });
                }
            } else {
                let lvar = ctx.fresh();
                typecheck_block(l, ctx, (lvar.into(), None))?;

                if let Err((lty, _)) = ctx.unify(lvar, PartialType::Unit) {
                    return Err(TypeError::IfWithUnexpectedReturn {
                        return_span: ctx.span_for(
                            l.value
                                .last
                                .as_ref()
                                .unwrap_or_lice("should have a return expr")
                                .metadata,
                        ),
                        return_ty: lty.to_string(),
                        if_block_span: ctx.span_for(l.metadata),
                    });
                }
            }

            Ok(())
        }
        ExprKind::Block(b) => typecheck_block(b, ctx, (expected_ty, None)),
        ExprKind::Ident(v) => {
            let var = ctx.type_variable_for_identifier(v);

            if let Err((l, r)) = ctx.unify(expected_ty, var) {
                return Err(TypeError::FailedUnification(FailedUnification {
                    expected: l.to_string(),
                    was: r.to_string(),
                    was_span: ctx.span_for(v.metadata),
                }));
            }

            Ok(())
        }
        ExprKind::Paren(e) => typecheck_expr(e, ctx, expected_ty),
        ExprKind::BinaryOp(op, l, r) => {
            macro_rules! failed {
                ($side: literal, $ty: expr) => {
                    |uni: FailedUnification| TypeError::BinaryOp {
                        op: op.value,
                        value_ty: uni.was,
                        expected_ty: $ty.to_string(),
                        side: $side,
                        value_span: uni.was_span,
                        op_span: ctx.span_for(op.metadata),
                    }
                };
            }

            match op.value {
                BinaryOp::Mul | BinaryOp::Div | BinaryOp::Add | BinaryOp::Sub => {
                    typecheck_expr(l, ctx, PartialType::Int)
                        .on_failed_unification(failed!("left", PartialType::Int))?;
                    typecheck_expr(r, ctx, PartialType::Int)
                        .on_failed_unification(failed!("right", PartialType::Int))?;

                    if let Err((l, r)) = ctx.unify(PartialType::Int, expected_ty) {
                        return Err(TypeError::FailedUnification(FailedUnification {
                            expected: r.to_string(),
                            was: l.to_string(),
                            was_span: ctx.span_for(expr.metadata),
                        }));
                    }

                    Ok(())
                }
                BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                    typecheck_expr(l, ctx, PartialType::Bool)
                        .on_failed_unification(failed!("left", PartialType::Bool))?;
                    typecheck_expr(l, ctx, PartialType::Bool)
                        .on_failed_unification(failed!("right", PartialType::Bool))?;

                    if let Err((l, r)) = ctx.unify(PartialType::Bool, expected_ty) {
                        return Err(TypeError::FailedUnification(FailedUnification {
                            expected: r.to_string(),
                            was: l.to_string(),
                            was_span: ctx.span_for(expr.metadata),
                        }));
                    }

                    Ok(())
                }
                BinaryOp::Gt
                | BinaryOp::Gte
                | BinaryOp::Lt
                | BinaryOp::Lte
                | BinaryOp::Eq
                | BinaryOp::Neq => {
                    let lvar = ctx.fresh();
                    let rvar = ctx.fresh();

                    typecheck_expr(l, ctx, PartialType::Variable(lvar))?;
                    typecheck_expr(r, ctx, PartialType::Variable(rvar))?;

                    if let Err((lt, rt)) = ctx.unify(lvar, rvar) {
                        return Err(TypeError::Comparison {
                            op: op.value,
                            left_ty: lt.to_string(),
                            right_ty: rt.to_string(),
                            left: ctx.span_for(l.metadata),
                            right: ctx.span_for(r.metadata),
                        });
                    }

                    if let Err((l, r)) = ctx.unify(PartialType::Bool, expected_ty) {
                        return Err(TypeError::FailedUnification(FailedUnification {
                            expected: r.to_string(),
                            was: l.to_string(),
                            was_span: ctx.span_for(expr.metadata),
                        }));
                    }

                    Ok(())
                }
            }
        }
        ExprKind::UnaryOp(op, expr) => match op.value {
            UnaryOp::Not => {
                typecheck_expr(expr, ctx, PartialType::Bool).on_failed_unification(
                    |uni: FailedUnification| TypeError::UnaryOp {
                        op: op.value,
                        expected_ty: PartialType::Bool.to_string(),
                        was_ty: uni.was.to_string(),
                        expr: ctx.span_for(expr.metadata),
                        op_span: ctx.span_for(op.metadata),
                    },
                )?;

                if let Err((l, r)) = ctx.unify(PartialType::Bool, expected_ty) {
                    return Err(TypeError::FailedUnification(FailedUnification {
                        expected: r.to_string(),
                        was: l.to_string(),
                        was_span: ctx.span_for(expr.metadata),
                    }));
                }

                Ok(())
            }
            UnaryOp::Neg => {
                typecheck_expr(expr, ctx, PartialType::Int).on_failed_unification(
                    |uni: FailedUnification| TypeError::UnaryOp {
                        op: op.value,
                        expected_ty: PartialType::Int.to_string(),
                        was_ty: uni.was.to_string(),
                        expr: ctx.span_for(expr.metadata),
                        op_span: ctx.span_for(op.metadata),
                    },
                )?;

                if let Err((l, r)) = ctx.unify(PartialType::Int, expected_ty) {
                    return Err(TypeError::FailedUnification(FailedUnification {
                        expected: r.to_string(),
                        was: l.to_string(),
                        was_span: ctx.span_for(expr.metadata),
                    }));
                }

                Ok(())
            }
        },
        ExprKind::Tuple(_) => {
            todo!()
        }
        ExprKind::Call(expr, params) => {
            let called_f_ty = ctx.fresh();
            typecheck_expr(expr, ctx, called_f_ty.into())?;

            let ret_var = ctx.fresh();
            let mut param_tys = BumpVec::new_in(ctx.arena);
            for param in params.value {
                let tyv = ctx.fresh();
                param_tys.push(tyv.into());

                typecheck_expr(param, ctx, tyv.into())?;
            }
            let expected_f_ty = PartialType::Function {
                params: param_tys.into_bump_slice(),
                ret: ctx.alloc(PartialType::Variable(ret_var)),
            };

            if let Err(e) = ctx.unify(called_f_ty, expected_f_ty) {
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
    _return_type: (PartialType<'a>, Option<MetadataId>),
) -> Result<(), TypeError> {
    match stmt {
        Statement::Expr(e) => {
            let var = ctx.fresh();
            typecheck_expr(e, ctx, PartialType::Variable(var))
        }
        Statement::Let(name, type_spec, expr) => {
            let expected_ty = type_spec
                .as_ref()
                .map(|i| type_spec_to_partial_type(&i.value, ctx))
                .unwrap_or_else(|| PartialType::Variable(ctx.fresh()));

            typecheck_expr(expr, ctx, expected_ty)?;

            let tv = ctx.type_variable_for_identifier(name);
            if let Err(_) = ctx.unify(tv, expected_ty) {
                lice!("should be the first usage of this type variable because this let is the definition")
            }

            Ok(())
        }
    }
}

fn typecheck_block<'a>(
    block: &Metadata<Block>,
    ctx: &mut TypeContext<'a, '_>,
    return_type: (PartialType<'a>, Option<MetadataId>),
) -> Result<(), TypeError> {
    for i in block.value.stmts {
        typecheck_statement(i, ctx, return_type)?;
    }

    if let Some(ref expr) = block.value.last {
        let expected_ty = ctx.fresh();
        typecheck_expr(expr, ctx, PartialType::Variable(expected_ty))?;

        let Err((l, r)) = ctx.unify(return_type.0, expected_ty) else {
            return Ok(());
        };

        if let Some(i) = return_type.1 {
            Err(TypeError::ImplicitReturn {
                expected_ret_ty: l.to_string(),
                expected_ret_ty_spec_span: ctx.span_for(i),
                was_ret_ty: r.to_string(),
                was_expr_span: ctx.span_for(expr.metadata),
            })
        } else {
            Err(TypeError::UnexpectedReturn {
                was_ret_ty: r.to_string(),
                was_expr_span: ctx.span_for(expr.metadata),
            })
        }
    } else {
        let Err((l, _)) = ctx.unify(return_type.0, PartialType::Unit) else {
            return Ok(());
        };

        Err(TypeError::ExpectedReturn {
            expected_ret_ty: l.to_string(),
            expected_ret_ty_spec_span: ctx.span_for(
                return_type.1.unwrap_or_lice(
                    "if ret == unit and last expr == unit we should never get here",
                ),
            ),
        })
    }
}

fn typecheck_function(f: &Function, ctx: &mut TypeContext) -> Result<(), TypeError> {
    let return_type = f
        .ret
        .as_ref()
        .map(|i| type_spec_to_partial_type(&i.value, ctx))
        .unwrap_or(PartialType::Unit);

    let mut params = BumpVec::new_in(ctx.arena);
    for i in f.parameters {
        let ty = type_spec_to_partial_type(&i.type_spec.value, ctx);
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
    let ty_var = ctx.type_variable_for_identifier(&f.name);

    if let Err(_) = ctx.unify(ty_var, func_ty) {
        lice!("should never fail because this is the first usage of this type variable, as we're declaring the function it describes here.");
    }

    typecheck_block(
        f.block,
        ctx,
        (return_type, f.ret.as_ref().map(|i| i.metadata)),
    )
}

pub(super) fn typecheck_item(item: &Item, ctx: &mut TypeContext) -> Result<(), TypeError> {
    match item {
        Item::Function(f) => typecheck_function(&f.value, ctx),
    }
}
