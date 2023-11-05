use crate::ast_metadata::{Metadata, MetadataId};
use crate::lice::Lice;
use crate::parser::ast::{
    BinaryOp, Block, Expr, ExprKind, Function, Item, Lit, Statement, UnaryOp,
};
use crate::typechecking;
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
        ExprKind::If(_, _, _) => {
            todo!()
        }
        ExprKind::Block(b) => typecheck_block(b, ctx, (expected_ty, None)),
        ExprKind::Ident(v) => {
            let var = ctx.type_variable_for_identifier(v);

            if let Err(e) = ctx.unify(expected_ty, var) {
                todo!()
            }

            Ok(())
        }
        ExprKind::Paren(e) => typecheck_expr(e, ctx, expected_ty),
        ExprKind::BinaryOp(op, l, r) => match op.value {
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Add | BinaryOp::Sub => {
                macro_rules! failed {
                    ($side: literal) => {
                        |uni: FailedUnification| TypeError::BinaryOp {
                            op: op.value,
                            value_ty: uni.was,
                            side: $side,
                            value_span: uni.was_span,
                            op_span: ctx.span_for(op.metadata),
                        }
                    };
                }

                typecheck_expr(l, ctx, PartialType::Int).on_failed_unification(failed!("left"))?;
                typecheck_expr(r, ctx, PartialType::Int).on_failed_unification(failed!("right"))?;

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
        },
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
        ExprKind::Tuple(t) => {
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
