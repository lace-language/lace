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

#[derive(Copy, Clone, Debug)]
pub struct ReturnContext<'a> {
    /// the expected outcome of the current expr
    expected_type: PartialType<'a>,

    /// for explicit returns, return expressions
    #[allow(unused)]
    function_return_type: PartialType<'a>,
    function_return_type_span: MetadataId,

    /// for breaks and return expressions etc.
    block_return_type: PartialType<'a>,

    /// is the block we're typechecking the toplevel scope of a function?
    is_function_block: bool,
}

fn typecheck_lit<'a>(
    lit: &Metadata<&Lit>,
    ctx: &mut TypeContext<'a, '_, '_>,
    return_context: ReturnContext<'a>,
) -> Result<(), TypeError> {
    let partial_type = match lit.value {
        Lit::Bool(_) => PartialType::Bool,
        Lit::Int(_) => PartialType::Variable(ctx.fresh_int()),
        Lit::String(_) => PartialType::String,
    };
    let res = ctx.unify(partial_type, return_context.expected_type);

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
    ctx: &mut TypeContext<'a, '_, '_>,
    return_context: ReturnContext<'a>,
) -> Result<(), TypeError> {
    ctx.store_type_info_for_node(expr, return_context.expected_type);

    match &expr.value {
        ExprKind::Lit(lit) => typecheck_lit(
            &Metadata {
                value: lit,
                metadata: expr.metadata,
            },
            ctx,
            return_context,
        ),
        ExprKind::If(condition, if_true, if_false) => {
            typecheck_expr(
                condition,
                ctx,
                ReturnContext {
                    expected_type: PartialType::Bool,
                    ..return_context
                },
            )
            .on_failed_unification(|uni| TypeError::IfCondition {
                condition_span: ctx.span_for(condition.metadata),
                condition_ty: uni.was,
                if_span: ctx.span_for(expr.metadata),
            })?;

            if let Some(r) = if_false {
                let if_true_var = ctx.fresh();
                let if_false_var = ctx.fresh();

                typecheck_block(
                    if_true,
                    ctx,
                    &ReturnContext {
                        expected_type: if_true_var.into(),
                        block_return_type: if_true_var.into(),
                        is_function_block: false,
                        ..return_context
                    },
                )?;
                typecheck_block(
                    r,
                    ctx,
                    &ReturnContext {
                        expected_type: if_false_var.into(),
                        block_return_type: if_false_var.into(),
                        is_function_block: false,
                        ..return_context
                    },
                )?;

                if let Err((if_true_ty, if_false_ty)) = ctx.unify(if_true_var, if_false_var) {
                    return Err(TypeError::IfElseEqual {
                        if_true_return_span: if_true
                            .value
                            .last
                            .as_ref()
                            .map(|i| ctx.span_for(i.metadata)),
                        if_true_block_span: ctx.span_for(if_true.metadata),
                        if_false_return_span: r
                            .value
                            .last
                            .as_ref()
                            .map(|i| ctx.span_for(i.metadata)),
                        if_false_block_span: ctx.span_for(r.metadata),
                        if_ty: if_true_ty.to_string(),
                        else_ty: if_false_ty.to_string(),
                        if_block_span: ctx.span_for(if_true.metadata),
                    });
                }

                if let Err((l, r)) = ctx.unify(if_true_var, return_context.expected_type) {
                    return Err(TypeError::FailedUnification(FailedUnification {
                        expected: r.to_string(),
                        was: l.to_string(),
                        was_span: ctx.span_for(expr.metadata),
                    }));
                }
            } else {
                let if_true_var = ctx.fresh();
                typecheck_block(
                    if_true,
                    ctx,
                    &ReturnContext {
                        expected_type: if_true_var.into(),
                        is_function_block: false,
                        ..return_context
                    },
                )?;

                if let Err((if_true_ty, _)) = ctx.unify(if_true_var, PartialType::Unit) {
                    return Err(TypeError::IfWithUnexpectedReturn {
                        return_span: ctx.span_for(
                            if_true
                                .value
                                .last
                                .as_ref()
                                .unwrap_or_lice("should have a return expr")
                                .metadata,
                        ),
                        return_ty: if_true_ty.to_string(),
                        if_block_span: ctx.span_for(if_true.metadata),
                    });
                }

                if let Err((l, r)) = ctx.unify(PartialType::Unit, return_context.expected_type) {
                    return Err(TypeError::FailedUnification(FailedUnification {
                        expected: r.to_string(),
                        was: l.to_string(),
                        was_span: ctx.span_for(expr.metadata),
                    }));
                }
            }

            Ok(())
        }
        ExprKind::Block(b) => typecheck_block(
            b,
            ctx,
            &ReturnContext {
                block_return_type: return_context.expected_type,
                is_function_block: false,
                ..return_context
            },
        ),
        ExprKind::Ident(v) => {
            let var = ctx.type_variable_for_identifier(v);

            if let Err((l, r)) = ctx.unify(return_context.expected_type, var) {
                return Err(TypeError::FailedUnification(FailedUnification {
                    expected: l.to_string(),
                    was: r.to_string(),
                    was_span: ctx.span_for(v.metadata),
                }));
            }

            Ok(())
        }
        ExprKind::Paren(e) => typecheck_expr(e, ctx, return_context),
        ExprKind::BinaryOp(op, l, r) => {
            macro_rules! failed {
                ($side: literal, $ty: expr) => {
                    |uni: FailedUnification| TypeError::ExactBinaryOp {
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
                    let ltyv = ctx.fresh_int().into();
                    let rtyv = ctx.fresh_int().into();

                    typecheck_expr(
                        l,
                        ctx,
                        ReturnContext {
                            expected_type: ltyv,
                            ..return_context
                        },
                    )
                    // todo: these errors can be nicer and more precise, but this won't be the final code for this check anyway.
                    .on_failed_unification(failed!("left", ltyv))?;
                    typecheck_expr(
                        r,
                        ctx,
                        ReturnContext {
                            expected_type: rtyv,
                            ..return_context
                        },
                    )
                    .on_failed_unification(failed!("right", rtyv))?;

                    if let Err((lt, rt)) = ctx.unify(ltyv, rtyv) {
                        return Err(TypeError::BinaryOp {
                            left_ty: lt.to_string(),
                            left_span: ctx.span_for(l.metadata),
                            right_ty: rt.to_string(),
                            right_span: ctx.span_for(r.metadata),

                            op: op.value,
                        });
                    }
                    if let Err((l, r)) = ctx.unify(ltyv, return_context.expected_type) {
                        return Err(TypeError::FailedUnification(FailedUnification {
                            expected: r.to_string(),
                            was: l.to_string(),
                            was_span: ctx.span_for(expr.metadata),
                        }));
                    }

                    Ok(())
                }
                BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                    typecheck_expr(
                        l,
                        ctx,
                        ReturnContext {
                            expected_type: PartialType::Bool,
                            ..return_context
                        },
                    )
                    .on_failed_unification(failed!("left", PartialType::Bool))?;
                    typecheck_expr(
                        r,
                        ctx,
                        ReturnContext {
                            expected_type: PartialType::Bool,
                            ..return_context
                        },
                    )
                    .on_failed_unification(failed!("right", PartialType::Bool))?;

                    if let Err((l, r)) = ctx.unify(PartialType::Bool, return_context.expected_type)
                    {
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

                    typecheck_expr(
                        l,
                        ctx,
                        ReturnContext {
                            expected_type: PartialType::Variable(lvar),
                            ..return_context
                        },
                    )?;
                    typecheck_expr(
                        r,
                        ctx,
                        ReturnContext {
                            expected_type: PartialType::Variable(rvar),
                            ..return_context
                        },
                    )?;

                    if let Err((lt, rt)) = ctx.unify(lvar, rvar) {
                        return Err(TypeError::Comparison {
                            op: op.value,
                            left_ty: lt.to_string(),
                            right_ty: rt.to_string(),
                            left: ctx.span_for(l.metadata),
                            right: ctx.span_for(r.metadata),
                        });
                    }

                    if let Err((l, r)) = ctx.unify(PartialType::Bool, return_context.expected_type)
                    {
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
                typecheck_expr(
                    expr,
                    ctx,
                    ReturnContext {
                        expected_type: PartialType::Bool,
                        ..return_context
                    },
                )
                .on_failed_unification(|uni: FailedUnification| {
                    TypeError::UnaryOp {
                        op: op.value,
                        expected_ty: PartialType::Bool.to_string(),
                        was_ty: uni.was.to_string(),
                        expr: ctx.span_for(expr.metadata),
                        op_span: ctx.span_for(op.metadata),
                    }
                })?;

                if let Err((l, r)) = ctx.unify(PartialType::Bool, return_context.expected_type) {
                    return Err(TypeError::FailedUnification(FailedUnification {
                        expected: r.to_string(),
                        was: l.to_string(),
                        was_span: ctx.span_for(expr.metadata),
                    }));
                }

                Ok(())
            }
            UnaryOp::Neg => {
                let ty = ctx.fresh_int().into();

                typecheck_expr(
                    expr,
                    ctx,
                    ReturnContext {
                        expected_type: ty,
                        ..return_context
                    },
                )
                .on_failed_unification(|uni: FailedUnification| {
                    TypeError::UnaryOp {
                        op: op.value,
                        expected_ty: ty.to_string(),
                        was_ty: uni.was.to_string(),
                        expr: ctx.span_for(expr.metadata),
                        op_span: ctx.span_for(op.metadata),
                    }
                })?;

                if let Err((l, r)) = ctx.unify(ty, return_context.expected_type) {
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
        ExprKind::Call(call, params) => {
            let called_f_ty = ctx.fresh();
            typecheck_expr(
                call,
                ctx,
                ReturnContext {
                    expected_type: called_f_ty.into(),
                    ..return_context
                },
            )?;

            let ret_var = ctx.fresh();
            let mut param_tys = BumpVec::new_in(ctx.arena);
            for param in params.value {
                let tyv = ctx.fresh();
                param_tys.push(tyv.into());

                typecheck_expr(
                    param,
                    ctx,
                    ReturnContext {
                        expected_type: tyv.into(),
                        ..return_context
                    },
                )?;
            }
            let expected_f_ty = PartialType::Function {
                params: param_tys.into_bump_slice(),
                ret: ctx.alloc(PartialType::Variable(ret_var)),
                function_name: None,
            };

            if let Err((l, r)) = ctx.unify(called_f_ty, expected_f_ty) {
                if let PartialType::Function { .. } = l {
                    return Err(TypeError::FunctionCall {
                        expected_type: r.to_string(),
                        actual_span: ctx.span_for(call.metadata),
                        actual_type: l.to_string(),
                    });
                } else {
                    return Err(TypeError::NotAFunction {
                        actual_type: l.to_string(),
                        actual_span: ctx.span_for(call.metadata),
                    });
                }
            }

            if let Err((l, r)) = ctx.unify(ret_var, return_context.expected_type) {
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

fn typecheck_statement<'a>(
    stmt: &Statement,
    ctx: &mut TypeContext<'a, '_, '_>,
    return_context: &ReturnContext<'a>,
) -> Result<(), TypeError> {
    match stmt {
        Statement::Expr(e) => {
            let var = ctx.fresh();
            typecheck_expr(
                e,
                ctx,
                ReturnContext {
                    expected_type: var.into(),
                    ..*return_context
                },
            )
        }
        Statement::Let(name, type_spec, expr) => {
            let expected_type = ctx.fresh().into();

            typecheck_expr(
                expr,
                ctx,
                ReturnContext {
                    expected_type,
                    ..*return_context
                },
            )?;

            if let Some(i) = type_spec {
                let type_spec = type_spec_to_partial_type(&i.value);

                if let Err((l, _)) = ctx.unify(expected_type, type_spec) {
                    return Err(TypeError::LetSpec {
                        type_spec_span: ctx.span_for(i.metadata),
                        type_spec_type: type_spec.to_string(),
                        was_type: l.to_string(),
                        expr_span: ctx.span_for(expr.metadata),
                    });
                }
            };

            let tv = ctx.type_variable_for_identifier(name);
            if ctx.unify(tv, expected_type).is_err() {
                lice!("should be the first usage of this type variable because this let is the definition")
            }

            Ok(())
        }
    }
}

fn typecheck_block<'a>(
    block: &Metadata<Block>,
    ctx: &mut TypeContext<'a, '_, '_>,
    return_context: &ReturnContext<'a>,
) -> Result<(), TypeError> {
    for i in block.value.stmts {
        typecheck_statement(i, ctx, return_context)?;
    }

    if let Some(ref expr) = block.value.last {
        let expected_ty = ctx.fresh();
        typecheck_expr(
            expr,
            ctx,
            ReturnContext {
                expected_type: expected_ty.into(),
                ..*return_context
            },
        )?;

        let Err((l, r)) = ctx.unify(return_context.block_return_type, expected_ty) else {
            return Ok(());
        };

        if return_context.is_function_block {
            Err(TypeError::ImplicitFunctionReturn {
                expected_ret_ty: l.to_string(),
                expected_ret_ty_spec_span: ctx.span_for(return_context.function_return_type_span),
                was_ret_ty: r.to_string(),
                was_expr_span: ctx.span_for(expr.metadata),
            })
        } else if block.value.last.is_some() {
            Err(TypeError::UnexpectedReturn {
                was_ret_ty: r.to_string(),
                was_expr_span: ctx.span_for(expr.metadata),
            })
        } else {
            Err(TypeError::ImplicitReturn {
                expected_ret_ty: l.to_string(),
                was_ret_ty: r.to_string(),
                was_expr_span: ctx.span_for(expr.metadata),
            })
        }
    } else {
        let Err((l, _)) = ctx.unify(return_context.block_return_type, PartialType::Unit) else {
            return Ok(());
        };

        if return_context.is_function_block {
            Err(TypeError::ExpectedFunctionReturn {
                expected_ret_ty: l.to_string(),
                expected_ret_ty_spec_span: ctx.span_for(return_context.function_return_type_span),
            })
        } else {
            Err(TypeError::ExpectedReturn {
                expected_ret_ty: l.to_string(),
            })
        }
    }
}

fn typecheck_function(f: &Metadata<Function>, ctx: &mut TypeContext) -> Result<(), TypeError> {
    let ty_var = ctx.type_variable_for_identifier(&f.value.name);
    let Some(PartialType::Function { ret, .. }) = ctx.types.type_of_type_variable(ty_var) else {
        lice!("static pass should have resolved this type");
    };

    let return_type = **ret;

    typecheck_block(
        f.value.block,
        ctx,
        &ReturnContext {
            expected_type: return_type,
            function_return_type: return_type,
            function_return_type_span: f
                .value
                .ret
                .as_ref()
                .map(|i| i.metadata)
                .unwrap_or(f.metadata),
            block_return_type: return_type,
            is_function_block: true,
        },
    )
}

pub(super) fn typecheck_item(item: &Item, ctx: &mut TypeContext) -> Result<(), TypeError> {
    match item {
        Item::Function(f) => typecheck_function(f, ctx),
    }
}
