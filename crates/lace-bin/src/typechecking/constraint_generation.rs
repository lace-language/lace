use crate::ast_metadata::{Metadata, MetadataId};
use crate::parser::ast::{
    BinaryOp, Block, Expr, ExprKind, File, Function, Item, Lit, Parameter, Statement, TypeSpec,
    UnaryOp,
};
use crate::typechecking::constraint::TypeConstraintGenerator;
use crate::typechecking::constraint_metadata::ConstraintMetadata;
use crate::typechecking::context::TypeContext;
use crate::typechecking::ty::{PartialType, TypeOrVariable};
use bumpalo::collections::Vec;

impl<'a, 'sp> TypeConstraintGenerator<'a, 'sp> for Expr<'_, '_> {
    type TypeResult = TypeOrVariable<'a>;

    fn generate_constraints(&self, ctx: &mut TypeContext<'a, 'sp>) -> Self::TypeResult {
        match &self.value {
            ExprKind::Lit(l) => l.generate_constraints(ctx),
            ExprKind::If(condition, if_true, if_false) => {
                let condition_type = condition.generate_constraints(ctx);

                ctx.add_equal_constraint(
                    condition_type,
                    PartialType::Bool,
                    ConstraintMetadata::BlockCondition(self.metadata),
                );

                let if_true_type = if_true.generate_constraints(ctx).0;

                if let Some(if_false) = if_false {
                    let if_false_type = if_false.generate_constraints(ctx).0;
                    ctx.add_equal_constraint(
                        if_true_type,
                        if_false_type,
                        ConstraintMetadata::IfReturn(if_true.metadata, if_false.metadata),
                    );
                } else {
                    // TODO: what's the context here?
                    ctx.add_equal_constraint(
                        if_true_type,
                        PartialType::Unit,
                        ConstraintMetadata::NoConstraintMetadata,
                    );
                }

                if_true_type
            }
            ExprKind::Block(b) => b.generate_constraints(ctx).0,
            ExprKind::Ident(i) => ctx.type_of_name(i).into(),
            ExprKind::Paren(expr) => expr.generate_constraints(ctx),
            // TODO: with operator overloading this obviously needs to become much more complicated.
            //       Also, we assume here that even for primitive types, + is only defined between integers, not strings.
            ExprKind::BinaryOp(op, l, r) => match op.value {
                BinaryOp::Mul | BinaryOp::Div | BinaryOp::Add | BinaryOp::Sub => {
                    let l_type = l.generate_constraints(ctx);
                    let r_type = r.generate_constraints(ctx);

                    ctx.add_equal_constraint(
                        l_type,
                        PartialType::Int,
                        ConstraintMetadata::BinaryOp(r.metadata, l.metadata, op.value),
                    );
                    ctx.add_equal_constraint(
                        r_type,
                        PartialType::Int,
                        ConstraintMetadata::BinaryOp(l.metadata, r.metadata, op.value),
                    );

                    PartialType::Int.into()
                }
                BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                    let l_type = l.generate_constraints(ctx);
                    let r_type = r.generate_constraints(ctx);

                    ctx.add_equal_constraint(
                        l_type,
                        PartialType::Bool,
                        ConstraintMetadata::BinaryOp(l.metadata, r.metadata, op.value),
                    );
                    ctx.add_equal_constraint(
                        r_type,
                        PartialType::Bool,
                        ConstraintMetadata::BinaryOp(l.metadata, r.metadata, op.value),
                    );

                    PartialType::Bool.into()
                }
                BinaryOp::Gt
                | BinaryOp::Gte
                | BinaryOp::Lt
                | BinaryOp::Lte
                | BinaryOp::Eq
                | BinaryOp::Neq => {
                    let l_type = l.generate_constraints(ctx);
                    let r_type = r.generate_constraints(ctx);

                    ctx.add_equal_constraint(
                        l_type,
                        r_type,
                        ConstraintMetadata::BinaryOp(l.metadata, r.metadata, op.value),
                    );

                    PartialType::Bool.into()
                }
            },
            ExprKind::UnaryOp(op, expr) => {
                let expr_type = expr.generate_constraints(ctx);

                let meta = ConstraintMetadata::UnaryOp(expr.metadata, op.value);
                match op.value {
                    UnaryOp::Not => {
                        ctx.add_equal_constraint(expr_type, PartialType::Bool, meta);
                        PartialType::Bool.into()
                    }
                    UnaryOp::Neg => {
                        ctx.add_equal_constraint(expr_type, PartialType::Int, meta);
                        PartialType::Int.into()
                    }
                }
            }
            ExprKind::Tuple(t) => {
                let mut res = Vec::new_in(ctx.arena);
                for i in *t {
                    let ty = i.generate_constraints(ctx);
                    res.push(ty);
                }

                PartialType::Tuple(res.into_bump_slice()).into()
            }
            ExprKind::Call(f, args) => {
                let mut param_types = Vec::new_in(ctx.arena);
                for i in args.value {
                    param_types.push(i.generate_constraints(ctx));
                }

                let ret_ty = ctx.fresh();

                let expected_f_ty = PartialType::Function {
                    params: param_types.into_bump_slice(),
                    ret: ctx.alloc(ret_ty.into()),
                };
                let defined_f_ty = f.generate_constraints(ctx);

                ctx.add_equal_constraint(
                    defined_f_ty,
                    expected_f_ty,
                    ConstraintMetadata::Call {
                        call_expr: self.metadata,
                    },
                );

                ret_ty.into()
            }
        }
    }
}

/// The types of ways in which a block could have returns in it
/// TODO: explicit return
pub enum BlockReturn {
    /// last item has semi, unit return
    None,
    /// return expression
    Implicit(MetadataId),
}

impl<'a, 'sp> TypeConstraintGenerator<'a, 'sp> for Metadata<Block<'_, '_>> {
    type TypeResult = (TypeOrVariable<'a>, BlockReturn);

    fn generate_constraints(&self, ctx: &mut TypeContext<'a, 'sp>) -> Self::TypeResult {
        for i in self.value.stmts {
            i.generate_constraints(ctx);
        }

        if let Some(ref last) = self.value.last {
            (
                last.generate_constraints(ctx),
                BlockReturn::Implicit(last.metadata),
            )
        } else {
            (PartialType::Unit.into(), BlockReturn::None)
        }
    }
}

impl<'a, 'sp> TypeConstraintGenerator<'a, 'sp> for Lit<'_> {
    type TypeResult = TypeOrVariable<'a>;

    fn generate_constraints(&self, _ctx: &mut TypeContext<'a, 'sp>) -> Self::TypeResult {
        match self {
            Lit::Bool(_) => PartialType::Bool.into(),
            Lit::Int(_) => PartialType::Int.into(),
            Lit::String(_) => PartialType::String.into(),
        }
    }
}

impl<'a, 'sp> TypeConstraintGenerator<'a, 'sp> for Statement<'_, '_> {
    type TypeResult = ();

    fn generate_constraints(&self, ctx: &mut TypeContext<'a, 'sp>) -> Self::TypeResult {
        match self {
            Statement::Expr(e) => {
                // whatever :shrug:
                let _ = e.generate_constraints(ctx);
            }
            Statement::Let(name, type_spec, value) => {
                let value_type = value.generate_constraints(ctx);

                let name_ty = ctx.type_of_name(name);
                ctx.add_equal_constraint(
                    name_ty,
                    value_type,
                    ConstraintMetadata::Assignment {
                        name: name.metadata,
                        value: value.metadata,
                    },
                );

                if let Some(spec) = type_spec {
                    let spec_ty = spec.value.generate_constraints(ctx);
                    ctx.add_equal_constraint(
                        value_type,
                        spec_ty,
                        ConstraintMetadata::TypeSpec {
                            spec: spec.metadata,
                            name: name.metadata,
                        },
                    );
                }
            }
        }
    }
}

impl<'a, 'sp> TypeConstraintGenerator<'a, 'sp> for TypeSpec<'_> {
    type TypeResult = TypeOrVariable<'a>;

    fn generate_constraints(&self, _ctx: &mut TypeContext<'a, 'sp>) -> Self::TypeResult {
        match self {
            // TODO: somewhere make the distinction between primitives and compounds, if necessary
            TypeSpec::Name(n) => match n.value.string {
                // TODO: expand to all different int types
                "int" => PartialType::Int.into(),
                "string" => PartialType::String.into(),
                "bool" => PartialType::Bool.into(),
                other => unimplemented!("type {other} not yet supported. Use int, string or bool"),
            },
        }
    }
}

impl<'a, 'sp> TypeConstraintGenerator<'a, 'sp> for File<'_, '_> {
    type TypeResult = ();

    fn generate_constraints(&self, ctx: &mut TypeContext<'a, 'sp>) -> Self::TypeResult {
        for i in self.items {
            i.generate_constraints(ctx);
        }
    }
}

impl<'a, 'sp> TypeConstraintGenerator<'a, 'sp> for Item<'_, '_> {
    type TypeResult = ();

    fn generate_constraints(&self, ctx: &mut TypeContext<'a, 'sp>) -> Self::TypeResult {
        match self {
            Item::Function(f) => f.generate_constraints(ctx),
        }
    }
}

impl<'a, 'sp> TypeConstraintGenerator<'a, 'sp> for Metadata<Function<'_, '_>> {
    type TypeResult = ();

    fn generate_constraints(&self, ctx: &mut TypeContext<'a, 'sp>) -> Self::TypeResult {
        let Function {
            name,
            parameters,
            ret,
            block,
        } = &self.value;
        let mut param_types = Vec::new_in(ctx.arena);

        // parameters
        for i in *parameters {
            let Parameter { name, type_spec } = i;

            let param_ty = ctx.type_of_name(name);
            let spec_ty = type_spec.value.generate_constraints(ctx);

            ctx.add_equal_constraint(
                param_ty,
                spec_ty,
                ConstraintMetadata::TypeSpec {
                    spec: type_spec.metadata,
                    name: name.metadata,
                },
            );

            param_types.push(param_ty);
        }

        // return type
        let ret_ty_spec = if let Some(ret) = ret {
            let ret_ty_spec = ret.value.generate_constraints(ctx);
            ret_ty_spec
        } else {
            PartialType::Unit.into()
        };

        // gives this function type
        let function_type = PartialType::Function {
            params: &[],
            ret: ctx.alloc(ret_ty_spec),
        };

        // the function name has this type
        let name = ctx.type_of_name(name);
        ctx.add_equal_constraint(
            name,
            function_type,
            ConstraintMetadata::FunctionDefinition {
                value: self.metadata,
            },
        );

        // and the block should return this type
        let (block_ret_ty, ret) = block.generate_constraints(ctx);
        ctx.add_equal_constraint(
            block_ret_ty,
            ret_ty_spec,
            ConstraintMetadata::FunctionReturn {
                function: self.metadata,
                ret,
            },
        );
    }
}
