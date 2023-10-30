use crate::ast_metadata::{Metadata, MetadataId};
use crate::parser::ast::{
    BinaryOp, Block, Expr, ExprKind, File, Function, Item, Lit, Parameter, Statement, TypeSpec,
    UnaryOp,
};
use crate::typechecking::constraint::TypeConstraintGenerator;
use crate::typechecking::constraint_metadata::ConstraintMetadata;
use crate::typechecking::context::TypeContext;
use crate::typechecking::ty::{ConcreteType, TypeVariable};
use bumpalo::collections::Vec;

impl<'a, 'sp> TypeConstraintGenerator<'a, 'sp> for Expr<'_, '_> {
    type TypeResult = TypeVariable;

    fn generate_constraints(&self, ctx: &mut TypeContext<'a, 'sp>) -> Self::TypeResult {
        match &self.value {
            ExprKind::Lit(l) => l.generate_constraints(ctx),
            ExprKind::If(condition, if_true, if_false) => {
                let condition_type = condition.generate_constraints(ctx);

                ctx.add_equal_constraint_concrete(
                    condition_type,
                    ConcreteType::Bool,
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
                    ctx.add_equal_constraint_concrete(
                        if_true_type,
                        ConcreteType::Unit,
                        ConstraintMetadata::NoConstraintMetadata,
                    );
                }

                if_true_type
            }
            ExprKind::Block(b) => b.generate_constraints(ctx).0,
            ExprKind::Ident(i) => ctx.type_of_name(i),
            ExprKind::Paren(expr) => expr.generate_constraints(ctx),
            // TODO: with operator overloading this obviously needs to become much more complicated.
            //       Also, we assume here that even for primitive types, + is only defined between integers, not strings.
            ExprKind::BinaryOp(op, l, r) => match op.value {
                BinaryOp::Mul | BinaryOp::Div | BinaryOp::Add | BinaryOp::Sub => {
                    let l_type = l.generate_constraints(ctx);
                    let r_type = r.generate_constraints(ctx);

                    ctx.add_equal_constraint_concrete(
                        l_type,
                        ConcreteType::Int,
                        ConstraintMetadata::BinaryOp(l.metadata, r.metadata, op.value),
                    );
                    ctx.add_equal_constraint_concrete(
                        r_type,
                        ConcreteType::Int,
                        ConstraintMetadata::BinaryOp(l.metadata, r.metadata, op.value),
                    );

                    ctx.concrete_type(ConcreteType::Int)
                }
                BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                    let l_type = l.generate_constraints(ctx);
                    let r_type = r.generate_constraints(ctx);

                    ctx.add_equal_constraint_concrete(
                        l_type,
                        ConcreteType::Bool,
                        ConstraintMetadata::BinaryOp(l.metadata, r.metadata, op.value),
                    );
                    ctx.add_equal_constraint_concrete(
                        r_type,
                        ConcreteType::Bool,
                        ConstraintMetadata::BinaryOp(l.metadata, r.metadata, op.value),
                    );

                    ctx.concrete_type(ConcreteType::Bool)
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

                    ctx.concrete_type(ConcreteType::Bool)
                }
            },
            ExprKind::UnaryOp(op, expr) => {
                let expr_type = expr.generate_constraints(ctx);

                let meta = ConstraintMetadata::UnaryOp(expr.metadata, op.value);
                match op.value {
                    UnaryOp::Not => {
                        ctx.add_equal_constraint_concrete(expr_type, ConcreteType::Bool, meta);
                        ctx.concrete_type(ConcreteType::Bool)
                    }
                    UnaryOp::Neg => {
                        ctx.add_equal_constraint_concrete(expr_type, ConcreteType::Int, meta);
                        ctx.concrete_type(ConcreteType::Int)
                    }
                }
            }
            ExprKind::Tuple(t) => {
                let mut res = Vec::new_in(ctx.arena);
                for i in *t {
                    let ty = i.generate_constraints(ctx);
                    res.push(ty);
                }

                ctx.concrete_type(ConcreteType::Tuple(res.into_bump_slice()))
            }
            ExprKind::Call(f, args) => {
                let mut param_types = Vec::new_in(ctx.arena);
                for i in args.value {
                    param_types.push(i.generate_constraints(ctx));
                }

                let ret_ty = ctx.fresh();

                let expected_f_ty = ConcreteType::Function {
                    params: param_types.into_bump_slice(),
                    ret: ctx.alloc(ret_ty),
                };
                let defined_f_ty = f.generate_constraints(ctx);

                ctx.add_equal_constraint_concrete(
                    defined_f_ty,
                    expected_f_ty,
                    ConstraintMetadata::Call {
                        call_expr: self.metadata,
                    },
                );

                ret_ty
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
    type TypeResult = (TypeVariable, BlockReturn);

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
            (ctx.concrete_type(ConcreteType::Unit), BlockReturn::None)
        }
    }
}

impl<'a, 'sp> TypeConstraintGenerator<'a, 'sp> for Lit<'_> {
    type TypeResult = TypeVariable;

    fn generate_constraints(&self, ctx: &mut TypeContext<'a, 'sp>) -> Self::TypeResult {
        match self {
            Lit::Bool(_) => ctx.concrete_type(ConcreteType::Bool),
            Lit::Int(_) => ctx.concrete_type(ConcreteType::Int),
            Lit::String(_) => ctx.concrete_type(ConcreteType::String),
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
    type TypeResult = TypeVariable;

    fn generate_constraints(&self, ctx: &mut TypeContext<'a, 'sp>) -> Self::TypeResult {
        match self {
            // TODO: somewhere make the distinction between primitives and compounds, if necessary
            TypeSpec::Name(n) => match n.value.string {
                // TODO: expand to all different int types
                "int" => ctx.concrete_type(ConcreteType::Int),
                "string" => ctx.concrete_type(ConcreteType::String),
                "bool" => ctx.concrete_type(ConcreteType::Bool),
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
            ctx.concrete_type(ConcreteType::Unit)
        };

        // gives this function type
        let function_type = ConcreteType::Function {
            params: &[],
            ret: ctx.alloc(ret_ty_spec),
        };

        // the function name has this type
        let name = ctx.type_of_name(name);
        ctx.add_equal_constraint_concrete(
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
