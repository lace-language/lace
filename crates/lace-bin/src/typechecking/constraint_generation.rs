use crate::parser::ast::{BinaryOp, Block, ExprKind, File, Function, Item, Lit, Parameter, Statement, TypeSpec, UnaryOp};
use bumpalo::collections::Vec;
use crate::parser::span::Spanned;
use crate::typechecking::constraint::TypeConstraintGenerator;
use crate::typechecking::context::TypeContext;
use crate::typechecking::ty::PartialType;
use crate::typechecking::ty::TypeOrVariable;

impl<'a> TypeConstraintGenerator<'a> for ExprKind<'_, '_> {
    type TypeResult = TypeOrVariable<'a>;

    fn generate_constraints(&self, ctx: &mut TypeContext<'a>) -> Self::TypeResult {
        match self {
            ExprKind::Lit(l) => l.generate_constraints(ctx),
            ExprKind::If(condition, if_true, if_false) => {
                let condition_type = condition.generate_constraints(ctx);
                ctx.add_equal_constraint(condition_type, PartialType::Bool);
                let if_true_type = if_true.generate_constraints(ctx);

                if let Some(if_false) = if_false {
                    let if_false_type = if_false.generate_constraints(ctx);
                    ctx.add_equal_constraint(if_true_type, if_false_type);
                } else {
                    ctx.add_equal_constraint(if_true_type, PartialType::Unit);
                }


                if_true_type
            }
            ExprKind::Block(b) => {
                b.generate_constraints(ctx)
            }
            ExprKind::Ident(i) => {
                ctx.type_of_name(i)
            }
            ExprKind::Paren(expr) => {
                expr.generate_constraints(ctx)
            }
            // TODO: with operator overloading this obviously needs to become much more complicated.
            //       Also, we assume here that even for primitive types, + is only defined between integers, not strings.
            ExprKind::BinaryOp(op, l, r) => match op.value {
                BinaryOp::Mul |
                BinaryOp::Div |
                BinaryOp::Add |
                BinaryOp::Sub => {
                    let l_type = l.generate_constraints(ctx);
                    let r_type = r.generate_constraints(ctx);

                    ctx.add_equal_constraint(l_type, PartialType::Int);
                    ctx.add_equal_constraint(r_type, PartialType::Int);

                    PartialType::Int.into()
                }
                BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                    let l_type = l.generate_constraints(ctx);
                    let r_type = r.generate_constraints(ctx);

                    ctx.add_equal_constraint(l_type, PartialType::Bool);
                    ctx.add_equal_constraint(r_type, PartialType::Bool);

                    PartialType::Bool.into()
                }
                BinaryOp::Gt |
                BinaryOp::Gte |
                BinaryOp::Lt |
                BinaryOp::Lte |
                BinaryOp::Eq |
                BinaryOp::Neq => {
                    let l_type = l.generate_constraints(ctx);
                    let r_type = r.generate_constraints(ctx);

                    ctx.add_equal_constraint(l_type, r_type);

                    PartialType::Bool.into()
                }
            }
            ExprKind::UnaryOp(op, expr) => {
                let expr_type = expr.generate_constraints(ctx);

                match op.value {
                    UnaryOp::Not => {
                        ctx.add_equal_constraint(expr_type, PartialType::Bool);

                        PartialType::Bool.into()
                    }
                    UnaryOp::Neg => {
                        ctx.add_equal_constraint(expr_type, PartialType::Int);

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

                let ret_ty = ctx.fresh().into();

                let expected_f_ty = PartialType::Function { params: param_types.into_bump_slice(), ret: ctx.alloc(ret_ty) };
                let defined_f_ty = f.generate_constraints(ctx);

                ctx.add_equal_constraint(defined_f_ty, expected_f_ty);

                ret_ty
            }
        }
    }
}

impl<'a> TypeConstraintGenerator<'a> for Spanned<Block<'_, '_>> {
    type TypeResult = TypeOrVariable<'a>;

    fn generate_constraints(&self, ctx: &mut TypeContext<'a>) -> Self::TypeResult {
        for i in self.value.stmts {
            i.generate_constraints(ctx);
        }

        if let Some(ref last) = self.value.last {
            last.generate_constraints(ctx)
        } else {
            PartialType::Unit.into()
        }
    }
}

impl<'a> TypeConstraintGenerator<'a> for Lit<'_> {
    type TypeResult = TypeOrVariable<'a>;

    fn generate_constraints(&self, _ctx: &mut TypeContext<'a>) -> Self::TypeResult {
        match self {
            Lit::Bool(_) => PartialType::Bool.into(),
            Lit::Int(_) => PartialType::Int.into(),
            Lit::String(_) => PartialType::String.into(),
        }
    }
}

impl<'a> TypeConstraintGenerator<'a> for Statement<'_, '_> {
    type TypeResult = ();

    fn generate_constraints(&self, ctx: &mut TypeContext<'a>) -> Self::TypeResult {
        match self {
            Statement::Expr(e) => {
                // whatever :shrug:
                let _ = e.generate_constraints(ctx);
            }
            Statement::Let(name, type_spec, value) => {
                let value_type = value.generate_constraints(ctx);

                let name_ty = ctx.type_of_name(name);
                ctx.add_equal_constraint(name_ty, value_type);

                if let Some(spec) = type_spec {
                    let spec_ty = spec.generate_constraints(ctx);
                    ctx.add_equal_constraint(value_type, spec_ty);
                }
            }
        }
    }
}

impl<'a> TypeConstraintGenerator<'a> for TypeSpec<'_> {
    type TypeResult = TypeOrVariable<'a>;

    fn generate_constraints(&self, _ctx: &mut TypeContext<'a>) -> Self::TypeResult {
        match self {
            // TODO: somewhere make the distinction between primitives and compounds, if necessary
            TypeSpec::Name(n) => match n.value.string {
                // TODO: expand to all different int types
                "int" => PartialType::Int.into(),
                "string" => PartialType::String.into(),
                "bool" => PartialType::Bool.into(),
                _ => unimplemented!(),
            }
        }
    }
}

impl<'a> TypeConstraintGenerator<'a> for File<'_, '_> {
    type TypeResult = ();

    fn generate_constraints(&self, ctx: &mut TypeContext<'a>) -> Self::TypeResult {
        for i in self.items {
            i.generate_constraints(ctx);
        }
    }
}

impl<'a> TypeConstraintGenerator<'a> for Item<'_, '_> {
    type TypeResult = ();

    fn generate_constraints(&self, ctx: &mut TypeContext<'a>) -> Self::TypeResult {
        match self {
            Item::Function(f) => f.generate_constraints(ctx)
        }
    }
}

impl<'a> TypeConstraintGenerator<'a> for Spanned<Function<'_, '_>> {
    type TypeResult = ();

    fn generate_constraints(&self, ctx: &mut TypeContext<'a>) -> Self::TypeResult {
        let Function { name, parameters, ret, block } = &self.value;
        let mut param_types = Vec::new_in(ctx.arena);

        // parameters
        for i in *parameters {
            let Parameter { name, type_spec } = i;

            let param_ty = ctx.type_of_name(name);
            let spec_ty = type_spec.generate_constraints(ctx);

            ctx.add_equal_constraint(param_ty, spec_ty);

            param_types.push(param_ty);
        }

        // return type
        let ret_ty_spec = if let Some(ret) = ret {
            let ret_ty_spec = ret.generate_constraints(ctx);
            ret_ty_spec
        } else {
            PartialType::Unit.into()
        };

        // gives this function type
        let function_type = PartialType::Function { params: &[], ret: ctx.alloc(ret_ty_spec) };

        // the function name has this type
        let name = ctx.type_of_name(name);
        ctx.add_equal_constraint(name, function_type);

        // and the block should return this type
        let block_ret_ty = block.generate_constraints(ctx);
        ctx.add_equal_constraint(block_ret_ty, ret_ty_spec);
    }
}
