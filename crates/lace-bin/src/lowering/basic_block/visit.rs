use crate::ast_metadata::MetadataId;
use crate::lice::Lice;
use crate::lowering::basic_block::BasicBlockBuilder;
use crate::lowering::lir;
use crate::lowering::lir::int::BinaryValue;
use crate::parser::ast;
use crate::parser::ast::{BinaryOp, UnaryOp};
use crate::typechecking::ty::Type;

impl lir::Value {
    /// ensures that a value is a place, by optionally putting it in a variable
    pub fn ensure_place(self, bbb: &mut BasicBlockBuilder) -> lir::Place {
        if let Self::Place(p) = self {
            p
        } else {
            bbb.emit_variable_assignment(self.into())
        }
    }
}

impl<'l, 'b, 't, 'a, 'n> BasicBlockBuilder<'l, 'b, 't, 'a, 'n> {
    fn visit_lit(&mut self, l: &ast::Lit, metadata: MetadataId) -> BinaryValue {
        match l {
            ast::Lit::Bool(true) => BinaryValue::TRUE,
            ast::Lit::Bool(false) => BinaryValue::FALSE,
            ast::Lit::Int(i) => {
                let ctx = self.ctx();
                let Type::Int { bits, signed } = ctx
                    .solved_types
                    .type_of_node(metadata, ctx.type_arena)
                    .unwrap_or_lice("resolve type failed")
                else {
                    lice!("type of int wasn't int");
                };

                BinaryValue::from_str(i, bits, signed)
                    .unwrap_or_lice("parse int shouldn't fail when parsed correctly")
            }
            ast::Lit::String(_) => todo!(),
        }
    }

    fn visit_expr(&mut self, e: &ast::Expr) -> lir::Value {
        match &e.value {
            ast::ExprKind::Lit(l) => self.visit_lit(l, e.metadata).into(),
            ast::ExprKind::If(_, _, _) => todo!(),
            ast::ExprKind::Block(_) => todo!(),
            ast::ExprKind::Ident(n) => self.ctx().lookup_variable(n.metadata).into(),
            ast::ExprKind::Paren(p) => self.visit_expr(p),
            ast::ExprKind::BinaryOp(op, left, right) => match &op.value {
                BinaryOp::Mul => {
                    let left = self.visit_expr(left);
                    let right = self.visit_expr(right);
                    self.emit_variable_assignment(lir::Expr::Mul(left, right))
                        .into()
                }
                BinaryOp::Div => {
                    let left = self.visit_expr(left);
                    let right = self.visit_expr(right);
                    self.emit_variable_assignment(lir::Expr::Div(left, right))
                        .into()
                }
                BinaryOp::Add => {
                    let left = self.visit_expr(left);
                    let right = self.visit_expr(right);
                    self.emit_variable_assignment(lir::Expr::Add(left, right))
                        .into()
                }
                BinaryOp::Sub => {
                    let left = self.visit_expr(left);
                    let right = self.visit_expr(right);
                    self.emit_variable_assignment(lir::Expr::Sub(left, right))
                        .into()
                }
                BinaryOp::LogicalAnd => {
                    // calculate left
                    let ans = self.visit_expr(left).ensure_place(self);

                    // generate the calculation of right
                    let (right, _) = self.child(|x| {
                        let right_ans = x.visit_expr(right);
                        // which, if executed, places it's result into ans
                        x.emit_assignment(ans.clone(), right_ans.into());
                    });

                    // if ans turns out to be false, continue here (the outcome
                    // of logical and is than false, which is already in `ans`)
                    let cont = self.ctx().fresh_label();

                    // terminate the current block with a switch
                    self.emit_terminator(
                        lir::Terminator::Switch {
                            on: ans.clone().into(),
                            // if the answer is true, execute the right block
                            // which will set ans to the desired value
                            cases: vec![(BinaryValue::TRUE, right.first_label())],
                            // otherwise, continue
                            otherwise: cont,
                        },
                        // hey, would you look at that. The next block we will generate lir
                        // in is where we would continue to
                        cont,
                    );

                    // make sure to also still insert the right code if we want to jump to it
                    self.insert_fresh_block(right);

                    ans.into()
                }
                BinaryOp::LogicalOr => {
                    // calculate left
                    let ans = self.visit_expr(left).ensure_place(self);
                    // generate the calculation of right
                    let (right, _) = self.child(|x| {
                        let right_ans = x.visit_expr(right);
                        // which, if executed, places it's result into ans
                        x.emit_assignment(ans.clone(), right_ans.into());
                    });

                    // if ans turns out to be false, continue here (the outcome
                    // of logical and is than false, which is already in `ans`)
                    let cont = self.ctx().fresh_label();

                    // terminate the current block with a switch
                    self.emit_terminator(
                        lir::Terminator::Switch {
                            on: ans.clone().into(),
                            // if the answer is true, execute the right block
                            // which will set ans to the desired value
                            cases: vec![(BinaryValue::FALSE, right.first_label())],
                            // otherwise, continue
                            otherwise: cont,
                        },
                        // hey, would you look at that. The next block we will generate lir
                        // in is where we would continue to
                        cont,
                    );

                    // make sure to also still insert the right code if we want to jump to it
                    self.insert_fresh_block(right);

                    ans.into()
                }
                BinaryOp::Gt => {
                    let left = self.visit_expr(left);
                    let right = self.visit_expr(right);
                    self.emit_variable_assignment(lir::Expr::Gt(left, right))
                        .into()
                }
                BinaryOp::Gte => {
                    let left = self.visit_expr(left);
                    let right = self.visit_expr(right);
                    self.emit_variable_assignment(lir::Expr::Gte(left, right))
                        .into()
                }
                BinaryOp::Lt => {
                    let left = self.visit_expr(left);
                    let right = self.visit_expr(right);
                    self.emit_variable_assignment(lir::Expr::Lt(left, right))
                        .into()
                }
                BinaryOp::Lte => {
                    let left = self.visit_expr(left);
                    let right = self.visit_expr(right);
                    self.emit_variable_assignment(lir::Expr::Lte(left, right))
                        .into()
                }
                BinaryOp::Eq => {
                    let left = self.visit_expr(left);
                    let right = self.visit_expr(right);
                    self.emit_variable_assignment(lir::Expr::Eq(left, right))
                        .into()
                }
                BinaryOp::Neq => {
                    let left = self.visit_expr(left);
                    let right = self.visit_expr(right);
                    self.emit_variable_assignment(lir::Expr::Neq(left, right))
                        .into()
                }
            },
            ast::ExprKind::UnaryOp(op, inner) => {
                let res = self.visit_expr(inner);
                match &op.value {
                    UnaryOp::Not => self.emit_variable_assignment(lir::Expr::Not(res)).into(),
                    UnaryOp::Neg => self.emit_variable_assignment(lir::Expr::Neg(res)).into(),
                }
            }
            ast::ExprKind::Tuple(_) => todo!(),
            ast::ExprKind::Call(_, _) => todo!(),
        }
    }

    fn visit_statement(&mut self, v: &ast::Statement) {
        match v {
            ast::Statement::Expr(e) => {
                let _ = self.visit_expr(e);
            }
            ast::Statement::Let(loc, _, expr) => {
                let res = self.visit_expr(expr);
                let var = self.ctx().lookup_variable(loc.metadata);

                self.emit_assignment(var.into(), res.into());
            }
        }
    }

    pub(in crate::lowering) fn visit_block(&mut self, f: &ast::Block) {
        for stmt in f.stmts {
            self.visit_statement(stmt)
        }
    }
}
