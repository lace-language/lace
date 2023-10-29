use crate::lexer::token::Token;
use crate::parser::ast::{Expr, ExprKind, Ident, Lit};
use crate::parser::error::{ParseError, ParseResult};
use crate::parser::precedence::Compatibility;
use crate::parser::Parser;
use crate::syntax_id::{Identified, WithNodeId};
use bumpalo::collections::Vec;

use super::ast::{BinaryOp, UnaryOp};

impl<'s, 'a> Parser<'s, 'a> {
    pub fn expr(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let mut lhs = self.unary()?;

        while let Some(operator) = self.peek_binary_operator()? {
            lhs = self.binary_expr_rhs(lhs, operator)?;
        }

        Ok(lhs)
    }

    fn binary_expr(
        &mut self,
        mut lhs: Expr<'s, 'a>,
        last_operator: &Identified<BinaryOp>,
    ) -> ParseResult<Expr<'s, 'a>> {
        while let Some(operator) = self.peek_binary_operator()? {
            // now we look at the precedence and associativity of our operator,
            // and determine whether we should continue parsing more expression
            // or return back up
            match operator.compatibility(&last_operator.value) {
                Compatibility::Continue => lhs = self.binary_expr_rhs(lhs, operator)?,
                Compatibility::Stop => break,
                Compatibility::Incompatible => {
                    // we peeked already that an operator is coming, we just need to know its span
                    let (_, span) = self.next()?;

                    return Err(ParseError::IncompatibleBinaryOp {
                        left_operator: last_operator.value.to_string(),
                        right_operator: operator.to_string(),
                        left_operator_span: self.spans.get(last_operator.node_id),
                        right_operator_span: span,
                    });
                }
            }
        }

        Ok(lhs)
    }

    fn binary_expr_rhs(
        &mut self,
        lhs: Expr<'s, 'a>,
        operator: BinaryOp,
    ) -> Result<Expr<'s, 'a>, ParseError> {
        // we peeked already that an operator is coming, we just need to know its span and progress the parser
        let (_, span) = self.next()?;
        let span = self.spans.store(span);
        let operator = operator.with_node_id(span);

        // if we continue,
        // parse the left hand side of the next expression
        let new_lhs = self.unary()?;
        // and now parse a next expression, with this newly parsed
        // left hand side. Maybe, it parses an operator too many,
        // in that case it returns it and we cache it in `maybe_operator`
        // and use it next if we determine we should not return it ourselves
        let rhs = self.binary_expr(new_lhs, &operator)?;

        // make an expression, and loop
        let span = self.spans.merge(&lhs, &rhs);

        Ok(ExprKind::BinaryOp(operator, self.alloc(lhs), self.alloc(rhs)).with_node_id(span))
    }

    fn peek_binary_operator(&mut self) -> ParseResult<Option<BinaryOp>> {
        self.peek_any([
            (tok![*], BinaryOp::Mul),
            (tok![/], BinaryOp::Div),
            (tok![+], BinaryOp::Add),
            (tok![-], BinaryOp::Sub),
            (tok![&&], BinaryOp::LogicalAnd),
            (tok![||], BinaryOp::LogicalOr),
            (tok![>], BinaryOp::Gt),
            (tok![>=], BinaryOp::Gte),
            (tok![<], BinaryOp::Lt),
            (tok![<=], BinaryOp::Lte),
            (tok![==], BinaryOp::Eq),
            (tok![!=], BinaryOp::Neq),
        ])
    }

    fn unary(&mut self) -> ParseResult<Expr<'s, 'a>> {
        if let Some((op, span)) =
            self.accept_any([(tok![-], UnaryOp::Neg), (tok![!], UnaryOp::Not)])?
        {
            let operator_span = self.spans.store(span);
            let arg = self.unary()?;
            let span = self.spans.store_merged(span, &arg);
            Ok(
                ExprKind::UnaryOp(op.with_node_id(operator_span), self.alloc(arg))
                    .with_node_id(span),
            )
        } else {
            self.call_expr()
        }
    }

    fn call_args(&mut self) -> ParseResult<Identified<&'a [Expr<'s, 'a>]>> {
        let start_span = self.accept_required(Token::RoundLeft)?;

        // empty parameters list
        if let Some(end_span) = self.accept_optional(Token::RoundRight)? {
            // yes, this is really necessary.
            let a: &[_] = &[];
            return Ok(a.with_node_id(self.spans.store(start_span.merge(&end_span))));
        }

        let mut parameters = Vec::new_in(self.arena);
        parameters.push(self.expr()?);

        while self.accept_optional(tok![,])?.is_some() {
            // for trailing comma
            if let Some(end_span) = self.accept_optional(Token::RoundRight)? {
                return Ok(parameters
                    .into_bump_slice()
                    .with_node_id(self.spans.store(start_span.merge(&end_span))));
            }

            parameters.push(self.expr()?);
        }

        let end_span = self.accept_required(Token::RoundRight)?;

        Ok(parameters
            .into_bump_slice()
            .with_node_id(self.spans.store(start_span.merge(&end_span))))
    }

    fn call_expr(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let mut expr = self.atom()?;

        while self.peek_is(Token::RoundLeft)? {
            let args = self.call_args()?;
            let span = self.spans.merge(&expr, &args);
            expr = ExprKind::Call(self.alloc(expr), args).with_node_id(span);
        }

        Ok(expr)
    }

    fn atom(&mut self) -> ParseResult<Expr<'s, 'a>> {
        if self.peek_is(Token::RoundLeft)? {
            return self.paren();
        } else if self.peek_is(Token::CurlyLeft)? {
            let expr = self.block()?;
            let span = expr.node_id;
            return Ok(ExprKind::Block(self.alloc(expr)).with_node_id(span));
        }

        if self.peek_is(tok![if])? {
            return self.if_else();
        }

        let (token, raw_span) = self.next()?;
        let span = self.spans.store(raw_span);
        let expr = match token {
            Token::Ident(s) => {
                ExprKind::Ident(Ident { string: s }.with_node_id(span)).with_node_id(span)
            }
            Token::False => ExprKind::Lit(Lit::Bool(false)).with_node_id(span),
            Token::True => ExprKind::Lit(Lit::Bool(true)).with_node_id(span),
            Token::String(s) => ExprKind::Lit(Lit::String(s)).with_node_id(span),
            Token::Int(i) => ExprKind::Lit(Lit::Int(i)).with_node_id(span),
            t => {
                return Err(ParseError::Expected {
                    expected: "an expression".into(),
                    got: t.to_string(),
                    span: raw_span,
                });
            }
        };

        Ok(expr)
    }

    pub(super) fn ident(&mut self) -> ParseResult<Identified<Ident<'s>>> {
        let (token, name_span) = self.next()?;
        let Token::Ident(name) = token else {
            return Err(ParseError::Expected {
                expected: "an identifier".into(),
                got: token.to_string(),
                span: name_span,
            });
        };

        Ok(Ident { string: name }.with_node_id(self.spans.store(name_span)))
    }

    fn paren(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let start_span = self.accept_required(Token::RoundLeft)?;

        if let Some(end_span) = self.accept_optional(Token::RoundRight)? {
            return Ok(
                ExprKind::Tuple(&[]).with_node_id(self.spans.store(start_span.merge(&end_span)))
            );
        }

        let expr = self.expr()?;

        if let Some(end_span) = self.accept_optional(Token::RoundRight)? {
            let expr = self.alloc(expr);
            return Ok(
                ExprKind::Paren(expr).with_node_id(self.spans.store(start_span.merge(&end_span)))
            );
        }

        let mut vec = Vec::new_in(self.arena);
        vec.push(expr);

        while self.accept_optional(tok![,])?.is_some() {
            if let Some(end_span) = self.accept_optional(Token::RoundRight)? {
                let slice = vec.into_bump_slice();
                return Ok(ExprKind::Tuple(slice)
                    .with_node_id(self.spans.store(start_span.merge(&end_span))));
            }
            vec.push(self.expr()?);
        }

        let end_span = self.accept_required(Token::RoundRight)?;
        let slice = vec.into_bump_slice();

        Ok(ExprKind::Tuple(slice).with_node_id(self.spans.store(start_span.merge(&end_span))))
    }
}
