use std::cmp::Ordering;

use crate::ice::Ice;
use crate::lexer::token::Token;
use crate::parser::ast::{Expr, ExprKind, Ident, Lit};
use crate::parser::error::{ParseError, ParseResult};
use crate::parser::span::{Spanned, WithSpan};
use crate::parser::Parser;
use bumpalo::collections::Vec;

use super::ast::{BinaryOp, UnaryOp};
use super::span::Span;

impl ToString for BinaryOp {
    fn to_string(&self) -> String {
        match self {
            BinaryOp::Mul => "+",
            BinaryOp::Div => "/",
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::LogicalAnd => "&&",
            BinaryOp::LogicalOr => "||",
            BinaryOp::Gt => ">",
            BinaryOp::Gte => ">=",
            BinaryOp::Lt => "<",
            BinaryOp::Lte => "<=",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
        }
        .to_string()
    }
}

/// Precedence of binary operators
///
/// The order of the variants of this enum is significant, because it defines
/// derived implementation of `PartialOrd`/`Ord`.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,

    /// The precedence of logical disjunction.
    Disjunction,

    /// The precedence of logical conjunction.
    Conjunction,

    /// The precedence of comparison operators.
    Comparison,

    /// The precedence of addition and subtraction.
    AddSub,

    /// The precedence of multiplication and division.
    MulDiv,
}

/// Associativity of binary operator.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Associativity {
    /// Left associativity.
    ///
    /// `<x> o <y> o <z>` is parsed as `(<x> o <y>) o <z>`.
    Left,

    /// Right associativity.
    ///
    /// `<x> o <y> o <z>` is parsed as `<x> o (<y> o <z>)`.
    #[allow(unused)]
    Right,

    /// Incompatible operators
    Not,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Compatibility {
    Continue,
    Stop,
    Incompatible,
}

impl BinaryOp {
    fn precedence(&self) -> Precedence {
        match self {
            Self::LogicalOr => Precedence::Disjunction,
            Self::LogicalAnd => Precedence::Conjunction,
            Self::Eq | Self::Neq | Self::Lt | Self::Lte | Self::Gt | Self::Gte => {
                Precedence::Comparison
            }
            Self::Mul | Self::Div => Precedence::MulDiv,
            Self::Add | Self::Sub => Precedence::AddSub,
        }
    }
}

impl Precedence {
    fn associativity(&self) -> Associativity {
        match self {
            Self::Disjunction | Self::Conjunction | Self::AddSub | Self::MulDiv => {
                Associativity::Left
            }
            Self::Comparison => Associativity::Not,
            Self::Lowest => Associativity::Not,
        }
    }

    pub fn compatibility(&self, other: &Self) -> Compatibility {
        match (self.cmp(other), self.associativity()) {
            (Ordering::Less, _) => Compatibility::Stop,
            (Ordering::Greater, _) => Compatibility::Continue,
            (Ordering::Equal, Associativity::Not) => Compatibility::Incompatible,
            (Ordering::Equal, Associativity::Right) => Compatibility::Continue,
            (Ordering::Equal, Associativity::Left) => Compatibility::Stop,
        }
    }
}

impl<'s, 'a> Parser<'s, 'a> {
    // TODO: Proper error handling
    // TODO: Function calls
    pub fn expr(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let lhs = self.unary()?;
        let (expr, maybe_op) = self.binary_expr(lhs, None)?;
        if maybe_op.is_some() {
            ice!("there was an binary operator left at the lowest precedence level");
        }
        Ok(expr)
    }

    fn accept_any<T>(
        &mut self,
        tokens: impl IntoIterator<Item = (Token<'s>, T)>,
    ) -> ParseResult<Option<(T, Span)>> {
        for (t, op) in tokens {
            if let Some(span) = self.accept_optional(t)? {
                return Ok(Some((op, span)));
            }
        }
        Ok(None)
    }

    fn binary_expr(
        &mut self,
        mut lhs: Expr<'s, 'a>,
        last_operator: Option<&Spanned<BinaryOp>>,
    ) -> ParseResult<(Expr<'s, 'a>, Option<Spanned<BinaryOp>>)> {
        let precedence_bound = match last_operator {
            Some(last_operator) => last_operator.precedence(),
            None => Precedence::Lowest,
        };

        // maybe we have an operator cached already,
        // somewhere in the parse loop. At the start, we do not however.
        let mut maybe_operator = None;

        loop {
            // if we have a cached operator: take it
            // if we don't, parse a new one!
            // else, this is the end of the expression
            let operator = if let Some(operator) = maybe_operator.take() {
                operator
            } else if let Some(operator) = self.binary_operator()? {
                operator
            } else {
                break;
            };

            // now we look at the precedence and associativity of our operator,
            // and determine wether we should continue parsing more expression
            // or return back up
            match operator.value.precedence().compatibility(&precedence_bound) {
                Compatibility::Continue => {
                    // if we continue,
                    // parse the left hand side of the next expression
                    let new_lhs = self.unary()?;
                    // and now parse a next expression, with this newly parsed
                    // left hand side. Maybe, it parses an operator too many,
                    // in that case it returns it and we cache it in `maybe_operator`
                    // and use it next if we determine we should not return it ourselves
                    let (rhs, maybe_new_op) = self.binary_expr(new_lhs, Some(&operator))?;
                    maybe_operator = maybe_new_op;

                    // make an expression, and loop
                    let span = self.spans.merge(&lhs, &rhs);
                    lhs = ExprKind::BinaryOp(operator, self.alloc(lhs), self.alloc(rhs))
                        .with_span(span);
                }
                Compatibility::Stop => {
                    return Ok((lhs, Some(operator)));
                }
                Compatibility::Incompatible => {
                    let last_operator = last_operator
                        .ice("we can never have incompatibility if there was no last operator");

                    return Err(ParseError::IncompatibleBinaryOp {
                        left_operator: last_operator.value.to_string(),
                        right_operator: operator.value.to_string(),
                        left_operator_span: self.spans.get(last_operator.span),
                        right_operator_span: self.spans.get(operator.span),
                    });
                }
            }
        }

        Ok((lhs, None))
    }

    fn binary_operator(&mut self) -> ParseResult<Option<Spanned<BinaryOp>>> {
        let Some((operator, span)) = self.accept_any([
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
        ])?
        else {
            return Ok(None);
        };

        Ok(Some(operator.with_span(self.spans.store(span))))
    }

    fn unary(&mut self) -> ParseResult<Expr<'s, 'a>> {
        if let Some((op, span)) =
            self.accept_any([(tok![-], UnaryOp::Neg), (tok![!], UnaryOp::Not)])?
        {
            let operator_span = self.spans.store(span);
            let arg = self.unary()?;
            let span = self.spans.store_merged(span, &arg);
            Ok(ExprKind::UnaryOp(op.with_span(operator_span), self.alloc(arg)).with_span(span))
        } else {
            self.call_expr()
        }
    }

    fn call_args(&mut self) -> ParseResult<Spanned<&'a [Expr<'s, 'a>]>> {
        let start_span = self.accept_required(Token::RoundLeft)?;

        // empty parameters list
        if let Some(end_span) = self.accept_optional(Token::RoundRight)? {
            // yes, this is really necessary.
            let a: &[_] = &[];
            return Ok(a.with_span(self.spans.store(start_span.merge(&end_span))));
        }

        let mut parameters = Vec::new_in(self.arena);
        parameters.push(self.expr()?);

        while self.accept_optional(tok![,])?.is_some() {
            // for trailing comma
            if let Some(end_span) = self.accept_optional(Token::RoundRight)? {
                return Ok(parameters
                    .into_bump_slice()
                    .with_span(self.spans.store(start_span.merge(&end_span))));
            }

            parameters.push(self.expr()?);
        }

        let end_span = self.accept_required(Token::RoundRight)?;

        Ok(parameters
            .into_bump_slice()
            .with_span(self.spans.store(start_span.merge(&end_span))))
    }

    fn call_expr(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let mut expr = self.atom()?;

        while self.peek_is(Token::RoundLeft)? {
            let args = self.call_args()?;
            let span = self.spans.merge(&expr, &args);
            expr = ExprKind::Call(self.alloc(expr), args).with_span(span);
        }

        Ok(expr)
    }

    fn atom(&mut self) -> ParseResult<Expr<'s, 'a>> {
        if self.peek_is(Token::RoundLeft)? {
            return self.paren();
        } else if self.peek_is(Token::CurlyLeft)? {
            let expr = self.block()?;
            let span = expr.span;
            return Ok(ExprKind::Block(self.alloc(expr)).with_span(span));
        }

        if self.peek_is(tok![if])? {
            return self.if_else();
        }

        let (token, raw_span) = self.next()?;
        let span = self.spans.store(raw_span);
        let expr = match token {
            Token::Ident(s) => ExprKind::Ident(Ident { string: s }).with_span(span),
            Token::False => ExprKind::Lit(Lit::Bool(false)).with_span(span),
            Token::True => ExprKind::Lit(Lit::Bool(true)).with_span(span),
            Token::String(s) => ExprKind::Lit(Lit::String(s)).with_span(span),
            Token::Int(i) => ExprKind::Lit(Lit::Int(i)).with_span(span),
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

    pub(super) fn ident(&mut self) -> ParseResult<Spanned<Ident<'s>>> {
        let (token, name_span) = self.next()?;
        let Token::Ident(name) = token else {
            return Err(ParseError::Expected {
                expected: "an identifier".into(),
                got: token.to_string(),
                span: name_span,
            });
        };

        Ok(Ident { string: name }.with_span(self.spans.store(name_span)))
    }

    fn paren(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let start_span = self.accept_required(Token::RoundLeft)?;

        if let Some(end_span) = self.accept_optional(Token::RoundRight)? {
            return Ok(
                ExprKind::Tuple(&[]).with_span(self.spans.store(start_span.merge(&end_span)))
            );
        }

        let expr = self.expr()?;

        if let Some(end_span) = self.accept_optional(Token::RoundRight)? {
            let expr = self.alloc(expr);
            return Ok(
                ExprKind::Paren(expr).with_span(self.spans.store(start_span.merge(&end_span)))
            );
        }

        let mut vec = Vec::new_in(self.arena);
        vec.push(expr);

        while self.accept_optional(tok![,])?.is_some() {
            if let Some(end_span) = self.accept_optional(Token::RoundRight)? {
                let slice = vec.into_bump_slice();
                return Ok(
                    ExprKind::Tuple(slice).with_span(self.spans.store(start_span.merge(&end_span)))
                );
            }
            vec.push(self.expr()?);
        }

        let end_span = self.accept_required(Token::RoundRight)?;
        let slice = vec.into_bump_slice();

        Ok(ExprKind::Tuple(slice).with_span(self.spans.store(start_span.merge(&end_span))))
    }
}
