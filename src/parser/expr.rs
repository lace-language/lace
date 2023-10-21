use crate::lexer::token::Token;
use crate::parser::ast::{Expr, ExprKind, Ident, Lit};
use crate::parser::error::{ParseError, ParseResult};
use crate::parser::span::{Spanned, WithSpan};
use crate::parser::Parser;
use bumpalo::collections::Vec;

use super::ast::{BinOp, UnaryOp};
use super::span::Span;

impl<'s, 'a> Parser<'s, 'a> {
    // TODO: Proper error handling
    // TODO: Function calls
    pub fn expr(&mut self) -> ParseResult<Expr<'s, 'a>> {
        self.disjunction()
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

    // disjunction = disjunction '&&' conjunction
    //             | conjunction
    fn disjunction(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let mut left = self.conjunction()?;
        while self.accept_optional(tok![||])?.is_some() {
            let right = self.conjunction()?;
            let span = self.spans.merge(&left, &right);
            left = ExprKind::BinOp(BinOp::LogicalOr, self.alloc(left), self.alloc(right))
                .with_span(span)
        }
        Ok(left)
    }

    // conjunction = conjunction '&&' inversion
    //             | inversion
    fn conjunction(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let mut left = self.comparison()?;
        while self.accept_optional(tok![&&])?.is_some() {
            let right = self.comparison()?;
            let span = self.spans.merge(&left, &right);
            left = ExprKind::BinOp(BinOp::LogicalAnd, self.alloc(left), self.alloc(right))
                .with_span(span);
        }
        Ok(left)
    }

    fn comparison(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let left = self.sum()?;
        if let Some((op, _span)) = self.accept_any([
            (tok![==], BinOp::Eq),
            (tok![!=], BinOp::Neq),
            (tok![>=], BinOp::Gte),
            (tok![<=], BinOp::Lte),
            (tok![>], BinOp::Gt),
            (tok![<], BinOp::Lt),
        ])? {
            let right = self.sum()?;
            let span = self.spans.merge(&left, &right);
            return Ok(ExprKind::BinOp(op, self.alloc(left), self.alloc(right)).with_span(span));
        } else {
            Ok(left)
        }
    }

    // sum = sum '+' term
    //     | sum '-' term
    //     | term
    fn sum(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let mut left = self.term()?;
        while let Some((op, _span)) =
            self.accept_any([(tok![+], BinOp::Add), (tok![-], BinOp::Sub)])?
        {
            let right = self.term()?;
            let span = self.spans.merge(&left, &right);
            left = ExprKind::BinOp(op, self.alloc(left), self.alloc(right)).with_span(span);
        }
        Ok(left)
    }

    // term = term '*' factor
    //      | term '/' factor
    //      | factor
    fn term(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let mut left = self.factor()?;
        while let Some((op, _span)) =
            self.accept_any([(tok![*], BinOp::Mul), (tok![/], BinOp::Div)])?
        {
            let right = self.factor()?;
            let span = self.spans.merge(&left, &right);
            left = ExprKind::BinOp(op, self.alloc(left), self.alloc(right)).with_span(span);
        }
        Ok(left)
    }

    // factor = '-' factor
    //        | '!' factor
    //        | atom
    fn factor(&mut self) -> ParseResult<Expr<'s, 'a>> {
        if let Some((op, span)) =
            self.accept_any([(tok![-], UnaryOp::Neg), (tok![!], UnaryOp::Not)])?
        {
            let arg = self.factor()?;
            let span = self.spans.store_merged(span, &arg);
            Ok(ExprKind::UnaryOp(op, self.alloc(arg)).with_span(span))
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
