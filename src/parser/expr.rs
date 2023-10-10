use crate::lexer::token::Token;
use crate::parser::ast::{Expr, ExprKind, Ident, Lit};
use crate::parser::error::{ParseError, ParseResult};
use crate::parser::span::{Spanned, WithSpan};
use crate::parser::Parser;
use bumpalo::collections::Vec;

impl<'s, 'a, 'e> Parser<'s, 'a, 'e> {
    // TODO: Proper error handling
    // TODO: Function calls
    pub fn expr(&mut self) -> ParseResult<'s, Expr<'s, 'a>> {
        self.disjunction()
    }

    fn disjunction(&mut self) -> ParseResult<'s, Expr<'s, 'a>> {
        let mut left = self.conjunction()?;
        while self.accept_optional(tok![||])?.is_some() {
            let right = self.conjunction()?;
            let span = self.spans.merge(&left, &right);
            left = ExprKind::LogicalOr(self.alloc(left), self.alloc(right)).with_span(span)
        }
        Ok(left)
    }

    // conjunction = conjunction 'and' inversion
    //             | inversion
    fn conjunction(&mut self) -> ParseResult<'s, Expr<'s, 'a>> {
        let mut left = self.comparison()?;
        while self.accept_optional(tok![&&])?.is_some() {
            let right = self.comparison()?;
            let span = self.spans.merge(&left, &right);
            left = ExprKind::LogicalAnd(self.alloc(left), self.alloc(right)).with_span(span);
        }
        Ok(left)
    }

    fn comparison(&mut self) -> ParseResult<'s, Expr<'s, 'a>> {
        let left = self.inversion()?;
        let tokens: &[(_, fn(_, _) -> _)] = &[
            (tok![==], ExprKind::Eq),
            (tok![!=], ExprKind::Neq),
            (tok![>=], ExprKind::Gte),
            (tok![<=], ExprKind::Lte),
            (tok![>], ExprKind::Gt),
            (tok![<], ExprKind::Lt),
        ];
        for (t, f) in tokens {
            if self.accept_optional(*t)?.is_some() {
                let right = self.inversion()?;
                let span = self.spans.merge(&left, &right);
                return Ok(f(self.alloc(left), self.alloc(right)).with_span(span));
            }
        }
        Ok(left)
    }

    // inversion = '!' inversion
    //           | sum
    // TODO: this should go to comparison
    fn inversion(&mut self) -> ParseResult<'s, Expr<'s, 'a>> {
        if let Some(span) = self.accept_optional(tok![!])? {
            let arg = self.inversion()?;
            let span = self.spans.store_merged(span, &arg);
            Ok(ExprKind::Not(self.alloc(arg)).with_span(span))
        } else {
            self.sum()
        }
    }

    // sum = sum '+' term
    //     | sum '-' term
    //     | term
    fn sum(&mut self) -> ParseResult<'s, Expr<'s, 'a>> {
        let mut left = self.term()?;
        loop {
            if self.accept_optional(tok![+])?.is_some() {
                let right = self.term()?;
                let span = self.spans.merge(&left, &right);
                left = ExprKind::Add(self.alloc(left), self.alloc(right)).with_span(span);
            } else if self.accept_optional(tok![-])?.is_some() {
                let right = self.term()?;
                let span = self.spans.merge(&left, &right);
                left = ExprKind::Sub(self.alloc(left), self.alloc(right)).with_span(span);
            } else {
                return Ok(left);
            }
        }
    }

    // term = term '*' factor
    //      | term '/' factor
    //      | factor
    fn term(&mut self) -> ParseResult<'s, Expr<'s, 'a>> {
        let mut left = self.factor()?;
        loop {
            if self.accept_optional(tok![*])?.is_some() {
                let right = self.factor()?;
                let span = self.spans.merge(&left, &right);
                left = ExprKind::Mul(self.alloc(left), self.alloc(right)).with_span(span);
            } else if self.accept_optional(tok![/])?.is_some() {
                let right = self.factor()?;
                let span = self.spans.merge(&left, &right);
                left = ExprKind::Div(self.alloc(left), self.alloc(right)).with_span(span);
            } else {
                return Ok(left);
            }
        }
    }

    // factor = '-' atom
    //        | atom
    fn factor(&mut self) -> ParseResult<'s, Expr<'s, 'a>> {
        if let Some(span) = self.accept_optional(tok![-])? {
            let arg = self.call_expr()?;
            let span = self.spans.store_merged(span, &arg);
            Ok(ExprKind::Neg(self.alloc(arg)).with_span(span))
        } else {
            self.call_expr()
        }
    }

    fn call_args(&mut self) -> ParseResult<'s, Spanned<&'a [Expr<'s, 'a>]>> {
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

    fn call_expr(&mut self) -> ParseResult<'s, Expr<'s, 'a>> {
        let atom = self.atom()?;

        if self.peek_is(Token::RoundLeft)? {
            let args = self.call_args()?;

            let span = self.spans.merge(&atom, &args);
            Ok(ExprKind::Call(self.alloc(atom), args).with_span(span))
        } else {
            Ok(atom)
        }
    }

    fn atom(&mut self) -> ParseResult<'s, Expr<'s, 'a>> {
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
                return self.ectx.fatal(
                    ParseError::Expected {
                        expected: "an expression".into(),
                        got: t.to_string(),
                        span: raw_span,
                    },
                    self.source(),
                )
            }
        };

        Ok(expr)
    }

    pub(super) fn ident(&mut self) -> ParseResult<'s, Spanned<Ident<'s>>> {
        let (token, name_span) = self.next()?;
        let Token::Ident(name) = token else {
            return self.ectx.fatal(
                ParseError::Expected {
                    expected: "an identifier".into(),
                    got: token.to_string(),
                    span: name_span,
                },
                self.source(),
            );
        };

        Ok(Ident { string: name }.with_span(self.spans.store(name_span)))
    }

    fn paren(&mut self) -> ParseResult<'s, Expr<'s, 'a>> {
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
