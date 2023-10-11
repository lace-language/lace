use std::cmp::Ordering;
use crate::lexer::Token;
use crate::parser::ast::{Expr, ExprKind, Ident, Lit};
use crate::parser::error::{ParseError, ParseResult};
use crate::parser::span::{Spanned, WithSpan, Span};
use crate::parser::Parser;
use bumpalo::collections::Vec;

/// A binary operator.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,

    IsEQ,
    IsNE,
    IsLT,
    IsLE,
    IsGT,
    IsGE,

    Con,
    Dis,
}

impl BinOp {
    /// The precedence of this operator.
    pub fn prec(&self) -> Prec {
        match self {
            Self::Add | Self::Sub => Prec::AddSub,
            Self::Mul | Self::Div => Prec::MulDiv,
            Self::IsEQ | Self::IsNE
                | Self::IsLT | Self::IsLE
                | Self::IsGT | Self::IsGE
                => Prec::Compare,
            Self::Con => Prec::Con,
            Self::Dis => Prec::Dis,
        }
    }

    /// The token for this operator.
    pub fn token(&self) -> Token<'static> {
        match self {
            Self::Add => tok![+],
            Self::Sub => tok![-],
            Self::Mul => tok![*],
            Self::Div => tok![/],
            Self::IsEQ => tok![==],
            Self::IsNE => tok![!=],
            Self::IsLT => tok![<],
            Self::IsLE => tok![<=],
            Self::IsGT => tok![>],
            Self::IsGE => tok![>=],
            Self::Con => tok![&&],
            Self::Dis => tok![||],
        }
    }

    /// Construct a new [`ExprKind`] of this operator.
    pub fn into_expr<'s, 'a>(
        self,
        lhs: &'a Expr<'s, 'a>,
        rhs: &'a Expr<'s, 'a>,
    ) -> ExprKind<'s, 'a> {
        (match self {
            Self::Add => ExprKind::Add,
            Self::Sub => ExprKind::Sub,
            Self::Mul => ExprKind::Mul,
            Self::Div => ExprKind::Div,
            Self::IsEQ => ExprKind::Eq,
            Self::IsNE => ExprKind::Neq,
            Self::IsLT => ExprKind::Lt,
            Self::IsLE => ExprKind::Lte,
            Self::IsGT => ExprKind::Gt,
            Self::IsGE => ExprKind::Gte,
            Self::Con => ExprKind::LogicalAnd,
            Self::Dis => ExprKind::LogicalOr,
        })(lhs, rhs)
    }
}

/// Operator precedence.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Prec {
    /// The precedence of logical disjunction.
    Dis,

    /// The precedence of logical conjunction.
    Con,

    /// The precedence of comparison operators.
    Compare,

    /// The precedence of addition and subtraction.
    AddSub,

    /// The precedence of multiplication and division.
    MulDiv,
}

impl Prec {
    /// The associativity for operators at this precedence.
    ///
    /// If [`None`] is returned, then operators at this precedence cannot be chained to each other.
    pub fn assoc(&self) -> Option<Assoc> {
        match self {
            Self::Dis => Some(Assoc::Left),
            Self::Con => Some(Assoc::Left),
            Self::Compare => None,
            Self::AddSub => Some(Assoc::Left),
            Self::MulDiv => Some(Assoc::Left),
        }
    }

    /// Determine the associativity between two operators.
    ///
    /// Given two operators `a` and `b` in the expression `x <a> y <b> z`, this function returns
    /// [`Assoc::Left`] if `y` binds to `a` or [`Assoc::Right`] if `y` binds to `b`.  If `a` and
    /// `b` cannot be chained, [`None`] will be returned instead.
    pub fn cmp(lhs: Self, rhs: Self) -> Option<Assoc> {
        match PartialOrd::partial_cmp(&lhs, &rhs)? {
            Ordering::Less => Some(Assoc::Right),
            Ordering::Equal => lhs.assoc(),
            Ordering::Greater => Some(Assoc::Left),
        }
    }
}

impl PartialOrd for Prec {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let (lhs, rhs) = (*self, *other);
        Some(Ord::cmp(&(lhs as usize), &(rhs as usize)))
    }
}

/// The associativity of an operator.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Assoc {
    /// Left associativity.
    ///
    /// `<x> o <y> o <z>` is parsed as `(<x> o <y>) o <z>`.
    Left,

    /// Right associativity.
    ///
    /// `<x> o <y> o <z>` is parsed as `<x> o (<y> o <z>)`.
    Right,
}

impl<'s, 'a> Parser<'s, 'a> {
    // TODO: Proper error handling
    pub fn expr(&mut self) -> ParseResult<Expr<'s, 'a>> {
        self.prec_expr(None)
    }

    fn prec_expr(
        &mut self,
        prev: Option<(BinOp, Span)>,
    ) -> ParseResult<Expr<'s, 'a>> {
        // The expression parsed thus far.
        let mut expr = self.unary_expr()?;

        // Try extending into a larger binary operation.
        while let Some(next) = self.binop(prev)? {
            // Get the right-hand side of this operation.
            let rhs = self.prec_expr(Some(next))?;

            // Construct the new binary operation.
            let span = self.spans.merge(&expr, &rhs);
            let [lhs, rhs] = [expr, rhs].map(|x| self.alloc(x));
            expr = next.0.into_expr(lhs, rhs).with_span(span);
        }

        Ok(expr)
    }

    /// Determine the associativity between two binary operators.
    fn binop(&mut self, lhs: Option<(BinOp, Span)>) -> ParseResult<Option<(BinOp, Span)>> {
        let Some(rhs) = self.peek_binop()? else { return Ok(None) };
        let Some(lhs) = lhs else {
            let (_, rhs_span) = self.next()?;
            return Ok(Some((rhs, rhs_span)));
        };

        if let Some(assoc) = Prec::cmp(lhs.0.prec(), rhs.prec()) {
            // The operators are compatible, succeed.
            if assoc == Assoc::Right {
                let (_, rhs_span) = self.next()?;
                return Ok(Some((rhs, rhs_span)));
            } else {
                return Ok(None);
            }
        }

        let (_, rhs_span) = self.next()?;
        Err(ParseError::IncompatBinOp {
            lhs: lhs.0.token().to_string(),
            rhs: rhs.token().to_string(),
            lhs_span: lhs.1,
            rhs_span,
        })
    }

    /// Peek to find a binary operator.
    fn peek_binop(&mut self) -> ParseResult<Option<BinOp>> {
        let operators = [
            BinOp::Add,
            BinOp::Sub,
            BinOp::Mul,
            BinOp::Div,
            BinOp::IsEQ,
            BinOp::IsNE,
            BinOp::IsLT,
            BinOp::IsLE,
            BinOp::IsGT,
            BinOp::IsGE,
            BinOp::Con,
            BinOp::Dis,
        ];

        let Some(token) = self.peek()? else { return Ok(None) };
        Ok(operators.into_iter().find(|o| o.token() == token))
    }

    /// Parse a unary expression.
    fn unary_expr(&mut self) -> ParseResult<Expr<'s, 'a>> {
        // Try parsing a prefix operation.
        let prefix_ops: [(_, fn(_) -> _); 2] = [
            (tok![-], ExprKind::Neg),
            (tok![!], ExprKind::Not),
        ];
        for (token, constructor) in prefix_ops {
            if let Some(span) = self.accept_optional(token)? {
                let inner = self.unary_expr()?;
                let span = self.spans.store_merged(span, &inner);
                let inner = self.alloc(inner);
                return Ok((constructor)(inner).with_span(span));
            }
        }

        // Parse the basic atom within the expression.
        let mut expr = self.atom()?;

        // Try parsing suffix operations.
        loop {
            if self.peek_is(Token::RoundLeft)? {
                let args = self.call_args()?;
                let span = self.spans.merge(&expr, &args);
                expr = ExprKind::Call(self.alloc(expr), args).with_span(span);
            } else {
                break;
            }
        }

        Ok(expr)
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
                })
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
