use std::iter::Peekable;

use crate::lexer::{Lexer, Token};
use crate::parser::ast::{Block, Expr, ExprKind, Ident, Lit, Statement};
use crate::parser::span::{Span, WithSpan};
use bumpalo::{collections::Vec, Bump};

macro_rules! tok {
    (&&) => {
        Token::AmpAmp
    };
    (||) => {
        Token::PipePipe
    };
    (<=) => {
        Token::AngleLeftEquals
    };
    (>=) => {
        Token::AngleRightEquals
    };
    (==) => {
        Token::EqualsEquals
    };
    (!=) => {
        Token::BangEquals
    };
    (!) => {
        Token::Bang
    };
    (+) => {
        Token::Plus
    };
    (-) => {
        Token::Minus
    };
    (*) => {
        Token::Star
    };
    (/) => {
        Token::Slash
    };
    (.) => {
        Token::Period
    };
    (,) => {
        Token::Comma
    };
    (;) => {
        Token::Semicolon
    };
    (:) => {
        Token::Colon
    };
    (=) => {
        Token::Equals
    };
    (fn) => {
        Token::Fn
    };
    (let) => {
        Token::Let
    };
    (if) => {
        Token::If
    };
    (else) => {
        Token::Else
    };
    (false) => {
        Token::False
    };
    (true) => {
        Token::True
    };
    (<) => {
        Token::AngleLeft
    };
    (>) => {
        Token::AngleRight
    };
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    EndOfInput,
    Expected,
    // TODO: Figure out how to get the error from logos
    UnrecognizedToken(()),
}

pub struct Parser<'s, 'a> {
    _source: &'s str,
    lexer: Peekable<Lexer<'s>>,
    arena: &'a Bump,
}

impl<'s, 'a> Parser<'s, 'a> {
    pub fn new(source: &'s str, arena: &'a Bump) -> Self {
        Self {
            _source: source,
            lexer: logos::Lexer::new(source).spanned().peekable(),
            arena,
        }
    }

    pub fn parse(&mut self) -> ParseResult<Expr<'s, 'a>> {
        self.expr()
    }

    fn alloc<T>(&mut self, x: T) -> &'a T {
        self.arena.alloc(x)
    }

    fn next(&mut self) -> ParseResult<(Token<'s>, Span)> {
        match self.lexer.next() {
            Some((Ok(t), s)) => Ok((t, s.into())),
            Some((Err(_), _)) => Err(ParseError::UnrecognizedToken(())),
            None => Err(ParseError::EndOfInput),
        }
    }

    fn peek(&mut self) -> ParseResult<Option<Token<'s>>> {
        match self.lexer.peek() {
            Some((Ok(t), _)) => Ok(Some(*t)),
            Some((Err(_), _)) => Err(ParseError::UnrecognizedToken(())),
            None => Ok(None),
        }
    }

    fn peek_is(&mut self, token: Token) -> ParseResult<bool> {
        let Some(lexed_token) = self.peek()? else {
            return Ok(false);
        };

        Ok(token == lexed_token)
    }

    fn accept_optional(&mut self, token: Token) -> ParseResult<Option<Span>> {
        if self.peek_is(token)? {
            Ok(Some(self.next()?.1))
        } else {
            Ok(None)
        }
    }

    fn accept_required(&mut self, token: Token) -> ParseResult<Span> {
        let (next, span) = self.next()?;
        if next == token {
            Ok(span)
        } else {
            Err(ParseError::Expected)
        }
    }

    // TODO: Proper error handling
    // TODO: Function calls
    fn expr(&mut self) -> ParseResult<Expr<'s, 'a>> {
        self.disjunction()
    }

    fn disjunction(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let mut left = self.conjunction()?;
        while self.accept_optional(tok![||])?.is_some() {
            let right = self.conjunction()?;
            let span = left.span().merge(right.span());
            left = ExprKind::LogicalOr(self.alloc(left), self.alloc(right)).with_span(span)
        }
        Ok(left)
    }

    // conjunction = conjunction 'and' inversion
    //             | inversion
    fn conjunction(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let mut left = self.comparison()?;
        while self.accept_optional(tok![&&])?.is_some() {
            let right = self.comparison()?;
            let span = left.span().merge(right.span());
            left = ExprKind::LogicalAnd(self.alloc(left), self.alloc(right)).with_span(span);
        }
        Ok(left)
    }

    fn comparison(&mut self) -> ParseResult<Expr<'s, 'a>> {
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
                let span = left.span().merge(right.span());
                return Ok(f(self.alloc(left), self.alloc(right)).with_span(span));
            }
        }
        Ok(left)
    }

    // inversion = '!' inversion
    //           | sum
    // TODO: this should go to comparison
    fn inversion(&mut self) -> ParseResult<Expr<'s, 'a>> {
        if let Some(span) = self.accept_optional(tok![!])? {
            let arg = self.inversion()?;
            let span = span.merge(arg.span());
            Ok(ExprKind::Not(self.alloc(arg)).with_span(span))
        } else {
            self.sum()
        }
    }

    // sum = sum '+' term
    //     | sum '-' term
    //     | term
    fn sum(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let mut left = self.term()?;
        loop {
            if self.accept_optional(tok![+])?.is_some() {
                let right = self.term()?;
                let span = left.span().merge(right.span());
                left = ExprKind::Add(self.alloc(left), self.alloc(right)).with_span(span);
            } else if self.accept_optional(tok![-])?.is_some() {
                let right = self.term()?;
                let span = left.span().merge(right.span());
                left = ExprKind::Sub(self.alloc(left), self.alloc(right)).with_span(span);
            } else {
                return Ok(left);
            }
        }
    }

    // term = term '*' factor
    //      | term '/' factor
    //      | factor
    fn term(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let mut left = self.factor()?;
        loop {
            if self.accept_optional(tok![*])?.is_some() {
                let right = self.factor()?;
                let span = left.span().merge(right.span());
                left = ExprKind::Mul(self.alloc(left), self.alloc(right)).with_span(span);
            } else if self.accept_optional(tok![/])?.is_some() {
                let right = self.factor()?;
                let span = left.span().merge(right.span());
                left = ExprKind::Div(self.alloc(left), self.alloc(right)).with_span(span);
            } else {
                return Ok(left);
            }
        }
    }

    // factor = '-' atom
    //        | atom
    fn factor(&mut self) -> ParseResult<Expr<'s, 'a>> {
        if let Some(span) = self.accept_optional(tok![-])? {
            let arg = self.atom()?;
            let span = span.merge(arg.span());
            Ok(ExprKind::Neg(self.alloc(arg)).with_span(span))
        } else {
            self.atom()
        }
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

        let (token, span) = self.next()?;
        let expr = match token {
            Token::Ident(s) => ExprKind::Ident(Ident { string: s }).with_span(span),
            Token::False => ExprKind::Lit(Lit::Bool(false)).with_span(span),
            Token::True => ExprKind::Lit(Lit::Bool(true)).with_span(span),
            Token::String(s) => ExprKind::Lit(Lit::String(s)).with_span(span),
            Token::Int(i) => ExprKind::Lit(Lit::Int(i)).with_span(span),
            _ => return Err(ParseError::Expected),
        };

        Ok(expr)
    }

    fn paren(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let start_span = self.accept_required(Token::RoundLeft)?;

        if let Some(end_span) = self.accept_optional(Token::RoundRight)? {
            return Ok(ExprKind::Tuple(&[]).with_span(start_span.merge(&end_span)));
        }

        let expr = self.expr()?;

        // TODO: Add parentheses to AST
        if self.accept_optional(Token::RoundRight)?.is_some() {
            return Ok(expr);
        }

        let mut vec = Vec::new_in(self.arena);
        vec.push(expr);

        while self.accept_optional(tok![,])?.is_some() {
            if let Some(end_span) = self.accept_optional(Token::RoundRight)? {
                let slice = vec.into_bump_slice();
                return Ok(ExprKind::Tuple(slice).with_span(start_span.merge(&end_span)));
            }
            vec.push(self.expr()?);
        }

        if let Some(end_span) = self.accept_optional(Token::RoundRight)? {
            let slice = vec.into_bump_slice();
            Ok(ExprKind::Tuple(slice).with_span(start_span.merge(&end_span)))
        } else {
            Err(ParseError::Expected)
        }
    }

    fn block(&mut self) -> ParseResult<Block<'s, 'a>> {
        let open_span = self.accept_required(Token::CurlyLeft)?;

        let mut vec = Vec::new_in(self.arena);

        loop {
            if let Some(close_span) = self.accept_optional(Token::CurlyRight)? {
                return Ok(Block {
                    stmts: vec.into_bump_slice(),
                    last: None,
                    span: open_span.merge(&close_span),
                });
            }
            // TODO: Spans for statements
            if self.accept_optional(tok![let])?.is_some() {
                let ident = self.ident()?;
                self.accept_required(tok![=])?;
                let expr = self.expr()?;
                self.accept_required(tok![;])?;
                vec.push(Statement::Let(ident, self.alloc(expr)));
            } else if self.peek_is(tok![if])? {
                let expr = self.if_else()?;

                if let Some(close_span) = self.accept_optional(Token::CurlyRight)? {
                    return Ok(Block {
                        stmts: vec.into_bump_slice(),
                        last: Some(expr),
                        span: open_span.merge(&close_span),
                    });
                }

                self.accept_optional(tok![;])?;
                vec.push(Statement::Expr(self.alloc(expr)));
            } else {
                let expr = self.expr()?;
                if self.accept_optional(tok![;])?.is_some() {
                    vec.push(Statement::Expr(self.alloc(expr)));
                } else {
                    let close_span = self.accept_required(Token::CurlyRight)?;
                    return Ok(Block {
                        stmts: vec.into_bump_slice(),
                        last: Some(expr),
                        span: open_span.merge(&close_span),
                    });
                }
            }
        }
    }

    fn if_else(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let start_span = self.accept_required(tok![if])?;

        let expr = self.expr()?;
        let then_block = self.block()?;

        if self.accept_optional(tok![else])?.is_some() {
            let else_block = if self.peek_is(tok![if])? {
                let else_if = self.if_else()?;
                let span = else_if.span;
                Block {
                    stmts: &[],
                    last: Some(else_if),
                    span,
                }
            } else {
                self.block()?
            };

            let span = start_span.merge(&else_block.span);
            Ok(ExprKind::If(
                self.alloc(expr),
                self.alloc(then_block),
                Some(self.alloc(else_block)),
            )
            .with_span(span))
        } else {
            let span = start_span.merge(&then_block.span);
            Ok(ExprKind::If(self.alloc(expr), self.alloc(then_block), None).with_span(span))
        }
    }

    fn ident(&mut self) -> ParseResult<Ident<'s>> {
        match self.next()?.0 {
            Token::Ident(s) => Ok(Ident { string: s }),
            _ => Err(ParseError::Expected),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::parser::ast::{Block, ExprKind, Ident, Lit, Statement};
    use crate::parser::span::Spanned;
    use bumpalo::Bump;

    macro_rules! assert_matches {
        ($expression:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {
            match $expression {
                $pattern $(if $guard)? => {},
                outcome => assert!(false, "expected {:?} to match {}", outcome, stringify!($pattern $(if $guard)?))
            }
        };
    }

    macro_rules! assert_expr_matches {
        ($source:literal, $pattern:pat $(if $guard:expr)? $(,)?) => {
            let arena = Bump::new();
            let mut p = Parser::new($source, &arena);
            let e = p.expr().unwrap();
            assert_matches!(e, $pattern $(if $guard)?)
        }
    }

    macro_rules! spanned {
        ($x:pat) => {
            Spanned { value: $x, .. }
        };
    }

    macro_rules! int {
        ($i:literal) => {
            spanned!(ExprKind::Lit(Lit::Int(stringify!($i))))
        };
    }

    macro_rules! string {
        ($i:literal) => {
            spanned!(ExprKind::Lit(Lit::String($i)))
        };
    }

    macro_rules! ident {
        ($i:ident) => {
            spanned!(ExprKind::Ident(Ident {
                string: stringify!($i),
            }))
        };
    }

    macro_rules! bool {
        ($i:literal) => {
            spanned!(ExprKind::Lit(Lit::Bool($i)))
        };
    }

    macro_rules! neg {
        ($x:pat) => {
            spanned!(ExprKind::Neg($x))
        };
    }

    macro_rules! not {
        ($x:pat) => {
            spanned!(ExprKind::Not($x))
        };
    }

    macro_rules! and {
        ($x:pat, $y:pat) => {
            spanned!(ExprKind::LogicalAnd($x, $y))
        };
    }

    macro_rules! or {
        ($x:pat, $y:pat) => {
            spanned!(ExprKind::LogicalOr($x, $y))
        };
    }

    macro_rules! add {
        ($x:pat, $y:pat) => {
            spanned!(ExprKind::Add($x, $y))
        };
    }

    macro_rules! sub {
        ($x:pat, $y:pat) => {
            spanned!(ExprKind::Sub($x, $y))
        };
    }

    macro_rules! mul {
        ($x:pat, $y:pat) => {
            spanned!(ExprKind::Mul($x, $y))
        };
    }

    macro_rules! div {
        ($x:pat, $y:pat) => {
            spanned!(ExprKind::Div($x, $y))
        };
    }

    macro_rules! gt {
        ($x:pat, $y:pat) => {
            spanned!(ExprKind::Gt($x, $y))
        };
    }

    macro_rules! gte {
        ($x:pat, $y:pat) => {
            spanned!(ExprKind::Gte($x, $y))
        };
    }

    macro_rules! lt {
        ($x:pat, $y:pat) => {
            spanned!(ExprKind::Lt($x, $y))
        };
    }

    macro_rules! lte {
        ($x:pat, $y:pat) => {
            spanned!(ExprKind::Lte($x, $y))
        };
    }

    macro_rules! eq {
        ($x:pat, $y:pat) => {
            spanned!(ExprKind::Eq($x, $y))
        };
    }

    macro_rules! neq {
        ($x:pat, $y:pat) => {
            spanned!(ExprKind::Neq($x, $y))
        };
    }

    macro_rules! tuple {
        ($($x:pat),*) => {
            spanned!(ExprKind::Tuple(&[$($x),*]))
        };
    }

    macro_rules! exp {
        ($x:pat) => {
            Statement::Expr($x)
        };
    }

    macro_rules! let_ {
        ($x:ident, $exp:pat) => {
            Statement::Let(
                Ident {
                    string: stringify!(x),
                },
                $exp,
            )
        };
    }

    macro_rules! block {
        ($($stmts:pat),*) => {
            Block { stmts: &[$($stmts),*], last: None, .. }
        };
        ($($stmts:pat),* => $exp:pat) => {
            Block { stmts: &[$($stmts),*], last: Some($exp), .. }
        };
    }

    macro_rules! block_expr {
        ($($tok:tt)*) =>  {
            spanned!(ExprKind::Block(block!{$($tok)*}))
        }
    }

    macro_rules! if_ {
        ($cond:pat, $then:pat) => {
            spanned!(ExprKind::If($cond, $then, None))
        };
        ($cond:pat, $then:pat, $else:pat) => {
            spanned!(ExprKind::If($cond, $then, Some($else)))
        };
    }

    #[test]
    fn booleans() {
        assert_expr_matches!("false", bool!(false));
        assert_expr_matches!("true", bool!(true));
    }

    #[test]
    fn integers() {
        assert_expr_matches!("10", int!(10));
        assert_expr_matches!("0", int!(0));
        assert_expr_matches!("0054", int!(0054));
        assert_expr_matches!("1_2_3_4_5", int!(1_2_3_4_5));
    }

    #[test]
    fn unary() {
        assert_expr_matches!("- 2", neg!(int!(2)));
    }

    #[test]
    fn logical() {
        assert_expr_matches!("!true", not!(bool!(true)));
        assert_expr_matches!("!false", not!(bool!(false)));
        assert_expr_matches!("true && false", and!(bool!(true), bool!(false)));
        assert_expr_matches!(
            "true && false && false",
            and!(and!(bool!(true), bool!(false)), bool!(false))
        );
        assert_expr_matches!("true || false", or!(bool!(true), bool!(false)));
        assert_expr_matches!(
            "true || false || false",
            or!(or!(bool!(true), bool!(false)), bool!(false))
        );
        assert_expr_matches!(
            "true && false || false",
            or!(and!(bool!(true), bool!(false)), bool!(false))
        );
        assert_expr_matches!(
            "true || false && false",
            or!(bool!(true), and!(bool!(false), bool!(false)))
        );
        assert_expr_matches!(
            "true || !false && false",
            or!(bool!(true), and!(not!(bool!(false)), bool!(false)))
        );
    }

    #[test]
    fn arithmetic_binop() {
        assert_expr_matches!("2 + 3", add!(int!(2), int!(3)));
        assert_expr_matches!("2 + 3 + 4", add!(add!(int!(2), int!(3)), int!(4)));
        assert_expr_matches!("2 - 3", sub!(int!(2), int!(3)));
        assert_expr_matches!("2 - 3 - 4", sub!(sub!(int!(2), int!(3)), int!(4)));
        assert_expr_matches!("2 * 3", mul!(int!(2), int!(3)));
        assert_expr_matches!("2 * 3 * 4", mul!(mul!(int!(2), int!(3)), int!(4)));
        assert_expr_matches!("2 / 3", div!(int!(2), int!(3)));
        assert_expr_matches!("2 / 3 / 4", div!(div!(int!(2), int!(3)), int!(4)));
        assert_expr_matches!(
            "2 / 3 + 4 * 5",
            add!(div!(int!(2), int!(3)), mul!(int!(4), int!(5)))
        );
        assert_expr_matches!("2 + 3 * 4", add!(int!(2), mul!(int!(3), int!(4))));
        assert_expr_matches!("(2 + 3) * 4", mul!(add!(int!(2), int!(3)), int!(4)));
    }

    #[test]
    fn tuples() {
        assert_expr_matches!("()", tuple!());
        assert_expr_matches!("(1)", int!(1));
        assert_expr_matches!("(1,)", tuple!(int!(1)));
        assert_expr_matches!("(1,2)", tuple!(int!(1), int!(2)));
        assert_expr_matches!("(1,2,)", tuple!(int!(1), int!(2)));
        assert_expr_matches!("(1,2,3)", tuple!(int!(1), int!(2), int!(3)));
        assert_expr_matches!("((1,2,3),)", tuple!(tuple!(int!(1), int!(2), int!(3))));
    }

    #[test]
    fn comparisons() {
        assert_expr_matches!("1 <= 2", lte!(int!(1), int!(2)));
        assert_expr_matches!("1 >= 2", gte!(int!(1), int!(2)));
        assert_expr_matches!("1 < 2", lt!(int!(1), int!(2)));
        assert_expr_matches!("1 > 2", gt!(int!(1), int!(2)));
        assert_expr_matches!("1 == 2", eq!(int!(1), int!(2)));
        assert_expr_matches!("1 != 2", neq!(int!(1), int!(2)));
    }

    #[test]
    fn ident() {
        assert_expr_matches!("foo", ident!(foo));
        assert_expr_matches!("bar", ident!(bar));
        assert_expr_matches!("x != y", neq!(ident!(x), ident!(y)));
    }

    #[test]
    fn blocks() {
        assert_expr_matches!("{}", block_expr! {});
        assert_expr_matches!("{ 10 }", block_expr! { => int!(10) });
        assert_expr_matches!("{ 10; }", block_expr! { exp!(int!(10)) });
        assert_expr_matches!(
            "{ 10; 20 }",
            block_expr! {
                exp!(int!(10))
                => int!(20)
            }
        );
        assert_expr_matches!(
            "{ let x = 10; x }",
            block_expr! {
                let_!(x, int!(10))
                => ident!(x)
            }
        );
        assert_expr_matches!(
            "{ if 1 { 2 } 3 }",
            block_expr!(exp!(if_!(int!(1), block!(=> int!(2)))) => int!(3))
        );
        assert_expr_matches!(
            "{ if 1 { 2 }; 3 }",
            block_expr!(exp!(if_!(int!(1), block!(=> int!(2)))) => int!(3))
        );
        assert_expr_matches!(
            "{ if 1 { 2 } else { 3 } 4 }",
            block_expr!(
                exp!(if_!(int!(1), block!(=> int!(2)), block!(=> int!(3))))
                => int!(4)
            )
        );
    }

    #[test]
    fn if_else() {
        assert_expr_matches!(
            "if 1 { 2 } else { 3 }",
            if_!(int!(1), block!(=> int!(2)), block!(=> int!(3))),
        );
        assert_expr_matches!(
            "if 1 { 2 } else { 3 } + 4",
            add!(
                if_!(int!(1), block!(=> int!(2)), block!(=> int!(3))),
                int!(4)
            )
        );
        assert_expr_matches!("if 1 { 2 }", if_!(int!(1), block!(=> int!(2))),);
        assert_expr_matches!(
            "if 1 { 2 } else if 3 { 4 } else { 5 }",
            if_!(
                int!(1),
                block!(=> int!(2)),
                block!(=> if_!(int!(3), block!(=> int!(4)), block!(=> int!(5))))
            ),
        );
    }

    #[test]
    fn strings() {
        assert_expr_matches!("\"Hello, world!\"", string!("\"Hello, world!\""));
        assert_expr_matches!("\"\"", string!("\"\""));
    }
}
