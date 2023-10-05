use std::iter::Peekable;

use crate::parser::ast::{Block, Expr, ExprKind, Ident, Lit, Statement};
use crate::lexer::{Lexer, Token};
use bumpalo::{collections::Vec, Bump};
use crate::parser::span::{Span, Spanned, WithSpan};

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

    fn accept(&mut self, token: Token) -> ParseResult<Option<Span>> {
        let res = self.accept_peek(token)?;
        if res {
            Ok(Some(self.next()?.1))
        } else {
            Ok(None)
        }
    }

    fn accept_peek(&mut self, token: Token) -> ParseResult<bool> {
        let Some(lexed_token) = self.peek()? else {
            return Ok(false);
        };

        Ok(token == lexed_token)
    }

    fn eat(&mut self, token: Token) -> ParseResult<Span> {
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
        while self.accept(tok![||])?.is_some() {
            let right = self.conjunction()?;
            left = ExprKind::LogicalOr(self.alloc(left), self.alloc(right))
                .with_span(left.span().merge(right.span()));
        }
        Ok(left)
    }

    // conjunction = conjunction 'and' inversion
    //             | inversion
    fn conjunction(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let mut left = self.comparison()?;
        while self.accept(tok![&&])?.is_some() {
            let right = self.comparison()?;
            left = ExprKind::LogicalAnd(self.alloc(left), self.alloc(right))
                .with_span(left.span().merge(right.span()));
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
            if self.accept(*t)?.is_some() {
                let right = self.inversion()?;
                return Ok(f(self.alloc(left), self.alloc(right)).with_span(left.span().merge(right.span())));
            }
        }
        Ok(left)
    }

    // inversion = '!' inversion
    //           | sum
    // TODO: this should go to comparison
    fn inversion(&mut self) -> ParseResult<Expr<'s, 'a>> {
        if let Some(span) = self.accept(tok![!])? {
            let arg = self.inversion()?;
            Ok(ExprKind::Not(self.alloc(arg)).with_span(span.merge(&arg.span())))
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
            if self.accept(tok![+])?.is_some() {
                let right = self.term()?;
                left = ExprKind::Add(self.alloc(left), self.alloc(right)).with_span(left.span().merge(right.span()));
            } else if self.accept(tok![-])?.is_some() {
                let right = self.term()?;
                left = ExprKind::Sub(self.alloc(left), self.alloc(right)).with_span(left.span().merge(right.span()));
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
            if self.accept(tok![*])?.is_some() {
                let right = self.factor()?;
                left = ExprKind::Mul(self.alloc(left), self.alloc(right))
                    .with_span(left.span().merge(right.span()));
            } else if self.accept(tok![/])?.is_some() {
                let right = self.factor()?;
                left = ExprKind::Div(self.alloc(left), self.alloc(right))
                    .with_span(left.span().merge(right.span()));
            } else {
                return Ok(left);
            }
        }
    }

    // factor = '-' atom
    //        | atom
    fn factor(&mut self) -> ParseResult<Expr<'s, 'a>> {
        if let Some(span) = self.accept(tok![-])? {
            let arg = self.atom()?;
            Ok(ExprKind::Neg(self.alloc(arg)).with_span(span.merge(arg.span())))
        } else {
            self.atom()
        }
    }

    fn atom(&mut self) -> ParseResult<Expr<'s, 'a>> {
        if self.accept_peek(Token::RoundLeft)? {
            return self.paren();
        } else if self.accept_peek(Token::CurlyLeft)? {
            let expr = self.block()?;
            let span = expr.span;
            return Ok(ExprKind::Block(self.alloc(expr)).with_span(span));
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
        self.eat(Token::RoundLeft)?;

        if self.accept(Token::RoundRight)? {
            return Ok(Expr::Tuple(&[]));
        }

        let expr = self.expr()?;

        if self.accept(Token::RoundRight)? {
            return Ok(expr);
        }

        let mut vec = Vec::new_in(self.arena);
        vec.push(expr);

        while self.accept(tok![,])? {
            if self.accept(Token::RoundRight)? {
                let slice = vec.into_bump_slice();
                return Ok(Expr::Tuple(slice));
            }
            vec.push(self.expr()?);
        }

        if self.accept(Token::RoundRight)? {
            let slice = vec.into_bump_slice();
            Ok(Expr::Tuple(slice))
        } else {
            Err(ParseError::Expected)
        }
    }

    fn block(&mut self) -> ParseResult<Block<'s, 'a>> {
        let open_span = self.eat(Token::CurlyLeft)?;

        let mut vec = Vec::new_in(self.arena);

        loop {
            if let Some(close_span) = self.accept(Token::CurlyRight)? {
                return Ok(Block {
                    stmts: vec.into_bump_slice(),
                    last: None,
                    span: open_span.merge(&close_span)
                });
            }
            if self.accept(tok![let])? {
                let ident = self.ident()?;
                self.eat(tok![=])?;
                let expr = self.expr()?;
                self.eat(tok![;])?;
                vec.push(Statement::Let(ident, self.alloc(expr)));
            } else {
                let expr = self.expr()?;
                if self.accept(tok![;])? {
                    vec.push(Statement::Expr(self.alloc(expr)));
                } else {
                    let close_span = self.eat(Token::CurlyRight)?;
                    return Ok(Block {
                        stmts: vec.into_bump_slice(),
                        last: Some(expr),
                        span: open_span.merge(&close_span)
                    });
                }
            }
        }
    }

    fn ident(&mut self) -> ParseResult<Ident<'s>> {
        match self.next()? {
            Token::Ident(s) => Ok(Ident { string: s }),
            _ => Err(ParseError::Expected),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::parser::ast::{Block, Expr, Ident, Lit, Statement};
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

    macro_rules! int {
        ($i:literal) => {
            Expr::Lit(Lit::Int(stringify!($i)))
        };
    }

    macro_rules! bool {
        ($i:literal) => {
            Expr::Lit(Lit::Bool($i))
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
        assert_expr_matches!("- 2", Expr::Neg(int!(2)));
    }

    #[test]
    fn logical() {
        assert_expr_matches!("!true", Expr::Not(bool!(true)));
        assert_expr_matches!("!false", Expr::Not(bool!(false)));
        assert_expr_matches!("true && false", Expr::LogicalAnd(bool!(true), bool!(false)));
        assert_expr_matches!(
            "true && false && false",
            Expr::LogicalAnd(Expr::LogicalAnd(bool!(true), bool!(false)), bool!(false))
        );
        assert_expr_matches!("true || false", Expr::LogicalOr(bool!(true), bool!(false)));
        assert_expr_matches!(
            "true || false || false",
            Expr::LogicalOr(Expr::LogicalOr(bool!(true), bool!(false)), bool!(false))
        );
        assert_expr_matches!(
            "true && false || false",
            Expr::LogicalOr(Expr::LogicalAnd(bool!(true), bool!(false)), bool!(false))
        );
        assert_expr_matches!(
            "true || false && false",
            Expr::LogicalOr(bool!(true), Expr::LogicalAnd(bool!(false), bool!(false)))
        );
        assert_expr_matches!(
            "true || !false && false",
            Expr::LogicalOr(
                bool!(true),
                Expr::LogicalAnd(Expr::Not(bool!(false)), bool!(false))
            )
        );
    }

    #[test]
    fn arithmetic_binop() {
        assert_expr_matches!("2 + 3", Expr::Add(int!(2), int!(3)));
        assert_expr_matches!("2 + 3 + 4", Expr::Add(Expr::Add(int!(2), int!(3)), int!(4)));
        assert_expr_matches!("2 - 3", Expr::Sub(int!(2), int!(3)));
        assert_expr_matches!("2 - 3 - 4", Expr::Sub(Expr::Sub(int!(2), int!(3)), int!(4)));
        assert_expr_matches!("2 * 3", Expr::Mul(int!(2), int!(3)));
        assert_expr_matches!("2 * 3 * 4", Expr::Mul(Expr::Mul(int!(2), int!(3)), int!(4)));
        assert_expr_matches!("2 / 3", Expr::Div(int!(2), int!(3)));
        assert_expr_matches!("2 / 3 / 4", Expr::Div(Expr::Div(int!(2), int!(3)), int!(4)));
        assert_expr_matches!(
            "2 / 3 + 4 * 5",
            Expr::Add(Expr::Div(int!(2), int!(3)), Expr::Mul(int!(4), int!(5)))
        );
        assert_expr_matches!("2 + 3 * 4", Expr::Add(int!(2), Expr::Mul(int!(3), int!(4))));
        assert_expr_matches!(
            "(2 + 3) * 4",
            Expr::Mul(Expr::Add(int!(2), int!(3)), int!(4))
        );
    }

    #[test]
    fn tuples() {
        assert_expr_matches!("()", Expr::Tuple(&[]));
        assert_expr_matches!("(1)", int!(1));
        assert_expr_matches!("(1,)", Expr::Tuple(&[int!(1)]));
        assert_expr_matches!("(1,2)", Expr::Tuple(&[int!(1), int!(2)]));
        assert_expr_matches!("(1,2,)", Expr::Tuple(&[int!(1), int!(2)]));
        assert_expr_matches!("(1,2,3)", Expr::Tuple(&[int!(1), int!(2), int!(3)]));
        assert_expr_matches!(
            "((1,2,3),)",
            Expr::Tuple(&[Expr::Tuple(&[int!(1), int!(2), int!(3)])])
        );
    }

    #[test]
    fn comparisons() {
        assert_expr_matches!("1 <= 2", Expr::Lte(int!(1), int!(2)));
        assert_expr_matches!("1 >= 2", Expr::Gte(int!(1), int!(2)));
        assert_expr_matches!("1 < 2", Expr::Lt(int!(1), int!(2)));
        assert_expr_matches!("1 > 2", Expr::Gt(int!(1), int!(2)));
        assert_expr_matches!("1 == 2", Expr::Eq(int!(1), int!(2)));
        assert_expr_matches!("1 != 2", Expr::Neq(int!(1), int!(2)));
    }

    #[test]
    fn ident() {
        assert_expr_matches!("foo", Expr::Ident(Ident { string: "foo" }));
        assert_expr_matches!("bar", Expr::Ident(Ident { string: "bar" }));
        assert_expr_matches!(
            "x != y",
            Expr::Neq(
                Expr::Ident(Ident { string: "x" }),
                Expr::Ident(Ident { string: "y" }),
            )
        );
    }

    #[test]
    fn blocks() {
        assert_expr_matches!(
            "{}",
            Expr::Block(Block {
                stmts: &[],
                last: None
            })
        );
        assert_expr_matches!(
            "{ 10 }",
            Expr::Block(Block {
                stmts: &[],
                last: Some(int!(10)),
            })
        );
        assert_expr_matches!(
            "{ 10; }",
            Expr::Block(Block {
                stmts: &[Statement::Expr(int!(10))],
                last: None,
            })
        );
        assert_expr_matches!(
            "{ 10; 20 }",
            Expr::Block(Block {
                stmts: &[Statement::Expr(int!(10))],
                last: Some(int!(20)),
            })
        );
        assert_expr_matches!(
            "{ let x = 10; x }",
            Expr::Block(Block {
                stmts: &[Statement::Let(Ident { string: "x" }, int!(10))],
                last: Some(Expr::Ident(Ident { string: "x" })),
            })
        );
    }

    #[test]
    fn strings() {
        assert_expr_matches!(
            "\"Hello, world!\"",
            Expr::Lit(Lit::String("\"Hello, world!\"")),
        );
        assert_expr_matches!("\"\"", Expr::Lit(Lit::String("\"\"")));
    }
}
