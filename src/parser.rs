use std::iter::Peekable;

use crate::ast::{Expr, Lit, Statement};
use crate::lexer::{Lexer, Token};
use bumpalo::{collections::Vec, Bump};
use logos::Logos;

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
enum ParseError {
    EndOfInput,
    Expected,
    // TODO: Figure out how to get the error from logos
    UnrecognizedToken(()),
}

struct Parser<'source, 'arena> {
    _source: &'source str,
    lexer: Peekable<Lexer<'source>>,
    arena: &'arena Bump,
}

impl<'source, 'arena> Parser<'source, 'arena> {
    fn new(source: &'source str, arena: &'arena Bump) -> Self {
        Self {
            _source: source,
            lexer: Lexer::new(source).peekable(),
            arena,
        }
    }

    fn alloc<T>(&mut self, x: T) -> &'arena T {
        self.arena.alloc(x)
    }

    fn next(&mut self) -> ParseResult<Token<'source>> {
        match self.lexer.next() {
            Some(Ok(t)) => Ok(t),
            Some(Err(_)) => Err(ParseError::UnrecognizedToken(())),
            None => Err(ParseError::EndOfInput),
        }
    }

    fn peek(&mut self) -> ParseResult<Option<Token<'source>>> {
        match self.lexer.peek() {
            Some(Ok(t)) => Ok(Some(t.clone())),
            Some(Err(_)) => Err(ParseError::UnrecognizedToken(())),
            None => Ok(None),
        }
    }

    fn accept(&mut self, token: Token) -> ParseResult<bool> {
        let Some(lexed_token) = self.peek()? else {
            return Ok(false);
        };

        if token == lexed_token {
            self.next()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    // TODO: Proper error handling
    // TODO: Operators
    // TODO: Function calls
    fn expr(&mut self) -> ParseResult<&'arena Expr<'source, 'arena>> {
        self.disjunction()
    }

    fn disjunction(&mut self) -> ParseResult<&'arena Expr<'source, 'arena>> {
        let mut left = self.conjunction()?;
        while self.accept(Token::PipePipe)? {
            let right = self.conjunction()?;
            left = self.alloc(Expr::LogicalOr(left, right));
        }
        Ok(left)
    }

    // conjunction = conjunction 'and' inversion
    //             | inversion
    fn conjunction(&mut self) -> ParseResult<&'arena Expr<'source, 'arena>> {
        let mut left = self.inversion()?;
        while self.accept(Token::AmpAmp)? {
            let right = self.inversion()?;
            left = self.alloc(Expr::LogicalAnd(left, right));
        }
        Ok(left)
    }

    // inversion = '!' inversion
    //           | sum
    // TODO: this should go to comparison
    fn inversion(&mut self) -> ParseResult<&'arena Expr<'source, 'arena>> {
        if self.accept(Token::Bang)? {
            let arg = self.inversion()?;
            Ok(self.alloc(Expr::UnaryNot(arg)))
        } else {
            self.sum()
        }
    }

    // sum = sum '+' term
    //     | sum '-' term
    //     | term
    fn sum(&mut self) -> ParseResult<&'arena Expr<'source, 'arena>> {
        let mut left = self.term()?;
        loop {
            if self.accept(Token::Plus)? {
                let right = self.term()?;
                left = self.alloc(Expr::Add(left, right));
            } else if self.accept(Token::Minus)? {
                let right = self.term()?;
                left = self.alloc(Expr::Sub(left, right));
            } else {
                return Ok(left);
            }
        }
    }

    // term = term '*' factor
    //      | term '/' factor
    //      | factor
    fn term(&mut self) -> ParseResult<&'arena Expr<'source, 'arena>> {
        let mut left = self.factor()?;
        loop {
            if self.accept(Token::Star)? {
                let right = self.factor()?;
                left = self.alloc(Expr::Mul(left, right));
            } else if self.accept(Token::Slash)? {
                let right = self.factor()?;
                left = self.alloc(Expr::Div(left, right));
            } else {
                return Ok(left);
            }
        }
    }

    // factor = '-' atom
    //        | atom
    fn factor(&mut self) -> ParseResult<&'arena Expr<'source, 'arena>> {
        if self.accept(Token::Minus)? {
            let arg = self.atom()?;
            Ok(self.alloc(Expr::UnaryMinus(arg)))
        } else {
            self.atom()
        }
    }

    fn atom(&mut self) -> ParseResult<&'arena Expr<'source, 'arena>> {
        let expr = match self.next()? {
            Token::False => Expr::Lit(Lit::Bool(false)),
            Token::True => Expr::Lit(Lit::Bool(true)),
            Token::String(s) => Expr::Lit(Lit::String(s)),
            Token::Int(i) => Expr::Lit(Lit::Int(i)),
            Token::RoundOpen => return self.paren(),
            _ => return Err(ParseError::Expected),
        };

        Ok(self.alloc(expr))
    }

    fn paren(&mut self) -> ParseResult<&'arena Expr<'source, 'arena>> {
        if self.accept(Token::RoundClose)? {
            return Ok(self.alloc(Expr::Tuple(&[])));
        }

        let expr = self.expr()?;

        if self.accept(Token::RoundClose)? {
            return Ok(expr);
        }

        let mut vec = Vec::new_in(self.arena);
        vec.push(expr);

        while self.accept(Token::Comma)? {
            if self.accept(Token::RoundClose)? {
                let slice = vec.into_bump_slice();
                return Ok(self.alloc(Expr::Tuple(slice)));
            }
            vec.push(self.expr()?);
        }

        if self.accept(Token::RoundClose)? {
            let slice = vec.into_bump_slice();
            Ok(self.alloc(Expr::Tuple(slice)))
        } else {
            Err(ParseError::Expected)
        }
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::ast::{Expr, Lit};
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
            Expr::Lit(Lit::Int($i))
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
        assert_expr_matches!("0054", int!(54));
        assert_expr_matches!("1_2_3_4_5", int!(12345));
    }

    #[test]
    fn unary() {
        assert_expr_matches!("- 2", Expr::UnaryMinus(int!(2)));
    }

    #[test]
    fn logical() {
        assert_expr_matches!("!true", Expr::UnaryNot(bool!(true)));
        assert_expr_matches!("!false", Expr::UnaryNot(bool!(false)));
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
                Expr::LogicalAnd(Expr::UnaryNot(bool!(false)), bool!(false))
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
    fn strings() {
        assert_expr_matches!(
            "\"Hello, world!\"",
            Expr::Lit(Lit::String("\"Hello, world!\"")),
        );
        assert_expr_matches!("\"\"", Expr::Lit(Lit::String("\"\"")));
    }
}
