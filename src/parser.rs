use crate::ast::{Expr, ExprId, Lit, Statement};
use crate::lexer::{Lexer, Token};
use logos::Logos;

struct Parser<'source> {
    _source: &'source str,
    lexer: Lexer<'source>,
    exprs: Vec<Expr>,
    _stmts: Vec<Statement>,
}

impl<'source> Parser<'source> {
    fn new(source: &'source str) -> Self {
        Self {
            _source: source,
            lexer: Lexer::new(source),
            exprs: Vec::new(),
            _stmts: Vec::new(),
        }
    }

    fn next(&mut self) -> Option<Result<Token, ()>> {
        self.lexer.next()
    }

    fn get_expr(&mut self, ExprId(i): ExprId) -> Expr {
        self.exprs.get(i).unwrap().clone()
    }

    // TODO: Proper error handling
    // TODO: Operators
    // TODO: Function calls
    fn expr(&mut self) -> Option<ExprId> {
        let Some(Ok(token)) = self.next() else {
            return None;
        };

        let expr = match token {
            Token::False => Expr::Lit(Lit::Bool(false)),
            Token::True => Expr::Lit(Lit::Bool(true)),
            Token::String(s) => Expr::Lit(Lit::String(s)),
            Token::Int(i) => Expr::Lit(Lit::Int(i)),
            _ => return None,
        };
        self.exprs.push(expr);
        Some(ExprId(self.exprs.len() - 1))
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::ast::{Expr, Lit};

    fn parse_expr(source: &str) -> Expr {
        let mut p = Parser::new(source);
        let e = p.expr().unwrap();
        p.get_expr(e)
    }

    #[test]
    fn booleans() {
        assert_eq!(parse_expr("false"), Expr::Lit(Lit::Bool(false)));
        assert_eq!(parse_expr("true"), Expr::Lit(Lit::Bool(true)));
    }

    #[test]
    fn integers() {
        assert_eq!(parse_expr("10"), Expr::Lit(Lit::Int(10)));
        assert_eq!(parse_expr("-10"), Expr::Lit(Lit::Int(-10)));
        assert_eq!(parse_expr("0"), Expr::Lit(Lit::Int(0)));
        assert_eq!(parse_expr("0054"), Expr::Lit(Lit::Int(54)));
        assert_eq!(parse_expr("-0054"), Expr::Lit(Lit::Int(-54)));
    }

    #[test]
    fn strings() {
        assert_eq!(
            parse_expr("\"Hello, world!\""),
            Expr::Lit(Lit::String("\"Hello, world!\"".into()))
        );
        assert_eq!(parse_expr("\"\""), Expr::Lit(Lit::String("\"\"".into())));
    }
}
