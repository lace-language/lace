use crate::lexer::{Lexer, Token};
use crate::parser::ast::File;
use crate::parser::span::Span;
use bumpalo::Bump;
use error::{ParseError, ParseResult};
use std::iter::Peekable;

#[macro_use]
pub mod tok;
pub mod ast;
pub mod error;
pub mod expr;
pub mod function;
pub mod span;
pub mod statement;
pub mod toplevel;
pub mod type_spec;

#[cfg(test)]
pub mod test;

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

    pub fn parse(&mut self) -> ParseResult<File<'s, 'a>> {
        self.file()
    }

    fn alloc<T>(&mut self, x: T) -> &'a T {
        self.arena.alloc(x)
    }

    fn has_next(&mut self) -> ParseResult<bool> {
        Ok(self.peek()?.is_some())
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
}
