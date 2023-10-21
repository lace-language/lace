use crate::lexer::token::Token;
use crate::lexer::token_buffer::TokenBuffer;
use crate::parser::ast::File;
use crate::parser::span::{Span, Spans};
use bumpalo::Bump;
use error::{ParseError, ParseResult};

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

mod precedence;
#[cfg(test)]
pub mod test;

pub struct Parser<'s, 'a> {
    token_buffer: TokenBuffer<'s>,
    spans: Spans,
    arena: &'a Bump,
}

impl<'s, 'a> Parser<'s, 'a> {
    pub fn new(token_buffer: TokenBuffer<'s>, arena: &'a Bump) -> Self {
        Self {
            token_buffer,
            spans: Spans::new(),
            arena,
        }
    }

    fn next(&mut self) -> ParseResult<(Token<'s>, Span)> {
        self.token_buffer.next().ok_or(ParseError::EndOfInput)
    }

    pub fn parse(mut self) -> ParseResult<File<'s, 'a>> {
        self.file()
    }

    fn alloc<T>(&mut self, x: T) -> &'a T {
        self.arena.alloc(x)
    }

    fn has_next(&mut self) -> bool {
        self.peek().is_some()
    }

    fn peek(&mut self) -> Option<Token<'s>> {
        self.token_buffer.peek().map(|(tok, _)| tok)
    }

    fn peek_is(&mut self, token: Token) -> ParseResult<bool> {
        let Some(lexed_token) = self.peek() else {
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
            Err(ParseError::Expected {
                expected: token.to_string(),
                got: next.to_string(),
                span,
            })
        }
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

    fn peek_any<T>(
        &mut self,
        tokens: impl IntoIterator<Item = (Token<'s>, T)>,
    ) -> ParseResult<Option<T>> {
        for (t, op) in tokens {
            if self.peek_is(t)? {
                return Ok(Some(op));
            }
        }
        Ok(None)
    }
}
