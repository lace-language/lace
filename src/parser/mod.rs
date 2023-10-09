use crate::lexer::token::Token;
use crate::parser::ast::File;
use crate::parser::span::{Span, Spans};
use bumpalo::Bump;
use error::{ParseError, ParseResult};
use crate::error::{ErrorContext, ResultExt};
use crate::lexer::token_buffer::TokenBuffer;
use crate::source_file::SourceFile;

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

pub struct Parser<'s, 'a, 'e> {
    token_buffer: TokenBuffer<'s>,
    spans: Spans,
    arena: &'a Bump,
    ectx: &'e mut ErrorContext<'s>
}

impl<'s, 'a, 'e> Parser<'s, 'a, 'e> {
    pub fn new(token_buffer: TokenBuffer<'s>, arena: &'a Bump, ectx: &'e mut ErrorContext<'s>) -> Self {
        Self {
            token_buffer,
            spans: Spans::new(),
            arena,
            ectx,
        }
    }

    fn source(&self) -> SourceFile<'s> {
        self.token_buffer.source()
    }

    fn next(&mut self) -> ParseResult<'s, (Token<'s>, Span)> {
        self.token_buffer.next()
            .ok_or(ParseError::EndOfInput)
            .map_err_fatal(self.ectx, self.source())
    }

    pub fn parse(mut self) -> ParseResult<'s, File<'s, 'a>> {
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

    fn peek_is(&mut self, token: Token) -> ParseResult<'s, bool> {
        let Some(lexed_token) = self.peek() else {
            return Ok(false);
        };

        Ok(token == lexed_token)
    }

    fn accept_optional(&mut self, token: Token) -> ParseResult<'s, Option<Span>> {
        if self.peek_is(token)? {
            Ok(Some(self.next()?.1))
        } else {
            Ok(None)
        }
    }

    fn accept_required(&mut self, token: Token) -> ParseResult<'s, Span> {
        let (next, span) = self.next()?;
        if next == token {
            Ok(span)
        } else {
            self.ectx.fatal(ParseError::Expected {
                expected: token.to_string(),
                got: next.to_string(),
                span,
            }, self.source())
        }
    }
}
