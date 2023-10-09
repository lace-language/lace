use crate::lexer::Token;
use crate::parser::ast::File;
use crate::parser::span::{Span, Spans};
use bumpalo::Bump;
use error::{ParseError, ParseResult};
use crate::token_preprocessor::PreprocessedTokens;

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
    preprocessed_tokens: PreprocessedTokens<'s>,
    spans: Spans,
    arena: &'a Bump,
}

impl<'s, 'a> Parser<'s, 'a> {
    pub fn new(preprocessed_tokens: PreprocessedTokens<'s>, arena: &'a Bump) -> Self {
        Self {
            preprocessed_tokens,
            spans: Spans::new(),
            arena,
        }
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

    fn next(&mut self) -> ParseResult<(Token<'s>, Span)> {
        self.preprocessed_tokens.next().ok_or(ParseError::EndOfInput)
    }

    fn peek(&mut self) -> Option<Token<'s>> {
        self.preprocessed_tokens.peek().map(|(tok, _)| tok)
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
}
