use crate::lexer::error::{LexError, LexResult};
use crate::lexer::token::Token;
use crate::lexer::token_stream::TokenStream;
use crate::parser::span::Span;
use crate::source_file::SourceFile;
use std::collections::VecDeque;

#[derive(Debug)]
pub struct TokenBuffer<'s> {
    tokens: VecDeque<(Token<'s>, Span)>,
}

impl<'s> TokenBuffer<'s> {
    pub fn from_token_stream(token_stream: TokenStream<'s>) -> LexResult<Self> {
        Ok(Self {
            tokens: token_stream
                .map(|(tok, span)| match tok {
                    Ok(i) => Ok((i, span)),
                    Err(()) => Err(LexError::UnrecognizedToken { span }),
                })
                .collect::<Result<_, _>>()?,
        })
    }

    pub fn from_source(source: SourceFile<'s>) -> LexResult<Self> {
        Self::from_token_stream(TokenStream::from_source(source))
    }

    pub fn next(&mut self) -> Option<(Token<'s>, Span)> {
        self.tokens.pop_front()
    }

    pub fn peek(&self) -> Option<(Token<'s>, Span)> {
        self.tokens.front().copied()
    }
}
