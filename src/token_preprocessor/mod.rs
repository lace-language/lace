use std::collections::VecDeque;
use crate::lexer::{TokenStream, Token};
use crate::parser::span::Span;
use crate::source_file::SourceFile;
use crate::token_preprocessor::error::{PreparseError, PreparseResult};

mod error;

pub struct PreprocessedTokens<'s> {
    tokens: VecDeque<(Token<'s>, Span)>,
    #[allow(unused)]
    source: SourceFile<'s>,
}

impl<'s> PreprocessedTokens<'s> {
    pub fn from_token_stream(token_stream: TokenStream<'s>) -> PreparseResult<Self> {
        let mut tokens = VecDeque::new();
        let source = token_stream.source;

        for (token, span) in token_stream {
            let token = token.map_err(|()| {
                PreparseError::UnrecognizedToken {
                    span,
                }
            })?;

            tokens.push_back((token, span));
        }

        Ok(Self {
            tokens,
            source,
        })
    }

    pub fn next(&mut self) -> Option<(Token<'s>, Span)> {
        self.tokens.pop_front()
    }

    pub fn peek(&self) -> Option<(Token<'s>, Span)> {
        self.tokens.front().copied()
    }
}
