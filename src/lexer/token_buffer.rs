use std::collections::VecDeque;
use thiserror::Error;
use crate::error::{ErrorContext, ResultExt};
use crate::lexer::error::{LexError, LexResult};
use crate::lexer::token::Token;
use crate::lexer::token_stream::TokenStream;
use crate::parser::span::Span;
use crate::source_file::SourceFile;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BracketType {
    Round,
    Curly,
    Square,
    Angle,
}

impl BracketType {
    pub fn closing_token(&self) -> Token<'static> {
        match self {
            BracketType::Round => Token::RoundRight,
            BracketType::Curly => Token::CurlyRight,
            BracketType::Square => Token::SquareRight,
            BracketType::Angle => Token::AngleRight,
        }
    }

    pub fn closing(&self) -> char {
        match self {
            BracketType::Round => ')',
            BracketType::Curly => '}',
            BracketType::Square => ']',
            BracketType::Angle => '>'
        }
    }

    pub fn opening(&self) -> char {
        match self {
            BracketType::Round => '(',
            BracketType::Curly => '{',
            BracketType::Square => '[',
            BracketType::Angle => '<'
        }
    }
}

#[derive(Debug, Error)]
pub enum BracketError {
    #[error("closing {} found without previous opening bracket of the same type", close_type.closing())]
    CloseWithoutOpen {
        close_type: BracketType,
        close_span: Span
    },
    #[error("closing {} inserted to match previous opening {} while parsing {}", close_type.closing(), close_type.opening(), expected_close_type.closing())]
    ClosingInserted {
        close_type: BracketType,
        open_span: Span,
        expected_close_type: BracketType,
        expected_close_span: Span,
    }
}

pub struct TokenBufferWriter<'s> {
    tokens: VecDeque<(Token<'s>, Span)>,
    brackets_stack: Vec<(BracketType, Span)>,
    bracket_errors: Vec<BracketError>,
}

impl<'s> TokenBufferWriter<'s> {
    pub fn new() -> Self {
        Self {
            tokens: VecDeque::new(),
            brackets_stack: Vec::new(),
            bracket_errors: vec![],
        }
    }

    fn insert_until_correct(&mut self, close_type: BracketType, close_span: Span) {
        while let Some((open_type, open_span)) = self.brackets_stack.pop() {
            if open_type == close_type {
                return;
            }

            // TODO: should be *no span*, not the open span, but there's currently no way to represent that.
            self.tokens.push_back((open_type.closing_token(), open_span));

            self.bracket_errors.push(BracketError::ClosingInserted {
                close_type,
                open_span,
                expected_close_type: close_type,
                expected_close_span: close_span,
            })
        }

        self.bracket_errors.push(BracketError::CloseWithoutOpen {
            close_type,
            close_span,
        })
    }

    fn process_bracket(&mut self, token: Token<'s>, span: Span) {
        match token {
            Token::RoundLeft => {
                self.brackets_stack.push((BracketType::Round, span));
            }
            Token::CurlyLeft => {
                self.brackets_stack.push((BracketType::Curly, span));
            }
            Token::SquareLeft => {
                self.brackets_stack.push((BracketType::Square, span));
            }
            Token::AngleLeft => {
                self.brackets_stack.push((BracketType::Angle, span));
            }
            Token::RoundRight => {
                self.insert_until_correct(BracketType::Round, span);
            }
            Token::CurlyRight => {
                self.insert_until_correct(BracketType::Curly, span);
            }
            Token::SquareRight => {
                self.insert_until_correct(BracketType::Square, span);
            }
            Token::AngleRight => {
                self.insert_until_correct(BracketType::Square, span);
            }
            _ => {/* not important */}
        }
    }

    fn process_token(&mut self, token: Token<'s>, span: Span) {
        self.process_bracket(token, span);
        self.tokens.push_back((token, span))
    }

    pub fn process_tokens(mut self, token_stream: TokenStream<'s>, ectx: &mut ErrorContext<'s>) -> LexResult<'s, TokenBuffer<'s>> {
        let source = token_stream.source;

        for (token, span) in token_stream {
            let token = token.map_err(|()| {
                LexError::UnrecognizedToken {
                    span,
                }
            }).map_err_fatal(ectx, source)?;
            self.process_token(token, span);
        }

        Ok(TokenBuffer {
            tokens: self.tokens,
            source,
        })
    }
}


pub struct TokenBuffer<'s> {
    tokens: VecDeque<(Token<'s>, Span)>,
    #[allow(unused)]
    source: SourceFile<'s>,
}

impl<'s> TokenBuffer<'s> {
    pub fn from_token_stream(token_stream: TokenStream<'s>, ectx: &mut ErrorContext<'s>) -> LexResult<'s, Self> {
        let writer = TokenBufferWriter::new();
        writer.process_tokens(token_stream, ectx)
    }

    pub fn next(&mut self) -> Option<(Token<'s>, Span)> {
        self.tokens.pop_front()
    }

    pub fn peek(&self) -> Option<(Token<'s>, Span)> {
        self.tokens.front().copied()
    }

    pub fn source(&self) -> SourceFile<'s> {
        self.source
    }
}
