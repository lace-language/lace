use std::collections::VecDeque;
use miette::Diagnostic;
use thiserror::Error;
use crate::error::{CompilerErrorKind, CompilerResult, CompilerResultExt, ErrorContext, ResultExt};
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
}

impl BracketType {
    pub fn closing_token(&self) -> Token<'static> {
        match self {
            BracketType::Round => Token::RoundRight,
            BracketType::Curly => Token::CurlyRight,
            BracketType::Square => Token::SquareRight,
        }
    }

    pub fn closing(&self) -> char {
        match self {
            BracketType::Round => ')',
            BracketType::Curly => '}',
            BracketType::Square => ']',
        }
    }

    pub fn opening(&self) -> char {
        match self {
            BracketType::Round => '(',
            BracketType::Curly => '{',
            BracketType::Square => '[',
        }
    }
}

pub type BracketResult<'s, T> = CompilerResult<'s, T, BracketError>;

#[derive(Debug, Error, Diagnostic, Clone)]
pub enum BracketError {
    #[error("closing {} found without previous opening {}", close_type.closing(), close_type.opening())]
    CloseWithoutOpen {
        close_type: BracketType,
        #[label("closing here")]
        close_span: Span,
    },
    #[error("closing {} inserted to match previous opening {} while parsing {}", open_type.closing(), open_type.opening(), expected_close_type.closing())]
    ClosingInserted {
        open_type: BracketType,
        #[label("opened here")]
        open_span: Span,
        expected_close_type: BracketType,
        #[label("expected close before this token")]
        expected_close_span: Span,
    },
}

impl Into<CompilerErrorKind> for BracketError {
    fn into(self) -> CompilerErrorKind {
        LexError::from(self).into()
    }
}


pub struct TokenBufferWriter<'s, 'e> {
    tokens: VecDeque<(Token<'s>, Span)>,
    brackets_stack: Vec<(BracketType, Span)>,
    ectx: &'e mut ErrorContext<'s>,
    source: SourceFile<'s>,
}

impl<'s, 'e> TokenBufferWriter<'s, 'e> {
    pub fn new(ectx: &'e mut ErrorContext<'s>, source: SourceFile<'s>) -> Self {
        Self {
            tokens: VecDeque::new(),
            brackets_stack: Vec::new(),
            ectx,
            source,
        }
    }

    fn insert_until_correct(&mut self, close_type: BracketType, close_span: Span) -> BracketResult<'s, ()> {
        while let Some((open_type, open_span)) = self.brackets_stack.pop() {
            if open_type == close_type {
                return Ok(());
            }

            // TODO: should be *no span*, not the open span, but there's currently no way to represent that.
            self.tokens.push_back((open_type.closing_token(), open_span));

            self.ectx.recoverable(BracketError::ClosingInserted {
                open_type,
                open_span,
                expected_close_type: close_type,
                expected_close_span: close_span,
            }, self.source)?;
        }

        self.ectx.fatal(BracketError::CloseWithoutOpen {
            close_type,
            close_span,
        }, self.source)?;
        Ok(())
    }

    fn process_bracket(&mut self, token: Token<'s>, span: Span) -> BracketResult<'s, ()> {
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
            Token::RoundRight => {
                self.insert_until_correct(BracketType::Round, span)?;
            }
            Token::CurlyRight => {
                self.insert_until_correct(BracketType::Curly, span)?;
            }
            Token::SquareRight => {
                self.insert_until_correct(BracketType::Square, span)?;
            }
            _ => { /* not important */ }
        }

        Ok(())
    }

    fn process_token(&mut self, token: Token<'s>, span: Span) -> LexResult<'s, ()> {
        self.process_bracket(token, span).map_into()?;
        self.tokens.push_back((token, span));

        Ok(())
    }

    pub fn process_tokens(mut self, token_stream: TokenStream<'s>) -> LexResult<'s, TokenBuffer<'s>> {
        let source = token_stream.source;

        for (token, span) in token_stream {
            let token = token.map_err(|()| {
                LexError::UnrecognizedToken {
                    span,
                }
            }).map_err_fatal(self.ectx, source)?;
            self.process_token(token, span)?;
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
        let writer = TokenBufferWriter::new(ectx, token_stream.source);
        writer.process_tokens(token_stream)
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
