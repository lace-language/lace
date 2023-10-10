use crate::error::{CompilerErrorKind, CompilerResult, CompilerResultExt, ErrorContext, ResultExt};
use crate::lexer::error::{LexError, LexResult};
use crate::lexer::token::Token;
use crate::lexer::token_stream::TokenStream;
use crate::parser::span::Span;
use crate::source_file::SourceFile;
use miette::Diagnostic;
use std::collections::VecDeque;
use thiserror::Error;

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

#[derive(Debug, Error, Diagnostic, Clone, PartialEq)]
pub enum BracketError {
    #[error("closing {} found without previous opening {}", close_type.closing(), close_type.opening())]
    CloseWithoutOpen {
        close_type: BracketType,
        #[label("closing here")]
        close_span: Span,
    },
    #[error("closing {} expected before this token to match previous opening {}", open_type.closing(), open_type.opening())]
    ClosingInserted {
        open_type: BracketType,
        #[label("previous opening '{}'", open_type.opening())]
        open_span: Span,
        close_type: BracketType,
        #[label("expected close before this token")]
        close_span: Span,
    },
}

impl From<BracketError> for CompilerErrorKind {
    fn from(value: BracketError) -> Self {
        LexError::from(value).into()
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

    fn insert_until_correct(
        &mut self,
        close_type: BracketType,
        close_span: Span,
    ) -> BracketResult<'s, ()> {
        while let Some((open_type, open_span)) = self.brackets_stack.pop() {
            if open_type == close_type {
                return Ok(());
            }

            // TODO: should be *no span*, not the open span, but there's currently no way to represent that.
            self.tokens
                .push_back((open_type.closing_token(), open_span));

            self.ectx.recoverable(
                BracketError::ClosingInserted {
                    open_type,
                    open_span,
                    close_type,
                    close_span,
                },
                self.source,
            )?;
        }

        self.ectx.fatal(
            BracketError::CloseWithoutOpen {
                close_type,
                close_span,
            },
            self.source,
        )?;
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

    pub fn process_tokens(
        mut self,
        token_stream: TokenStream<'s>,
    ) -> LexResult<'s, TokenBuffer<'s>> {
        let source = token_stream.source;

        for (token, span) in token_stream {
            let token = token
                .map_err(|()| LexError::UnrecognizedToken { span })
                .map_err_fatal(self.ectx, source)?;
            self.process_token(token, span)?;
        }

        Ok(TokenBuffer {
            tokens: self.tokens,
            source,
        })
    }
}

#[derive(Debug)]
pub struct TokenBuffer<'s> {
    tokens: VecDeque<(Token<'s>, Span)>,
    source: SourceFile<'s>,
}

impl<'s> TokenBuffer<'s> {
    pub fn from_token_stream(
        token_stream: TokenStream<'s>,
        ectx: &mut ErrorContext<'s>,
    ) -> LexResult<'s, Self> {
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

#[cfg(test)]
mod tests {
    use crate::error::ErrorContext;
    use crate::lexer::error::LexError;
    use crate::lexer::token::Token;
    use crate::lexer::token_buffer::{BracketError, BracketType, TokenBuffer};
    use crate::lexer::token_stream::TokenStream;
    use crate::parser::span::Span;
    use crate::source_file::SourceFile;

    #[test]
    fn insert_brackets() {
        let source = SourceFile::new("([)", "test.lc");
        let mut ectx = ErrorContext::new();

        let token_stream = TokenStream::from_source(source);
        let mut buf = TokenBuffer::from_token_stream(token_stream, &mut ectx).unwrap();

        assert_eq!(buf.next().unwrap().0, Token::RoundLeft);
        assert_eq!(buf.next().unwrap().0, Token::SquareLeft);
        assert_eq!(buf.next().unwrap().0, Token::SquareRight);
        assert_eq!(buf.next().unwrap().0, Token::RoundRight);

        let err = ectx.finish_compile_make_recoverable_fatal(()).unwrap_err();
        assert!(
            err.contains(LexError::BracketError(BracketError::ClosingInserted {
                open_type: BracketType::Square,
                open_span: Span::new(1, 1),
                close_type: BracketType::Round,
                close_span: Span::new(2, 1),
            }))
        );
    }

    #[test]
    fn insert_brackets_fatal() {
        let source = SourceFile::new("([)", "test.lc");
        let mut ectx = ErrorContext::always_fatal();

        let token_stream = TokenStream::from_source(source);
        let buf = TokenBuffer::from_token_stream(token_stream, &mut ectx).unwrap_err();

        assert_eq!(
            buf.get_fatal(),
            &LexError::BracketError(BracketError::ClosingInserted {
                open_type: BracketType::Square,
                open_span: Span::new(1, 1),
                close_type: BracketType::Round,
                close_span: Span::new(2, 1),
            })
        );
    }

    #[test]
    fn close_without_open() {
        let source = SourceFile::new(")", "test.lc");
        let mut ectx = ErrorContext::always_fatal();

        let token_stream = TokenStream::from_source(source);
        let buf = TokenBuffer::from_token_stream(token_stream, &mut ectx).unwrap_err();

        assert_eq!(
            buf.get_fatal(),
            &LexError::BracketError(BracketError::CloseWithoutOpen {
                close_type: BracketType::Round,
                close_span: Span::new(0, 1),
            })
        );
    }
}
