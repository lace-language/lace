use crate::lexer::token::Token;
use crate::parser::span::Span;
use crate::source_file::SourceFile;
use logos::SpannedIter;

pub struct TokenStream<'s> {
    lexer: SpannedIter<'s, Token<'s>>,
    pub source: SourceFile<'s>,
}

impl<'s> TokenStream<'s> {
    pub fn from_source(source: SourceFile<'s>) -> Self {
        Self {
            lexer: logos::Lexer::new(source.contents).spanned(),
            source,
        }
    }
}

impl<'s> Iterator for TokenStream<'s> {
    type Item = (Result<Token<'s>, ()>, Span);

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next().map(|(tok, span)| (tok, span.into()))
    }
}
