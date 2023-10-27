use crate::lexer::token_buffer::TokenBuffer;
use crate::parser::ast::{Expr, File};
use crate::parser::error::ParseResult;
use crate::parser::Parser;
use crate::source_file::SourceFile;
use bumpalo::Bump;

pub fn parse_test<'s>(
    source: SourceFile<'s>,
    test: impl for<'a> FnOnce(&'a mut Parser<'s, 'a>) + 's,
) {
    let bump = Bump::new();
    let token_buffer = TokenBuffer::from_source(source).unwrap();
    let mut parser = Parser::new(token_buffer, &bump);
    test(&mut parser);
}

pub fn parse_file_test<'s>(
    source: SourceFile<'s>,
    test: impl for<'a> FnOnce(ParseResult<File<'s, 'a>>) + 's,
) {
    parse_test(source, move |p| test(p.file()))
}

pub fn parse_expr_test<'s>(
    source: SourceFile<'s>,
    test: impl for<'a> FnOnce(ParseResult<Expr<'s, 'a>>) + 's,
) {
    parse_test(source, move |p| test(p.expr()))
}
