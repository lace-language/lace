#![allow(clippy::module_inception)]

mod ice;
mod lexer;
mod parser;
mod source_file;
mod error;

#[cfg(test)]
mod ui_tests;

use std::process::ExitCode;
use bumpalo::Bump;
use clap::{self, Parser as ClapParser};
use parser::Parser;
use crate::error::{CompilerErrorKind, CompilerResult, ErrorContext, CompilerResultExt};
use lexer::token_stream::TokenStream;
use crate::parser::ast::Ast;
use crate::source_file::SourceFile;
use lexer::token_buffer::TokenBuffer;

#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Options {
    file_name: String,
}

fn compile<'s, 'a>(source: SourceFile<'s>, arena: &'a Bump) -> CompilerResult<'s, Ast<'s, 'a>, CompilerErrorKind> {
    let mut ectx = ErrorContext::new();

    let token_stream = TokenStream::from_source(source);
    let token_buffer = TokenBuffer::from_token_stream(token_stream, &mut ectx).map_make_generic()?;

    let parser = Parser::new(token_buffer, arena, &mut ectx);
    let ast = parser.parse().map_make_generic()?;

    ectx.finish_compile_make_recoverable_fatal(ast)
}

fn main() -> ExitCode {
    let cli = Options::parse();
    let file_name = &cli.file_name;
    let contents = std::fs::read_to_string(file_name).unwrap();
    let source_file = SourceFile::new(&contents, file_name);

    let arena = Bump::new();

    let ast = match compile(source_file, &arena) {
        Err(e) => {
            eprintln!("{:?}", e);
            return ExitCode::FAILURE;
        },
        Ok(i) => i,
    };
    println!("{:?}", ast);

    ExitCode::SUCCESS
}
