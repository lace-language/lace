#![allow(clippy::module_inception)]

mod error;
mod ice;
mod lexer;
mod parser;
mod source_file;

use crate::error::{print_error, CompilerError};
use crate::parser::ast::Ast;
use crate::source_file::SourceFile;
use bumpalo::Bump;
use clap::{self, Parser as ClapParser};
use lexer::token_buffer::TokenBuffer;
use parser::Parser;
use std::process::ExitCode;

#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Options {
    file_name: String,
}

fn compile<'s, 'a>(source: SourceFile<'s>, arena: &'a Bump) -> Result<Ast<'s, 'a>, CompilerError> {
    let token_buffer = TokenBuffer::from_source(source)?;

    let parser = Parser::new(token_buffer, arena);
    let ast = parser.parse()?;

    Ok(ast)
}

fn main() -> ExitCode {
    let cli = Options::parse();
    let file_name = &cli.file_name;
    let contents = std::fs::read_to_string(file_name).unwrap();
    let source_file = SourceFile::new(&contents, file_name);

    let arena = Bump::new();

    let ast = match compile(source_file, &arena) {
        Err(e) => {
            print_error(e, source_file);
            return ExitCode::FAILURE;
        }
        Ok(i) => i,
    };
    println!("{:?}", ast);

    ExitCode::SUCCESS
}
