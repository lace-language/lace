#![allow(clippy::module_inception)]

mod error;
mod ice;
mod lexer;
mod parser;
mod source_file;

use crate::error::{CompilerError, ResultExt};
use crate::parser::ast::Ast;
use crate::source_file::SourceFile;
use bumpalo::Bump;
use clap::{self, Parser as ClapParser};
use lexer::token_buffer::TokenBuffer;
use parser::Parser;
use miette::Report;

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

fn main() -> Result<(), Report> {
    let cli = Options::parse();
    let filename = &cli.file_name;
    let contents = std::fs::read_to_string(filename).unwrap();
    let source_file = SourceFile{contents: &contents, filename};

    let arena = Bump::new();

    let ast = compile(source_file, &arena).map_err_miette(source_file)?;
    println!("{:?}", ast);

    Ok(())
}
