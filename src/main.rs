#![allow(clippy::module_inception)]

mod ice;
mod lexer;
mod parser;
mod token_preprocessor;
mod source_file;
mod error;

#[cfg(test)]
mod ui_tests;

use bumpalo::Bump;
use clap::{self, Parser as ClapParser};
use parser::Parser;
use crate::error::ToMiette;
use crate::lexer::TokenStream;
use crate::parser::ast::Ast;
use crate::source_file::SourceFile;
use crate::token_preprocessor::PreprocessedTokens;

#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Options {
    file_name: String,
}

fn compile<'s, 'a>(source: SourceFile<'s>, arena: &'a Bump) -> miette::Result<Ast<'s, 'a>> {
    let token_stream = TokenStream::from_source(source);
    let preprocessed_tokens = PreprocessedTokens::from_token_stream(token_stream)
        .to_miette(source)?;

    let parser = Parser::new(preprocessed_tokens, arena);
    let ast = parser.parse()
        .to_miette(source)?;

    Ok(ast)
}

fn main() -> miette::Result<()> {
    let cli = Options::parse();
    let file_name = &cli.file_name;
    let contents = std::fs::read_to_string(file_name).unwrap();
    let source_file = SourceFile::new(&contents, file_name);

    let arena = Bump::new();
    let ast = compile(source_file, &arena)?;
    println!("{:?}", ast);
    Ok(())
}
