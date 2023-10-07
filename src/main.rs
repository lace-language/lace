#![allow(clippy::module_inception)]

mod ice;
mod lexer;
mod parser;

use bumpalo::Bump;
use clap::{self, Parser as ClapParser};
use parser::Parser;

#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Options {
    file_name: String,
}

fn main() -> miette::Result<()> {
    let cli = Options::parse();
    let file_name = &cli.file_name;
    let contents = std::fs::read_to_string(file_name).unwrap();

    let arena = Bump::new();
    let parser = Parser::new(file_name, &contents, &arena);
    println!("{:?}", parser.parse()?);
    Ok(())
}
