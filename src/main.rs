#![allow(clippy::module_inception)]

mod ice;
mod lexer;
mod parser;

use bumpalo::Bump;
use parser::Parser;

fn main() -> miette::Result<()> {
    let arena = Bump::new();
    let parser = Parser::new("fn foo() { let 10 = 10; }", &arena);
    println!("{:?}", parser.parse()?);
    Ok(())
}
