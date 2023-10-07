#![allow(clippy::module_inception)]

mod ice;
mod lexer;
mod parser;

use bumpalo::Bump;
use parser::Parser;

fn main() {
    let arena = Bump::new();
    let parser = Parser::new("1 + 1", &arena);
    println!("{:?}", parser.parse());
}
