mod lexer;
mod parser;
mod ice;

use bumpalo::Bump;
use crate::parser::parser::Parser;

fn main() {
    let arena = Bump::new();
    let mut parser = Parser::new("1 + 1", &arena);
    println!("{:?}", parser.parse());
}
