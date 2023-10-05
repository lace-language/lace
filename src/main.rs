mod ice;
mod lexer;
mod parser;

use crate::parser::parser::Parser;
use bumpalo::Bump;

fn main() {
    let arena = Bump::new();
    let mut parser = Parser::new("1 + 1", &arena);
    println!("{:?}", parser.parse());
}
