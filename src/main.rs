#![allow(clippy::module_inception)]

mod ice;
mod lexer;
mod nameres;
mod parser;

use bumpalo::Bump;
use clap::{self, Parser as ClapParser};
use miette::LabeledSpan;
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

    // Parsing
    let arena = Bump::new();
    let parser = Parser::new(file_name, &contents, &arena);
    let (spans, ast) = parser.parse()?;

    // Name resolution
    let mut graph = nameres::Graph::new(file_name);
    let resolved = graph.resolve(&ast);

    // For debugging:
    graph.print();

    let src: &str = contents.leak();
    eprintln!("resolved {}", resolved.len());
    for (from, to) in resolved {
        let report = miette::miette!(
            labels = vec![
                LabeledSpan::at(spans.get(from), "reference"),
                LabeledSpan::at(spans.get(to), "definition"),
            ],
            "resolved"
        )
        .with_source_code(src);
        eprintln!("{:?}", report);
    }
    // println!("{:?}", parser.parse()?);
    Ok(())
}
