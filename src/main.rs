#![allow(clippy::module_inception)]

mod error;
mod ice;
mod lexer;
mod nameres;
mod parser;
mod source_file;

use crate::error::{print_error, CompilerError};
use crate::parser::ast::Ast;
use crate::source_file::SourceFile;
use bumpalo::Bump;
use clap::{self, Parser as ClapParser};
use lexer::token_buffer::TokenBuffer;
use miette::LabeledSpan;
use parser::Parser;
use std::process::ExitCode;

#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Options {
    file_name: String,
}

fn compile<'s, 'a>(source: SourceFile<'s>, arena: &'a Bump) -> Result<Ast<'s, 'a>, CompilerError> {
    let token_buffer = TokenBuffer::from_source(source)?;

    // Parsing
    let parser = Parser::new(token_buffer, arena);
    let (spans, ast) = parser.parse()?;

    // Name resolution
    let mut graph = nameres::Graph::new(source.filename);
    let resolved = graph.resolve(&ast);

    // For debugging:
    graph.print();

    let src: &str = source.contents.leak();
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
