#![allow(clippy::module_inception)]

#[macro_use]
mod lice;

#[cfg(test)]
#[macro_use]
mod test;

mod ast_metadata;
mod debug_file;
mod error;
mod ids;
mod lexer;
mod lowering;
mod name_resolution;
mod parser;
mod source_file;
mod typechecking;

use crate::error::{CompilerError, ResultExt, TypeErrors};
use crate::lowering::lower;
use crate::parser::ast::Ast;
use crate::source_file::SourceFile;
use bumpalo::Bump;
use clap::{self, Parser as ClapParser};
use lexer::token_buffer::TokenBuffer;
use miette::Report;
use miette::{LabeledSpan, Severity};
use parser::Parser;
use typechecking::typecheck;

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
    let mut graph = name_resolution::Graph::new(source.filename);
    let resolved = graph.resolve(&ast);

    // For debugging:
    graph.save_debug();

    let type_arena = Bump::new();
    let types = typecheck(&ast, &resolved, &spans, &type_arena).map_err(TypeErrors::from)?;

    let disp_arena = Bump::new();
    eprintln!("resolved {} references", resolved.names.len());
    for (from, to) in &resolved.names {
        println!("{}", source.slice_span(spans.get(*from)));
        let ty = types.type_of_name(*from, &disp_arena)?;

        let report = miette::miette!(
            labels = vec![
                LabeledSpan::at(spans.get(*from), "reference"),
                LabeledSpan::at(spans.get(*to), "definition"),
            ],
            help = format!("type is {}", ty),
            severity = Severity::Advice,
            "resolved"
        )
        .with_source_code(source.named_source());
        eprintln!("{:?}", report);
    }

    let lir = lower(&ast, &types)?;
    println!("{}", lir);

    Ok(ast)
}

fn main() -> Result<(), Report> {
    let cli = Options::parse();
    let filename = &cli.file_name;
    let contents = std::fs::read_to_string(filename).unwrap();
    let source_file = SourceFile {
        contents: &contents,
        filename,
    };

    let arena = Bump::new();

    let ast = compile(source_file, &arena).map_err_miette(source_file)?;
    println!("{:?}", ast);

    Ok(())
}
