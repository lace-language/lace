use crate::error::ResultExt;
use crate::lexer::token_buffer::TokenBuffer;
use crate::nameres;
use crate::parser::Parser;
use crate::source_file::SourceFile;
use bumpalo::Bump;
use miette::LabeledSpan;

macro_rules! parse {
    (let $pat: pat = $source: literal) => {
        let arena = Bump::new();
        let source = SourceFile::new("test.lc", $source);
        let token_buffer = TokenBuffer::from_source(source).unwrap_miette(source);
        let parser = Parser::new(token_buffer, &arena);
        let (spans, ast) = parser.parse().unwrap_miette(source);
        let $pat = (spans, ast, source);
    };
}

#[test]
fn test_no_nameres() {
    parse!(let (_spans, ast, source) = "fn main(){}");

    let mut graph = nameres::Graph::new(source.filename);
    let resolved = graph.resolve(&ast);
    assert!(resolved.is_empty())
}

#[test]
fn test_recursive() {
    parse!(let (spans, ast, source) = "fn main(){main();}");

    let mut graph = nameres::Graph::new(source.filename);
    let resolved = graph.resolve(&ast);

    assert_eq!(resolved.len(), 1);

    eprintln!("resolved {}", resolved.len());
    for (from, to) in resolved {
        let report = miette::miette!(
            labels = vec![
                LabeledSpan::at(spans.get(from), "reference"),
                LabeledSpan::at(spans.get(to), "definition"),
            ],
            "resolved"
        )
        .with_source_code(source.named_source());
        eprintln!("{:?}", report);
    }
}
