use crate::error::ResultExt;
use crate::lexer::token_buffer::TokenBuffer;
use crate::name_resolution;
use crate::parser::Parser;
use crate::source_file::SourceFile;
use bumpalo::Bump;

macro_rules! parse {
    (let $pat: pat = $source: literal) => {
        let arena = Bump::new();
        let source = SourceFile {
            filename: "test.lc",
            contents: $source,
        };
        let token_buffer = TokenBuffer::from_source(source)
            .map_err_miette(source)
            .unwrap();
        let parser = Parser::new(token_buffer, &arena);
        let (spans, ast) = parser.parse().map_err_miette(source).unwrap();
        let $pat = (spans, ast, source);
    };
}

#[test]
fn test_no_nameres() {
    parse!(let (_spans, ast, source) = "fn main(){}");

    let mut graph = name_resolution::Graph::new(source.filename);
    let resolved = graph.resolve(&ast);
    assert!(resolved.names.is_empty())
}

#[test]
fn test_recursive() {
    parse!(let (_spans, ast, source) = "fn main(){main();}");

    let mut graph = name_resolution::Graph::new(source.filename);
    let resolved = graph.resolve(&ast);

    assert_eq!(resolved.names.len(), 1);

    // eprintln!("resolved {}", resolved.names.len());
    // for (from, to) in resolved {
    //     let report = miette::miette!(
    //         labels = vec![
    //             LabeledSpan::at(spans.get(from), "reference"),
    //             LabeledSpan::at(spans.get(to), "definition"),
    //         ],
    //         "resolved"
    //     )
    //     .with_source_code(source.named_source());
    //     eprintln!("{:?}", report);
    // }
}
