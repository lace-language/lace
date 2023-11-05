use crate::error::ResultExt;
use crate::typechecking::error::TypeError;

macro_rules! parse {
    (let $pat: pat = $source: literal) => {
        let arena = ::bumpalo::Bump::new();
        let source = $crate::source_file::SourceFile {
            filename: "test.lc",
            contents: $source,
        };
        let token_buffer = $crate::lexer::token_buffer::TokenBuffer::from_source(source)
            .map_err_miette(source)
            .unwrap();
        let parser = $crate::parser::Parser::new(token_buffer, &arena);
        let (spans, ast) = parser.parse().map_err_miette(source).unwrap();

        let mut graph = $crate::name_resolution::Graph::new(source.filename);
        let resolved = graph.resolve(&ast);

        let type_arena = ::bumpalo::Bump::new();
        let types = $crate::typechecking::typecheck(&ast, &resolved, &spans, &type_arena);

        let $pat = types;
    };
}

#[test]
fn invalid_binop_usage() {
    parse!(let types = "fn main() {3 + true}");
    assert_matches!(&types.unwrap_err()[0], TypeError::BinaryOp { .. });
    parse!(let types = "fn main() {3 * true}");
    assert_matches!(&types.unwrap_err()[0], TypeError::BinaryOp { .. });
    parse!(let types = "fn main() {3 - true}");
    assert_matches!(&types.unwrap_err()[0], TypeError::BinaryOp { .. });
    parse!(let types = "fn main() {3 / true}");
    assert_matches!(&types.unwrap_err()[0], TypeError::BinaryOp { .. });
    parse!(let types = "fn main() {3 || true}");
    assert_matches!(&types.unwrap_err()[0], TypeError::BinaryOp { .. });
    parse!(let types = "fn main() {3 && true}");
    assert_matches!(&types.unwrap_err()[0], TypeError::BinaryOp { .. });
}

#[test]
fn invalid_comparison() {
    parse!(let types = "fn main() {3 > true}");
    assert_matches!(&types.unwrap_err()[0], TypeError::Comparison { .. });
    parse!(let types = "fn main() {3 < true}");
    assert_matches!(&types.unwrap_err()[0], TypeError::Comparison { .. });
    parse!(let types = "fn main() {3 >= true}");
    assert_matches!(&types.unwrap_err()[0], TypeError::Comparison { .. });
    parse!(let types = "fn main() {3 <= true}");
    assert_matches!(&types.unwrap_err()[0], TypeError::Comparison { .. });
    parse!(let types = "fn main() {3 == true}");
    assert_matches!(&types.unwrap_err()[0], TypeError::Comparison { .. });
    parse!(let types = "fn main() {3 != true}");
    assert_matches!(&types.unwrap_err()[0], TypeError::Comparison { .. });
}

#[test]
fn invalid_unary_op() {
    parse!(let types = "fn main() {-true}");
    assert_matches!(&types.unwrap_err()[0], TypeError::UnaryOp { .. });
    parse!(let types = "fn main() {!3}");
    assert_matches!(&types.unwrap_err()[0], TypeError::UnaryOp { .. });
}

#[test]
fn let_spec() {
    parse!(let types = "fn main() {let a: bool = 3;}");
    assert_matches!(&types.unwrap_err()[0], TypeError::LetSpec { .. });
}

#[test]
fn function_arity() {
    parse!(let types = "fn x() {} fn main() {x(1);}");
    assert_matches!(&types.unwrap_err()[0], TypeError::FunctionCall { .. });
    parse!(let types = "fn x(a: int) {} fn main() {x();}");
    assert_matches!(&types.unwrap_err()[0], TypeError::FunctionCall { .. });
    parse!(let types = "fn x(a: int) {} fn main() {x(1);}");
    assert!(types.is_ok());
}

// TODO: this test should pass but now gives completely the wrong error :(
#[test]
fn param_types() {
    // parse!(let types = "fn x(x: bool) {} fn main() {x(1);}");
    // assert_matches!(&types.unwrap_err()[0], TypeError::FunctionCall { .. });
}
