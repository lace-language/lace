use crate::error::ResultExt;
use crate::lexer::token_buffer::TokenBuffer;
use crate::name_resolution::Graph;
use crate::parser::Parser;
use crate::source_file::SourceFile;
use crate::typechecking::error::TypeError;
use crate::typechecking::solved::SolvedTypes;
use crate::typechecking::typecheck;
use bumpalo::Bump;

pub fn typecheck_test<'s>(
    source: SourceFile<'s>,
    test: impl for<'a> FnOnce(Result<SolvedTypes, Vec<TypeError>>) + 's,
) {
    let bump = Bump::new();
    let token_buffer = TokenBuffer::from_source(source).unwrap();
    let parser = Parser::new(token_buffer, &bump);
    let (spans, ast) = parser.parse().map_err_miette(source).unwrap();

    let mut graph = Graph::new(source.filename);
    let resolved = graph.resolve(&ast);

    let type_arena = Bump::new();
    let types = typecheck(&ast, &resolved, &spans, &type_arena);

    test(types);
}

pub fn typecheck_test_one_error<'s>(
    source: SourceFile<'s>,
    test: impl for<'a> FnOnce(TypeError) + 's,
) {
    typecheck_test(source, |res| {
        assert!(res.is_err(), "{:?}", res.unwrap());
        let errs = res.unwrap_err();
        assert_eq!(errs.len(), 1);

        test(errs.into_iter().next().unwrap())
    })
}

#[test]
fn invalid_binop_usage() {
    let binop = |e| assert_matches!(e, TypeError::BinaryOp { .. });
    typecheck_test_one_error(SourceFile::test("fn main() {3 + true}"), binop);
    typecheck_test_one_error(SourceFile::test("fn main() {3 * true}"), binop);
    typecheck_test_one_error(SourceFile::test("fn main() {3 - true}"), binop);
    typecheck_test_one_error(SourceFile::test("fn main() {3 / true}"), binop);
    typecheck_test_one_error(SourceFile::test("fn main() {3 || true}"), binop);
    typecheck_test_one_error(SourceFile::test("fn main() {3 && true}"), binop);
}

#[test]
fn invalid_comparison() {
    let comparison = |e| assert_matches!(e, TypeError::Comparison { .. });
    typecheck_test_one_error(SourceFile::test("fn main() {3 > true}"), comparison);
    typecheck_test_one_error(SourceFile::test("fn main() {3 < true}"), comparison);
    typecheck_test_one_error(SourceFile::test("fn main() {3 >= true}"), comparison);
    typecheck_test_one_error(SourceFile::test("fn main() {3 <= true}"), comparison);
    typecheck_test_one_error(SourceFile::test("fn main() {3 == true}"), comparison);
    typecheck_test_one_error(SourceFile::test("fn main() {3 != true}"), comparison);
}

#[test]
fn invalid_unary_op() {
    let unop = |e| assert_matches!(e, TypeError::UnaryOp { .. });
    typecheck_test_one_error(SourceFile::test("fn main() {-true}"), unop);
    typecheck_test_one_error(SourceFile::test("fn main() {!3}"), unop);
}

#[test]
fn let_spec() {
    typecheck_test_one_error(SourceFile::test("fn main() {let a: bool = 3;}"), |e| {
        assert_matches!(e, TypeError::LetSpec { .. })
    });
}

#[test]
fn function_arity() {
    let call = |e| assert_matches!(e, TypeError::FunctionCall { .. });
    typecheck_test_one_error(SourceFile::test("fn x() {} fn main() {x(1);}"), call);
    typecheck_test_one_error(SourceFile::test("fn x(a: int) {} fn main() {x();}"), call);
    typecheck_test(SourceFile::test("fn x(a: int) {} fn main() {x(1);}"), |t| {
        assert!(t.is_ok())
    });
}

// TODO: this test should pass but now gives completely the wrong error :(
#[ignore]
#[test]
fn param_types() {
    typecheck_test_one_error(
        SourceFile::test("fn x(x: bool) {} fn main() {x(1);}"),
        |e| assert_matches!(e, TypeError::FunctionCall { .. }),
    );
}
