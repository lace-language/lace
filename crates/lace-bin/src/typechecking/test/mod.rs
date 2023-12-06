use crate::error::{CompilerError, ResultExt};
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

pub fn typecheck_test_ok(source: SourceFile<'_>) {
    typecheck_test(source, |res| {
        if res.is_err() {
            let err = res
                .as_ref()
                .map_err(|t| CompilerError::Type(t.clone().into()))
                .map_err_miette(source)
                .unwrap_err();

            assert!(false, "unexpected error:\n{err}\n({:?})", res.unwrap_err());
        }
    })
}

#[test]
fn invalid_binop_usage() {
    let binop = |e| assert_matches!(e, TypeError::ExactBinaryOp { .. });
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

#[test]
fn func_eq() {
    typecheck_test_one_error(
        SourceFile::test("fn a() {} fn b() {} fn main() { a == b; }"),
        |e| assert_matches!(e, TypeError::Comparison { .. }),
    );

    typecheck_test_ok(SourceFile::test("fn a() {} fn main() { a == a; }"))
}

#[test]
fn if_test() {
    typecheck_test_ok(SourceFile::test(
        "fn main() {
if true {
    3;
} else {
    3;
}
    }",
    ));
    typecheck_test_ok(SourceFile::test(
        "fn main() {
let ans = if true {
    3
} else {
    3
};
    }",
    ));

    typecheck_test_ok(SourceFile::test(
        "fn main() {
let ans: int = if true {
    3
} else {
    3
};
    }",
    ));

    typecheck_test_one_error(
        SourceFile::test(
            "fn main() {
let ans: int = if true {
    3
} else {
    true
};
    }",
        ),
        |e| {
            assert_matches!(
                e,
                TypeError::IfElseEqual {
                    if_true_return_span: Some(_),
                    if_false_return_span: Some(_),
                    ..
                }
            )
        },
    );

    typecheck_test_one_error(
        SourceFile::test(
            "fn main() {
let ans: int = if true {
    3
} else {
    true;
};
    }",
        ),
        |e| {
            assert_matches!(
                e,
                TypeError::IfElseEqual {
                    if_true_return_span: Some(_),
                    if_false_return_span: None,
                    ..
                }
            )
        },
    );

    typecheck_test_one_error(
        SourceFile::test(
            "fn main() {
let ans: bool = if true {
    3;
} else {
    true
};
    }",
        ),
        |e| {
            assert_matches!(
                e,
                TypeError::IfElseEqual {
                    if_true_return_span: None,
                    if_false_return_span: Some(_),
                    ..
                }
            )
        },
    );

    typecheck_test_ok(SourceFile::test(
        "fn main() {
let ans = if true {
    3;
} else {
    true;
};
    }",
    ));

    typecheck_test_ok(SourceFile::test(
        "fn main() {
let ans: int = if true {
    3
} else {
    3
};
    }",
    ));

    typecheck_test_one_error(
        SourceFile::test(
            "fn main() {
let ans: bool = if true {
    3
} else {
    3
};
    }",
        ),
        |e| assert_matches!(e, TypeError::LetSpec { .. }),
    );
    typecheck_test_one_error(
        SourceFile::test(
            "fn main() {
let ans: bool = if true {
    3;
} else {
    3;
};
    }",
        ),
        |e| assert_matches!(e, TypeError::LetSpec { .. }),
    );
}
