use crate::parser::ast::*;

macro_rules! assert_expr_matches {
    ($source:literal, $pattern:pat $(if $guard:expr)? $(,)?) => {
        $crate::parser::test::parse_test_helper::parse_expr_test(
            $crate::source_file::SourceFile{contents: $source, filename: "test.lc"},
            |expr| {
                assert_matches!(expr.unwrap(), $pattern $(if $guard)?)
            }
        )
    }
}

macro_rules! assert_file_matches {
    ($source:literal, $pattern:pat $(if $guard:expr)? $(,)?) => {
        $crate::parser::test::parse_test_helper::parse_file_test(
            $crate::source_file::SourceFile{contents: $source, filename: "test.lc"},
            |file| {
                assert_matches!(file.unwrap(), $pattern $(if $guard)?)
            }
        )
    }
}

#[test]
fn booleans() {
    assert_expr_matches!("false", bool!(false));
    assert_expr_matches!("true", bool!(true));
}

#[test]
fn integers() {
    assert_expr_matches!("10", int!(10));
    assert_expr_matches!("0", int!(0));
    assert_expr_matches!("0054", int!(0054));
    assert_expr_matches!("1_2_3_4_5", int!(1_2_3_4_5));
}

#[test]
fn unary() {
    assert_expr_matches!("- 2", neg!(int!(2)));
    assert_expr_matches!("-- 2", neg!(neg!(int!(2))));
    assert_expr_matches!("- 2 + 3", add!(neg!(int!(2)), int!(3)));
}

#[test]
fn logical() {
    assert_expr_matches!("!true", not!(bool!(true)));
    assert_expr_matches!("!false", not!(bool!(false)));
    assert_expr_matches!("true && false", and!(bool!(true), bool!(false)));
    assert_expr_matches!(
        "true && false && false",
        and!(and!(bool!(true), bool!(false)), bool!(false))
    );
    assert_expr_matches!("true || false", or!(bool!(true), bool!(false)));
    assert_expr_matches!(
        "true || false || false",
        or!(or!(bool!(true), bool!(false)), bool!(false))
    );
    assert_expr_matches!(
        "true && false || false",
        or!(and!(bool!(true), bool!(false)), bool!(false))
    );
    assert_expr_matches!(
        "true || false && false",
        or!(bool!(true), and!(bool!(false), bool!(false)))
    );
    assert_expr_matches!(
        "true || !false && false",
        or!(bool!(true), and!(not!(bool!(false)), bool!(false)))
    );
}

#[test]
fn inversion_and_arithmetic() {
    assert_expr_matches!("!false + false", add!(not!(bool!(false)), bool!(false)));
    assert_expr_matches!("!false - false", sub!(not!(bool!(false)), bool!(false)));
}

#[test]
fn arithmetic_binop() {
    assert_expr_matches!("2 + 3", add!(int!(2), int!(3)));
    assert_expr_matches!("2 + 3 + 4", add!(add!(int!(2), int!(3)), int!(4)));
    assert_expr_matches!("2 - 3", sub!(int!(2), int!(3)));
    assert_expr_matches!("2 - 3 - 4", sub!(sub!(int!(2), int!(3)), int!(4)));
    assert_expr_matches!("2 * 3", mul!(int!(2), int!(3)));
    assert_expr_matches!("2 * 3 * 4", mul!(mul!(int!(2), int!(3)), int!(4)));
    assert_expr_matches!("2 / 3", div!(int!(2), int!(3)));
    assert_expr_matches!("2 / 3 / 4", div!(div!(int!(2), int!(3)), int!(4)));
    assert_expr_matches!(
        "2 / 3 + 4 * 5",
        add!(div!(int!(2), int!(3)), mul!(int!(4), int!(5)))
    );
    assert_expr_matches!("2 + 3 * 4", add!(int!(2), mul!(int!(3), int!(4))));
    assert_expr_matches!("(2 + 3) * 4", mul!(paren!(add!(int!(2), int!(3))), int!(4)));
}

#[test]
fn tuples() {
    assert_expr_matches!("()", tuple!());
    assert_expr_matches!("(1)", paren!(int!(1)));
    assert_expr_matches!("(1,)", tuple!(int!(1)));
    assert_expr_matches!("(1,2)", tuple!(int!(1), int!(2)));
    assert_expr_matches!("(1,2,)", tuple!(int!(1), int!(2)));
    assert_expr_matches!("(1,2,3)", tuple!(int!(1), int!(2), int!(3)));
    assert_expr_matches!("((1,2,3),)", tuple!(tuple!(int!(1), int!(2), int!(3))));
}

#[test]
fn comparisons() {
    assert_expr_matches!("1 <= 2", lte!(int!(1), int!(2)));
    assert_expr_matches!("1 >= 2", gte!(int!(1), int!(2)));
    assert_expr_matches!("1 < 2", lt!(int!(1), int!(2)));
    assert_expr_matches!("1 > 2", gt!(int!(1), int!(2)));
    assert_expr_matches!("1 == 2", eq!(int!(1), int!(2)));
    assert_expr_matches!("1 != 2", neq!(int!(1), int!(2)));
}

#[test]
fn ident() {
    assert_expr_matches!("foo", ident_expr!(foo));
    assert_expr_matches!("bar", ident_expr!(bar));
    assert_expr_matches!("x != y", neq!(ident_expr!(x), ident_expr!(y)));
}

#[test]
fn blocks() {
    assert_expr_matches!("{}", block_expr! {});
    assert_expr_matches!("{ 10 }", block_expr! { => int!(10) });
    assert_expr_matches!("{ 10; }", block_expr! { stmt!(int!(10)) });
    assert_expr_matches!(
        "{ 10; 20 }",
        block_expr! {
            stmt!(int!(10))
            => int!(20)
        }
    );
    assert_expr_matches!(
        "{ let x = 10; x }",
        block_expr! {
            stmt!(let: x, int!(10))
            => ident_expr!(x)
        }
    );
    assert_expr_matches!(
        "{ let x: int = 10; x }",
        block_expr! {
            stmt!(let: x, type_spec!(name: int), int!(10))
            => ident_expr!(x)
        }
    );
    assert_expr_matches!(
        "{ if 1 { 2 } 3 }",
        block_expr!(stmt!(if_!(int!(1), block!(=> int!(2)))) => int!(3))
    );
    assert_expr_matches!(
        "{ if 1 { 2 }; 3 }",
        block_expr!(stmt!(if_!(int!(1), block!(=> int!(2)))) => int!(3))
    );
    assert_expr_matches!(
        "{ if 1 { 2 } else { 3 } 4 }",
        block_expr!(
            stmt!(if_!(int!(1), block!(=> int!(2)), block!(=> int!(3))))
            => int!(4)
        )
    );
}

#[test]
fn if_else() {
    assert_expr_matches!(
        "if 1 { 2 } else { 3 }",
        if_!(int!(1), block!(=> int!(2)), block!(=> int!(3))),
    );
    assert_expr_matches!(
        "if 1 { 2 } else { 3 } + 4",
        add!(
            if_!(int!(1), block!(=> int!(2)), block!(=> int!(3))),
            int!(4)
        )
    );
    assert_expr_matches!("if 1 { 2 }", if_!(int!(1), block!(=> int!(2))),);
    assert_expr_matches!(
        "if 1 { 2 } else if 3 { 4 } else { 5 }",
        if_!(
            int!(1),
            block!(=> int!(2)),
            block!(=> if_!(int!(3), block!(=> int!(4)), block!(=> int!(5))))
        ),
    );
}

#[test]
fn strings() {
    assert_expr_matches!("\"Hello, world!\"", string!("\"Hello, world!\""));
    assert_expr_matches!("\"\"", string!("\"\""));
}

#[test]
fn call() {
    assert_expr_matches!("a()", call!(ident_expr!(a) => []));
    assert_expr_matches!(
        "a(1)(2)",
        call!(call!(ident_expr!(a) => [int!(1)]) => [int!(2)])
    );
    assert_expr_matches!("-a(1)", neg!(call!(ident_expr!(a) => [int!(1)])));
    assert_expr_matches!(
        "a(1, 2, 3)",
        call!(
        ident_expr!(a) => [int!(1), int!(2), int!(3)])
    );
    assert_expr_matches!(
        "a(1, 2, 3,)",
        call!(
        ident_expr!(a) => [int!(1), int!(2), int!(3)])
    );
}

#[test]
fn function() {
    assert_file_matches!(
        "fn text() {}",
        file! {item!(func: function! {
            fn text() => block!()
        })}
    );
    assert_file_matches!(
        "fn text(a: int) {}",
        file! {item!(func: function! {
            fn text(a: type_spec!(name: int)) => block!()
        })}
    );
    assert_file_matches!(
        "fn text(a: int, b: int) {}",
        file! {item!(func: function! {
            fn text(a: type_spec!(name: int), b: type_spec!(name: int)) => block!()
        })}
    );
    assert_file_matches!(
        "fn text(a: int, b: int,) {}",
        file! {item!(func: function! {
            fn text(a: type_spec!(name: int), b: type_spec!(name: int)) => block!()
        })}
    );
    assert_file_matches!(
        "fn text(a: int, b: int,) -> int {}",
        file! {item!(func: function! {
            fn text(a: type_spec!(name: int), b: type_spec!(name: int)) -> type_spec!(name: int) => block!()
        })}
    );
    assert_file_matches!(
        "fn text() {
        let a = 3;
    }",
        file! {item!(func: function! {
            fn text() => block!(
                stmt!(let: a, int!(3))
            )
        })}
    );
    assert_file_matches!(
        "
    fn add(a: int, b: int,) -> int {
        let c = a + b;
        c
    }

    fn main() {
        print(add(1, 2));
    }
    ",
        file! {item!(func: function! {
            fn add(a: type_spec!(name: int), b: type_spec!(name: int)) -> type_spec!(name: int) => block!(
                stmt!(let: c, add!(ident_expr!(a), ident_expr!(b))) => ident_expr!(c)
            )
        }), item!(func: function! {
            fn main() => block!(
                Statement::Expr(call!(ident_expr!(print) => [call!(ident_expr!(add) => [int!(1), int!(2)])]))
            )
        })}
    );
}
