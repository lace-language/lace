use crate::parser::ast::{
    Block, ExprKind, File, Function, Ident, Item, Lit, Parameter, Statement, TypeSpec,
};
use crate::parser::span::Spanned;
use crate::parser::Parser;
use bumpalo::Bump;

macro_rules! assert_matches {
    ($expression:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {
        match $expression {
            $pattern $(if $guard)? => {}
            outcome => assert!(false, "expected {:?} to match {}", outcome, stringify!($pattern $(if $guard)?))
        }
    };
}

macro_rules! assert_expr_matches {
    ($source:literal, $pattern:pat $(if $guard:expr)? $(,)?) => {
        let arena = Bump::new();
        let mut p = Parser::new($source, &arena);
        let e = p.expr().unwrap();
        assert_matches!(e, $pattern $(if $guard)?)
    }
}

macro_rules! assert_file_matches {
    ($source:literal, $pattern:pat $(if $guard:expr)? $(,)?) => {
        let arena = Bump::new();
        let mut p = Parser::new($source, &arena);
        let e = p.file().unwrap();
        assert_matches!(e, $pattern $(if $guard)?)
    }
}

macro_rules! spanned {
    ($x:pat) => {
        Spanned { value: $x, .. }
    };
}

macro_rules! int {
    ($i:literal) => {
        spanned!(ExprKind::Lit(Lit::Int(stringify!($i))))
    };
}

macro_rules! string {
    ($i:literal) => {
        spanned!(ExprKind::Lit(Lit::String($i)))
    };
}

macro_rules! expr_ident {
    ($i:ident) => {
        spanned!(ExprKind::Ident(Ident {
            string: stringify!($i),
        }))
    };
}

macro_rules! ident {
    ($i:ident) => {
        spanned!(Ident {
            string: stringify!($i),
        })
    };
}

macro_rules! bool {
    ($i:literal) => {
        spanned!(ExprKind::Lit(Lit::Bool($i)))
    };
}

macro_rules! neg {
    ($x:pat) => {
        spanned!(ExprKind::Neg($x))
    };
}

macro_rules! not {
    ($x:pat) => {
        spanned!(ExprKind::Not($x))
    };
}

macro_rules! and {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::LogicalAnd($x, $y))
    };
}

macro_rules! or {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::LogicalOr($x, $y))
    };
}

macro_rules! add {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::Add($x, $y))
    };
}

macro_rules! sub {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::Sub($x, $y))
    };
}

macro_rules! mul {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::Mul($x, $y))
    };
}

macro_rules! div {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::Div($x, $y))
    };
}

macro_rules! gt {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::Gt($x, $y))
    };
}

macro_rules! gte {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::Gte($x, $y))
    };
}

macro_rules! lt {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::Lt($x, $y))
    };
}

macro_rules! lte {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::Lte($x, $y))
    };
}

macro_rules! eq {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::Eq($x, $y))
    };
}

macro_rules! neq {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::Neq($x, $y))
    };
}

macro_rules! tuple {
    ($($x:pat),*) => {
        spanned!(ExprKind::Tuple(&[$($x),*]))
    };
}

macro_rules! exp {
    ($x:pat) => {
        Statement::Expr($x)
    };
}

macro_rules! let_ {
    ($x:ident, $ty: pat, $exp:pat) => {
        Statement::Let(
            spanned!(Ident {
                string: stringify!($x),
            }),
            Some($ty),
            $exp,
        )
    };

    ($x:ident, $exp:pat) => {
        Statement::Let(
            spanned!(Ident {
                string: stringify!($x),
            }),
            None,
            $exp,
        )
    };
}

macro_rules! call {
    ($callee: pat => [$($arg: pat),*]) => {
        spanned!(ExprKind::Call($callee, spanned!(&[$($arg),*])))
    };
}

macro_rules! block {
    ($($stmts:pat),*) => {
        Block { stmts: &[$($stmts),*], last: None, .. }
    };
    ($($stmts:pat),* => $exp:pat) => {
        Block { stmts: &[$($stmts),*], last: Some($exp), .. }
    };
}

macro_rules! file {
    ($($items:pat),*) => {
        File {
            items: &[$($items),*]
        }
    };
}

macro_rules! item {
    (func: $pat: pat) => {
        Item::Function(spanned!($pat))
    };
}

macro_rules! type_spec {
    (name: $name: ident) => {
        spanned!(TypeSpec::Name(ident!($name)))
    };
}

macro_rules! function {
    (fn $name: ident ($($arg:ident:$ty:pat),*) -> $ret:pat => $block: pat) => {
        Function {
            name: ident!($name),
            parameters: &[$(Parameter {
                name: ident!($arg),
                type_spec: $ty,
            }),*],
            ret: Some($ret),
            block: $block,
        }
    };

    (fn $name: ident ($($arg:ident:$ty:pat),*) => $block: pat) => {
        Function {
            name: ident!($name),
            parameters: &[$(Parameter {
                name: ident!($arg),
                type_spec: $ty,
            }),*],
            ret: None,
            block: $block,
        }
    };
}

macro_rules! block_expr {
    ($($tok:tt)*) =>  {
        spanned!(ExprKind::Block(block!{$($tok)*}))
    }
}

macro_rules! if_ {
    ($cond:pat, $then:pat) => {
        spanned!(ExprKind::If($cond, $then, None))
    };
    ($cond:pat, $then:pat, $else:pat) => {
        spanned!(ExprKind::If($cond, $then, Some($else)))
    };
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
    assert_expr_matches!("(2 + 3) * 4", mul!(add!(int!(2), int!(3)), int!(4)));
}

#[test]
fn tuples() {
    assert_expr_matches!("()", tuple!());
    assert_expr_matches!("(1)", int!(1));
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
    assert_expr_matches!("foo", expr_ident!(foo));
    assert_expr_matches!("bar", expr_ident!(bar));
    assert_expr_matches!("x != y", neq!(expr_ident!(x), expr_ident!(y)));
}

#[test]
fn blocks() {
    assert_expr_matches!("{}", block_expr! {});
    assert_expr_matches!("{ 10 }", block_expr! { => int!(10) });
    assert_expr_matches!("{ 10; }", block_expr! { exp!(int!(10)) });
    assert_expr_matches!(
        "{ 10; 20 }",
        block_expr! {
            exp!(int!(10))
            => int!(20)
        }
    );
    assert_expr_matches!(
        "{ let x = 10; x }",
        block_expr! {
            let_!(x, int!(10))
            => expr_ident!(x)
        }
    );
    assert_expr_matches!(
        "{ let x: int = 10; x }",
        block_expr! {
            let_!(x, type_spec!(name: int), int!(10))
            => expr_ident!(x)
        }
    );
    assert_expr_matches!(
        "{ if 1 { 2 } 3 }",
        block_expr!(exp!(if_!(int!(1), block!(=> int!(2)))) => int!(3))
    );
    assert_expr_matches!(
        "{ if 1 { 2 }; 3 }",
        block_expr!(exp!(if_!(int!(1), block!(=> int!(2)))) => int!(3))
    );
    assert_expr_matches!(
        "{ if 1 { 2 } else { 3 } 4 }",
        block_expr!(
            exp!(if_!(int!(1), block!(=> int!(2)), block!(=> int!(3))))
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
    assert_expr_matches!("a()", call!(expr_ident!(a) => []));
    assert_expr_matches!("a(1, 2, 3)", call!(
        expr_ident!(a) => [int!(1), int!(2), int!(3)])
    );
    assert_expr_matches!("a(1, 2, 3,)", call!(
        expr_ident!(a) => [int!(1), int!(2), int!(3)])
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
                let_!(a, int!(3))
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
                let_!(c, add!(expr_ident!(a), expr_ident!(b))) => expr_ident!(c)
            )
        }), item!(func: function! {
            fn main() => block!(
                Statement::Expr(call!(print => [call!(add => [int!(1), int!(2)])]))
            )
        })}
    );
}
