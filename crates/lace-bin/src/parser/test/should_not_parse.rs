use crate::parser::error::ParseError;

macro_rules! should_error_expr {
    ($name: ident, $source: literal $(if $guard: expr)?) => {
        should_error_expr!(@ $name, $source, Err(_) $(if $guard)?);
    };
    ($name: ident, $source: literal, $pattern: pat $(if $guard: expr)?) => {
        should_error_expr!(@ $name, $source, Err($pattern) $(if $guard)?);
    };
    (@$name: ident, $source: literal, $pattern: pat $(if $guard: expr)?) => {
        #[test]
        fn $name () {
            $crate::parser::test::parse_test_helper::parse_expr_test(
                $crate::source_file::SourceFile{contents: $source, filename: "test.lc"},
                |expr| {
                    assert_matches!(expr, $pattern $(if $guard)?)
                }
            )
        }
    };
}

macro_rules! should_error_file {
    ($name: ident, $source: literal $(if $guard: expr)?) => {
        should_error_file!(@ $name, $source, Err(_) $(if $guard)?);
    };
    ($name: ident, $source: literal, $pattern: pat $(if $guard: expr)?) => {
        should_error_file!(@ $name, $source, Err($pattern) $(if $guard)?);
    };
    (@$name: ident, $source: literal, $pattern: pat $(if $guard: expr)?) => {
        #[test]
        fn $name () {
            $crate::parser::test::parse_test_helper::parse_file_test(
                $crate::source_file::SourceFile{contents: $source, filename: "test.lc"},
                |file| {
                    assert_matches!(file, $pattern $(if $guard)?)
                }
            )
        }
    };
}

should_error_expr!(
    repeated_equal,
    "a == b == c",
    ParseError::IncompatibleBinaryOp { .. }
);
should_error_expr!(
    repeated_greater,
    "a > b > c",
    ParseError::IncompatibleBinaryOp { .. }
);
should_error_expr!(
    repeated_less,
    "a < b < c",
    ParseError::IncompatibleBinaryOp { .. }
);
should_error_expr!(
    repeated_greater_equal,
    "a >= b >= c",
    ParseError::IncompatibleBinaryOp { .. }
);
should_error_expr!(
    repeated_less_equal,
    "a <= b <= c",
    ParseError::IncompatibleBinaryOp { .. }
);
should_error_expr!(
    repeated_not_equal,
    "a != b != c",
    ParseError::IncompatibleBinaryOp { .. }
);
should_error_expr!(
    repeated_mixed_1,
    "a >= b == c",
    ParseError::IncompatibleBinaryOp { .. }
);
should_error_expr!(
    repeated_mixed_2,
    "a != b == c",
    ParseError::IncompatibleBinaryOp { .. }
);

// TODO: make and test for nicer errors that refer to other languages that do
//       have these operators
should_error_expr!(increment, "a ++", ParseError::Expected { .. });
should_error_expr!(integer_divide, "a // b", ParseError::Expected { .. });

// TODO: make and test for nicer errors that refer to other languages that do
//       have these keywords
should_error_file!(go_func, "func x() {}", ParseError::Expected { .. });
should_error_file!(js_function, "function x() {}", ParseError::Expected { .. });
should_error_file!(kotlin_fun, "fun x() {}", ParseError::Expected { .. });
should_error_file!(python_def, "def x() {}", ParseError::Expected { .. });
