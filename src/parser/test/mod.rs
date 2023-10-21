macro_rules! assert_matches {
    ($expression:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {
        match $expression {
            $pattern $(if $guard)? => {}
            outcome => assert!(false, "expected {:?} to match {}", outcome, stringify!($pattern $(if $guard)?))
        }
    };
}

#[macro_use]
mod ast_macros;

mod should_not_parse;
mod should_parse;
