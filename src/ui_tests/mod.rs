use bumpalo::Bump;
use crate::compile;
use crate::source_file::SourceFile;

macro_rules! parse_test {
    ($name: ident) => {
        #[test]
        fn unrecognised_token() {
            let bump = Bump::new();

            let res= compile(
                SourceFile::new(include_str!(concat!("parse/", stringify!($name),".lc")), concat!("parse/", stringify!($name),".lc")),
                &bump
            ).expect_err("expected error: unrecognised token");
            let report = format!("{:?}", res);
            let expected = include_str!(concat!("parse/", stringify!($name),".out"));

            assert_eq!(report, expected);
        }
    };
}

parse_test!(invalid_token);