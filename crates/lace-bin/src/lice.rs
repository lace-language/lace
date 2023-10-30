use std::error::Error;

macro_rules! lice {
    ($($tt:tt)*) => {
        panic!("LICE (bug) {}", format_args!($($tt)*))
    }
}

pub trait Lice<T> {
    fn unwrap_or_lice(self, message: &str) -> T;
}

impl<T> Lice<T> for Option<T> {
    fn unwrap_or_lice(self, message: &str) -> T {
        match self {
            None => {
                lice!("{}", message)
            }
            Some(v) => v,
        }
    }
}

impl<T, E: Error> Lice<T> for Result<T, E> {
    fn unwrap_or_lice(self, message: &str) -> T {
        match self {
            Ok(v) => v,
            Err(e) => {
                lice!("{message} (error was: {e})")
            }
        }
    }
}
