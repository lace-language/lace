use std::error::Error;

macro_rules! ice {
    ($($tt:tt)*) => {
        panic!("ICE {}", format_args!($($tt)*))
    }
}

pub trait Ice<T> {
    fn unwrap_or_ice(self, message: &str) -> T;
}

impl<T> Ice<T> for Option<T> {
    fn unwrap_or_ice(self, message: &str) -> T {
        match self {
            None => {
                ice!("{}", message)
            }
            Some(v) => v,
        }
    }
}

impl<T, E: Error> Ice<T> for Result<T, E> {
    fn unwrap_or_ice(self, message: &str) -> T {
        match self {
            Ok(v) => v,
            Err(e) => {
                ice!("{message} (error was: {e})")
            }
        }
    }
}
