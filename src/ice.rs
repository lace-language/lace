use std::error::Error;

pub trait Ice<T> {
    fn ice(self, message: &str) -> T;
}

impl<T> Ice<T> for Option<T> {
    fn ice(self, message: &str) -> T {
        match self {
            None => panic!("ICE: {}", message),
            Some(v) => v,
        }
    }
}

impl<T, E: Error> Ice<T> for Result<T, E> {
    fn ice(self, message: &str) -> T {
        match self {
            Ok(v) => v,
            Err(e) => {
                panic!("ICE: {message} (error was: {e})");
            }
        }
    }
}
