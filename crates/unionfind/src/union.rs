use std::convert::Infallible;

pub trait Union<T> {
    type Err;

    fn union(&mut self, a: T, b: T) -> Result<T, Self::Err>;
}

impl<F, T> Union<T> for F
where
    F: FnMut(T, T) -> T,
{
    type Err = Infallible;

    fn union(&mut self, a: T, b: T) -> Result<T, Self::Err> {
        Ok((self)(a, b))
    }
}
