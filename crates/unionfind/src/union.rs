use std::convert::Infallible;

pub trait Union<T> {
    type Err;

    fn union(&self, a: T, b: T) -> Result<T, Self::Err>;
}

impl<F, T> Union<T> for F
where
    F: Fn(T, T) -> T,
{
    type Err = Infallible;

    fn union(&self, a: T, b: T) -> Result<T, Self::Err> {
        Ok((self)(a, b))
    }
}
