use std::marker::PhantomData;
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct IdGenerator<T> {
    curr: AtomicUsize,
    phantom: PhantomData<T>,
}

impl<T> IdGenerator<T>
where
    T: From<usize>,
{
    pub fn new() -> Self {
        Self {
            curr: AtomicUsize::new(0),
            phantom: PhantomData,
        }
    }

    pub fn fresh(&self) -> T {
        self.curr.fetch_add(1, Ordering::Relaxed).into()
    }

    pub fn fresh_mut(&mut self) -> T {
        let val = self.curr.get_mut();
        let res = (*val).into();
        *val += 1;

        res
    }
}

impl<T> Iterator for IdGenerator<T>
where
    T: From<usize>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.fresh_mut())
    }
}
