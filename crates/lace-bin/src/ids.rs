use std::sync::atomic::{AtomicUsize, Ordering};

pub struct IdGenerator<T, F: Fn(usize) -> T> {
    curr: AtomicUsize,
    f: F,
}

impl<T, F> IdGenerator<T, F>
where
    F: Fn(usize) -> T,
{
    pub fn new(f: F) -> Self {
        Self {
            curr: AtomicUsize::new(0),
            f,
        }
    }

    pub fn fresh(&self) -> T {
        (self.f)(self.curr.fetch_add(1, Ordering::Relaxed))
    }
}
