use std::ops::{Deref, DerefMut};

pub trait WithSpan {
    fn with_span(self, span: Span) -> Spanned<Self> where Self: Sized;
}

impl<T> WithSpan for T {
    fn with_span(self, span: Span) -> Spanned<Self> {
        Spanned {
            span,
            value: self,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Spanned<T> {
    span: Span,
    pub value: T,
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T> Spanned<T> {
    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span {
    offset: u32,
    length: u32,
}

impl From<logos::Span> for Span {
    fn from(value: logos::Span) -> Self {
        Self::new(value.start as u32, (value.end - value.start) as u32)
    }
}

impl Span {
    pub fn new(offset: u32, length: u32) -> Self {
        Self::Span {
            offset,
            length,
        }
    }

    /// Merges two spans, creating a new span that encompasses both spans.
    pub fn merge(&self, other: &Self) -> Self {
        let new_offset = self.offset.min(other.offset);

        let end_a = self.offset + self.length;
        let end_b = other.offset + other.length;
        let new_end = end_a.max(end_b);

        let new_length = new_end - new_offset;

        Self {
            offset: new_offset,
            length: new_length,
        }
    }

    pub fn offset(&self) -> usize {
        self.offset as usize
    }

    pub fn length(&self) -> usize {
        self.length as usize
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::span::Span;

    #[test]
    fn merge_ok() {
        let span_a = Span::new(0, 10);
        let span_b = Span::new(10, 10);

        assert_eq!(span_a.merge(&span_b), Some(Span::new(0, 20, SourceID(1))));
    }

    #[test]
    fn merge_different_source() {
        let span_a = Span::new(0, 10, SourceID(0));
        let span_b = Span::new(10, 10, SourceID(1));

        assert_eq!(span_a.merge(&span_b), None);
    }

    #[test]
    fn merge_hole() {
        let span_a = Span::new(0, 10, SourceID(0));
        let span_b = Span::new(20, 10, SourceID(0));

        assert_eq!(span_a.merge(&span_b), Some(Span::new(0, 30, SourceID(0))));
    }

    #[test]
    fn merge_reverse() {
        let span_a = Span::new(10, 10, SourceID(0));
        let span_b = Span::new(30, 10, SourceID(0));

        assert_eq!(span_b.merge(&span_a), Some(Span::new(10, 30, SourceID(0))));
    }
}



