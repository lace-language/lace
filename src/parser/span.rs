use std::ops::{Deref, DerefMut};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NodeId(usize);

pub struct Spans(Vec<Span>);

impl Spans {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn store(&mut self, span: Span) -> NodeId {
        self.0.push(span);
        NodeId(self.0.len() - 1)
    }

    pub fn merge<A, B>(
        &mut self,
        &Spanned {
            span: NodeId(a), ..
        }: &Spanned<A>,
        &Spanned {
            span: NodeId(b), ..
        }: &Spanned<B>,
    ) -> NodeId {
        let a = self.0[a];
        let b = self.0[b];
        self.store(a.merge(&b))
    }

    pub fn store_merged<T>(
        &mut self,
        span: Span,
        &Spanned {
            span: NodeId(a), ..
        }: &Spanned<T>,
    ) -> NodeId {
        let a = self.0[a];
        self.store(span.merge(&a))
    }
}

pub trait WithSpan {
    fn with_span(self, span: NodeId) -> Spanned<Self>
    where
        Self: Sized;
}

impl<T> WithSpan for T {
    fn with_span(self, span: NodeId) -> Spanned<Self> {
        Spanned { span, value: self }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: NodeId,
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
    pub fn span(&self) -> NodeId {
        self.span
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    offset: u32,
    length: u32,
}

impl From<Span> for miette::SourceSpan {
    fn from(span: Span) -> Self {
        Self::from((span.offset(), span.length()))
    }
}

impl From<logos::Span> for Span {
    fn from(value: logos::Span) -> Self {
        Self::new(value.start as u32, (value.end - value.start) as u32)
    }
}

impl Span {
    pub fn new(offset: u32, length: u32) -> Self {
        Self { offset, length }
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

    #[allow(dead_code)]
    pub fn offset(&self) -> usize {
        self.offset as usize
    }

    #[allow(dead_code)]
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

        assert_eq!(span_a.merge(&span_b), Span::new(0, 20));
    }

    #[test]
    fn merge_hole() {
        let span_a = Span::new(0, 10);
        let span_b = Span::new(20, 10);

        assert_eq!(span_a.merge(&span_b), Span::new(0, 30));
    }

    #[test]
    fn merge_reverse() {
        let span_a = Span::new(10, 10);
        let span_b = Span::new(30, 10);

        assert_eq!(span_b.merge(&span_a), Span::new(10, 30));
    }
}
