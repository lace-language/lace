use miette::NamedSource;
use crate::ice::Ice;
use crate::parser::span::Span;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct SourceFile<'s> {
    pub contents: &'s str,
    pub filename: &'s str,
}

impl<'s> SourceFile<'s> {
    pub fn named_source(&self) -> NamedSource {
        NamedSource::new(self.filename, self.contents.to_string())
    }

    pub fn slice_span(&self, span: Span) -> &'s str {
        let mut res = self.contents
            .char_indices()
            .skip(span.offset())
            .take(span.length());

        let (start, c) = res.next().unwrap_or_ice("span start not within source");
        match span.length() {
            0 => "",
            1 => &self.contents[start..start+c.len_utf8()],
            _ => {
                let (end, last) = res.last().unwrap_or_ice("span end not within source");
                &self.contents[start..end+last.len_utf8()]
            }
        }
    }
}
