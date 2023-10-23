use miette::NamedSource;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct SourceFile<'s> {
    pub contents: &'s str,
    pub filename: &'s str,
}

impl<'s> SourceFile<'s> {
    pub fn named_source(&self) -> NamedSource {
        NamedSource::new(self.filename, self.contents.to_string())
    }
}
