use miette::NamedSource;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct SourceFile<'s> {
    pub contents: &'s str,
    pub filename: &'s str,
}

impl<'s> SourceFile<'s> {
    pub fn new(filename: &'s str, contents: &'s str) -> Self {
        Self { contents, filename }
    }

    pub fn named_source(&self) -> NamedSource {
        NamedSource::new(self.filename, self.contents.to_string())
    }
}

impl<'s> From<(&'s str, &'s str)> for SourceFile<'s> {
    fn from((name, contents): (&'s str, &'s str)) -> Self {
        Self::new(name, contents)
    }
}
