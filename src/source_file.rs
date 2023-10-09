use miette::NamedSource;

#[derive(Copy, Clone, Debug)]
pub struct SourceFile<'s> {
    pub contents: &'s str,
    pub filename: &'s str,
}

impl<'s> SourceFile<'s> {
    pub fn new(contents: &'s str, filename: &'s str) -> Self {
        Self {
            contents,
            filename,
        }
    }

    pub fn named_source(&self) -> NamedSource {
        NamedSource::new(self.filename, self.contents.to_string())
    }
}
