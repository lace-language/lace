use miette::{Diagnostic, Report};
use crate::source_file::SourceFile;

pub trait ToMiette<T> {
    fn to_miette(self, source: SourceFile) -> miette::Result<T>;
}

impl<T, E> ToMiette<T> for Result<T, E>
    where E: Diagnostic + Send + Sync + 'static
{
    fn to_miette(self, source: SourceFile) -> miette::Result<T> {
        self.map_err(|e| {
            Report::new(e)
                .with_source_code(source.named_source())
        })
    }
}