use crate::parser::ast::TypeSpec;
use crate::parser::error::ParseResult;
use crate::parser::span::{Spanned, WithSpan};
use crate::parser::Parser;

impl<'s, 'a> Parser<'s, 'a> {
    pub(super) fn type_spec(&mut self) -> ParseResult<Spanned<TypeSpec<'s>>> {
        let name = self.ident()?;
        let span = *name.span();

        Ok(TypeSpec::Name(name).with_span(span))
    }
}
