use crate::ast_metadata::{Metadata, WithNodeId};
use crate::parser::ast::TypeSpec;
use crate::parser::error::ParseResult;
use crate::parser::Parser;

impl<'s, 'a> Parser<'s, 'a> {
    pub(super) fn type_spec(&mut self) -> ParseResult<Metadata<TypeSpec<'s>>> {
        let name = self.ident()?;
        let span = name.metadata;

        Ok(TypeSpec::Name(name).with_metadata(span))
    }
}
