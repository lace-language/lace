use crate::parser::ast::TypeSpec;
use crate::parser::error::ParseResult;
use crate::parser::Parser;
use crate::syntax_id::{Identified, WithNodeId};

impl<'s, 'a> Parser<'s, 'a> {
    pub(super) fn type_spec(&mut self) -> ParseResult<Identified<TypeSpec<'s>>> {
        let name = self.ident()?;
        let span = name.span();

        Ok(TypeSpec::Name(name).with_node_id(span))
    }
}
