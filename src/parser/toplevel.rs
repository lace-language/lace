use crate::parser::ast::{File, Item};
use crate::parser::error::{ParseError, ParseResult};
use crate::parser::Parser;
use crate::parser::Token;
use bumpalo::collections;

impl<'s, 'a> Parser<'s, 'a> {
    pub fn file(&mut self) -> ParseResult<File<'s, 'a>> {
        let mut items = collections::Vec::new_in(self.arena);

        while self.has_next()? {
            items.push(self.item()?);
        }

        Ok(File {
            items: items.into_bump_slice(),
        })
    }

    fn item(&mut self) -> ParseResult<Item<'s, 'a>> {
        if self.peek_is(tok![fn])? {
            Ok(Item::Function(self.parse_function()?))
        } else {
            Err(ParseError::Expected)
        }
    }
}
