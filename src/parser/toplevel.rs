use crate::lexer::token::Token;
use crate::parser::ast::{File, Item};
use crate::parser::error::{ParseError, ParseResult};
use crate::parser::Parser;
use bumpalo::collections::Vec;

impl<'s, 'a> Parser<'s, 'a> {
    pub fn file(&mut self) -> ParseResult<File<'s, 'a>> {
        let mut items = Vec::new_in(self.arena);

        while self.has_next() {
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
            let (token, span) = self.next()?;
            Err(ParseError::Expected {
                expected: "an item, such as a function".into(),
                got: token.to_string(),
                span,
            })
        }
    }
}
