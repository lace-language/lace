use crate::ast_metadata::{Metadata, WithNodeId};
use crate::lexer::token::Token;
use crate::parser::ast::{Function, Parameter};
use crate::parser::error::ParseResult;
use crate::parser::Parser;
use bumpalo::collections::Vec;

impl<'s, 'a> Parser<'s, 'a> {
    fn parse_parameter(&mut self) -> ParseResult<Parameter<'s>> {
        let name = self.ident()?;
        self.accept_required(tok![:])?;
        let type_spec = self.type_spec()?;

        Ok(Parameter { name, type_spec })
    }

    fn parse_parameters(&mut self) -> ParseResult<&'a [Parameter<'s>]> {
        self.accept_required(Token::RoundLeft)?;

        // empty parameters list
        if self.accept_optional(Token::RoundRight)?.is_some() {
            return Ok(&[]);
        }

        let mut parameters = Vec::new_in(self.arena);
        parameters.push(self.parse_parameter()?);

        while self.accept_optional(tok![,])?.is_some() {
            // for trailing comma
            if self.accept_optional(Token::RoundRight)?.is_some() {
                return Ok(parameters.into_bump_slice());
            }

            parameters.push(self.parse_parameter()?);
        }

        self.accept_required(Token::RoundRight)?;

        Ok(parameters.into_bump_slice())
    }

    pub(super) fn parse_function(&mut self) -> ParseResult<Metadata<Function<'s, 'a>>> {
        let start_span = self.accept_required(tok![fn])?;

        let name = self.ident()?;
        let parameters = self.parse_parameters()?;
        let ret = if self.accept_optional(tok![->])?.is_some() {
            Some(self.type_spec()?)
        } else {
            None
        };

        let block = self.block()?;
        let fn_span = self.spans.store_merged(start_span, &block);

        Ok(Function {
            name,
            parameters,
            ret,
            block: self.alloc(block),
        }
        .with_metadata(fn_span))
    }
}
