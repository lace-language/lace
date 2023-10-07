use crate::lexer::Token;
use crate::parser::ast::{Function, Parameter};
use crate::parser::error::ParseResult;
use crate::parser::span::{Spanned, WithSpan};
use crate::parser::Parser;
use bumpalo::collections;

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

        let mut parameters = collections::Vec::new_in(self.arena);
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

    pub(super) fn parse_function(&mut self) -> ParseResult<Spanned<Function<'s, 'a>>> {
        let start_span = self.accept_required(tok![fn])?;

        let name = self.ident()?;
        let parameters = self.parse_parameters()?;
        let ret = if self.accept_optional(tok![->])?.is_some() {
            Some(self.type_spec()?)
        } else {
            None
        };

        let block = self.block()?;
        let fn_span = start_span.merge(&block.span);

        Ok(Function {
            name,
            parameters,
            ret,
            block: self.alloc(block),
        }
        .with_span(fn_span))
    }
}
