use crate::lexer::token::Token;
use crate::parser::ast::{Block, Expr, ExprKind, Statement};
use crate::parser::error::ParseResult;
use crate::parser::Parser;
use crate::syntax_id::WithNodeId;
use bumpalo::collections::Vec;

use crate::syntax_id::Identified;

impl<'s, 'a> Parser<'s, 'a> {
    fn let_(&mut self) -> ParseResult<Statement<'s, 'a>> {
        self.accept_required(tok![let])?;

        let ident = self.ident()?;

        let type_spec = if self.accept_optional(tok![:])?.is_some() {
            Some(self.type_spec()?)
        } else {
            None
        };

        self.accept_required(tok![=])?;
        let expr = self.expr()?;
        self.accept_required(tok![;])?;

        Ok(Statement::Let(ident, type_spec, self.alloc(expr)))
    }

    pub(super) fn block(&mut self) -> ParseResult<Identified<Block<'s, 'a>>> {
        let start_span = self.accept_required(Token::CurlyLeft)?;

        let mut vec = Vec::new_in(self.arena);

        loop {
            if let Some(end_span) = self.accept_optional(Token::CurlyRight)? {
                return Ok(Block {
                    stmts: vec.into_bump_slice(),
                    last: None,
                }
                .with_node_id(self.spans.store(start_span.merge(&end_span))));
            }
            // TODO: Spans for statements
            if self.peek_is(tok![let])? {
                vec.push(self.let_()?);
            } else if self.peek_is(tok![if])? {
                let expr = self.if_else()?;

                if let Some(end_span) = self.accept_optional(Token::CurlyRight)? {
                    return Ok(Block {
                        stmts: vec.into_bump_slice(),
                        last: Some(expr),
                    }
                    .with_node_id(self.spans.store(start_span.merge(&end_span))));
                }

                self.accept_optional(tok![;])?;
                vec.push(Statement::Expr(self.alloc(expr)));
            } else {
                let expr = self.expr()?;
                if self.accept_optional(tok![;])?.is_some() {
                    vec.push(Statement::Expr(self.alloc(expr)));
                } else {
                    let end_span = self.accept_required(Token::CurlyRight)?;
                    return Ok(Block {
                        stmts: vec.into_bump_slice(),
                        last: Some(expr),
                    }
                    .with_node_id(self.spans.store(start_span.merge(&end_span))));
                }
            }
        }
    }

    pub(super) fn if_else(&mut self) -> ParseResult<Expr<'s, 'a>> {
        let start_span = self.accept_required(tok![if])?;

        let expr = self.expr()?;
        let then_block = self.block()?;

        if self.accept_optional(tok![else])?.is_some() {
            let else_block = if self.peek_is(tok![if])? {
                let else_if = self.if_else()?;
                let span = else_if.span();
                Block {
                    stmts: &[],
                    last: Some(else_if),
                }
                .with_node_id(span)
            } else {
                self.block()?
            };

            let span = self.spans.store_merged(start_span, &else_block);
            Ok(ExprKind::If(
                self.alloc(expr),
                self.alloc(then_block),
                Some(self.alloc(else_block)),
            )
            .with_node_id(span))
        } else {
            let span = self.spans.store_merged(start_span, &then_block);
            Ok(ExprKind::If(self.alloc(expr), self.alloc(then_block), None).with_node_id(span))
        }
    }
}
