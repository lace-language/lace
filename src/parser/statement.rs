use crate::lexer::Token;
use crate::parser::ast::{Block, Expr, ExprKind, Statement};
use crate::parser::error::ParseResult;
use crate::parser::span::WithSpan;
use crate::parser::Parser;
use bumpalo::collections::Vec;

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

    pub(super) fn block(&mut self) -> ParseResult<Block<'s, 'a>> {
        let open_span = self.accept_required(Token::CurlyLeft)?;

        let mut vec = Vec::new_in(self.arena);

        loop {
            if let Some(close_span) = self.accept_optional(Token::CurlyRight)? {
                return Ok(Block {
                    stmts: vec.into_bump_slice(),
                    last: None,
                    span: open_span.merge(&close_span),
                });
            }
            // TODO: Spans for statements
            if self.peek_is(tok![let])? {
                vec.push(self.let_()?);
            } else if self.peek_is(tok![if])? {
                let expr = self.if_else()?;

                if let Some(close_span) = self.accept_optional(Token::CurlyRight)? {
                    return Ok(Block {
                        stmts: vec.into_bump_slice(),
                        last: Some(expr),
                        span: open_span.merge(&close_span),
                    });
                }

                self.accept_optional(tok![;])?;
                vec.push(Statement::Expr(self.alloc(expr)));
            } else {
                let expr = self.expr()?;
                if self.accept_optional(tok![;])?.is_some() {
                    vec.push(Statement::Expr(self.alloc(expr)));
                } else {
                    let close_span = self.accept_required(Token::CurlyRight)?;
                    return Ok(Block {
                        stmts: vec.into_bump_slice(),
                        last: Some(expr),
                        span: open_span.merge(&close_span),
                    });
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
                let span = else_if.span;
                Block {
                    stmts: &[],
                    last: Some(else_if),
                    span,
                }
            } else {
                self.block()?
            };

            let span = start_span.merge(&else_block.span);
            Ok(ExprKind::If(
                self.alloc(expr),
                self.alloc(then_block),
                Some(self.alloc(else_block)),
            )
            .with_span(span))
        } else {
            let span = start_span.merge(&then_block.span);
            Ok(ExprKind::If(self.alloc(expr), self.alloc(then_block), None).with_span(span))
        }
    }
}
