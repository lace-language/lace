use crate::lowering::basic_block::{BasicBlockBuilder, BlockList};
use crate::lowering::lir;
use crate::lowering::lir::Label;
use std::mem;

impl<'l, 'b, 't, 'a, 'n> BasicBlockBuilder<'l, 'b, 't, 'a, 'n> {
    fn wrap_up_current(&mut self, t: lir::Terminator) {
        self.blocks.push(lir::BasicBlock {
            name: self.current_label,
            stmts: mem::take(&mut self.current_statements),
            terminator: t,
        });
    }

    /// Emitting a terminator ends the current basic block, and adds it to the
    /// finished basic blocks field of this basic block builder. The new basic
    /// block that is started will get the label `next_block_label`. The reason
    /// you pass this next label in, is so that in the terminator you're emitting
    /// you can already refer to it by generating the label yourself through `ctx`.
    ///
    /// If you want a fresh new block, use [`emit_terminator_fresh_block`](Self::emit_terminator_fresh_block)
    pub fn emit_terminator(&mut self, t: lir::Terminator, next_block_label: Label) {
        self.wrap_up_current(t);
        self.current_label = next_block_label;
    }

    #[allow(unused)] // TODO: remove
    pub fn emit_terminator_fresh_block(&mut self, t: lir::Terminator) {
        let lbl = self.ctx().fresh_label();
        self.emit_terminator(t, lbl)
    }

    pub fn finish(mut self, t: lir::Terminator) -> BlockList {
        self.wrap_up_current(t);
        self.blocks
    }
}
