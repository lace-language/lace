use crate::lice::Lice;
use crate::lowering::basic_block::BasicBlockBuilder;
use crate::lowering::lir::Label;
use crate::lowering::variable::VariableDeclarations;
use crate::lowering::{lir, LoweringContext};
use std::ops::Deref;

/// A type that represents a frozen basic block builder,
/// one that should not be used for lir generation anymore, but can be inserted into newly
/// generated lir using [`BasicBlockBuilder::insert`].
/// This is a useful type since the variable generator is moved in and out of `BasicBlockBuilders`,
/// but can only ever be in one of them. During [`BasicBlockBuilder::child`] it temporarily moves into
/// the child builder, but after that it moves back into the parent, which means the child bbb must be
/// frozen to prevent accidentally unwrapping.
pub struct FrozenBasicBlockBuilder<'l, 'b, 't, 'a, 'n>(BasicBlockBuilder<'l, 'b, 't, 'a, 'n>);

impl<'l, 'b, 't, 'a, 'n> Deref for FrozenBasicBlockBuilder<'l, 'b, 't, 'a, 'n> {
    type Target = BasicBlockBuilder<'l, 'b, 't, 'a, 'n>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'l, 'b, 't, 'a, 'n> BasicBlockBuilder<'l, 'b, 't, 'a, 'n> {
    pub fn vd(&mut self) -> &mut VariableDeclarations {
        self.__vd.as_mut().unwrap_or_lice("vd moved out")
    }

    pub fn ctx(&mut self) -> &mut LoweringContext<'b, 't, 'a, 'n> {
        self.__ctx.as_mut().unwrap_or_lice("ctx moved out")
    }

    pub fn freeze(self) -> FrozenBasicBlockBuilder<'l, 'b, 't, 'a, 'n> {
        FrozenBasicBlockBuilder(self)
    }

    /// build a basic blocks separately from this one. To insert the result, use [`insert`](Self::insert).
    ///
    /// Note: after child, the resulting [`BasicBlockBuilder`] should not be used to build more basic blocks
    /// just use it to insert. TODO:
    pub fn child<T>(
        &mut self,
        f: impl FnOnce(&mut BasicBlockBuilder<'l, 'b, 't, 'a, 'n>) -> T,
    ) -> (FrozenBasicBlockBuilder<'l, 'b, 't, 'a, 'n>, T) {
        // make a child, and move the ctx and vd into there
        let mut bbb = BasicBlockBuilder::new(self.__ctx.take().unwrap(), self.__vd.take().unwrap());

        // run this function while ctx and vd are there
        let res = f(&mut bbb);

        // put the ctx and vd back
        self.__ctx = bbb.__ctx.take();
        self.__vd = bbb.__vd.take();

        (bbb.freeze(), res)
    }

    /// Insert another basic block builder into this one, at the point we're currently generating.
    /// for example, when `other` is:
    /// ```
    /// a:
    ///     x
    ///     y
    ///     z
    /// b:
    ///     x
    ///     y
    /// ```
    /// currently, and self is currently building something like:
    /// ```
    /// c:
    ///     x
    ///     y
    /// ```
    ///
    /// inserting `other` into `self` would give:
    /// ```
    /// // where we were working in `self`
    /// c:
    ///     x
    ///     y
    ///     goto jump_after // added by this insert
    /// // what we generated in `other`
    /// a:
    ///     x
    ///     y
    ///     z
    /// b:
    ///     x
    ///     y
    ///     goto jump_after // added by this insert
    /// jump_after: // added by this insert, which is the `current_label` now
    ///     // where we can continue generating lir now
    /// ```
    ///
    /// and jump_after would be inserted into the `current_label`
    pub fn insert(
        &mut self,
        other: FrozenBasicBlockBuilder<'l, 'b, 't, 'a, 'n>,
        jump_after: Label,
    ) {
        let blocks = other.0.finish(lir::Terminator::Goto(jump_after));
        self.blocks.extend(blocks);

        // TODO: this is maybe not necessary, depending on if we define fallthrough
        //       in lir. Not defining fallthrough might make optimizations easier.
        //       if we ever decide this: change the docs too!
        self.emit_terminator(lir::Terminator::Goto(jump_after), jump_after);
    }

    /// Like [`insert`](Self::insert) but generates the `jump_after` label fresh
    pub fn insert_fresh_block(&mut self, other: FrozenBasicBlockBuilder<'l, 'b, 't, 'a, 'n>) {
        let jump_after = self.ctx().fresh_label();
        self.insert(other, jump_after)
    }
}
