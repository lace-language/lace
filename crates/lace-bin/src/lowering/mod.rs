//! Lace Intermediate Representation

pub mod error;
pub mod executor;
mod function;
pub mod lir;

use crate::lowering::error::LoweringError;
use crate::lowering::function::FunctionLowerer;
use crate::lowering::lir::Lir;
use crate::parser::ast::{Ast, Item};
use crate::typechecking::solved::SolvedTypes;
use bumpalo::Bump;

pub fn lower(ast: &Ast, solved_types: &SolvedTypes) -> Result<Lir, LoweringError> {
    let mut functions = Vec::new();
    // used to allocate resolved types in.
    // TODO: maybe do this on the heap too. My reasoning for not doign that is
    //       that making new mir nodes usually doesn't create new types, so this
    //       arena won't grow unbounded.
    let type_arena = Bump::new();

    for item in ast.items {
        match item {
            Item::Function(f) => {
                let mut ctx = FunctionLowerer::new(solved_types, &type_arena);
                functions.push(ctx.lower_function(f)?)
            }
        }
    }

    Ok(Lir { functions })
}
