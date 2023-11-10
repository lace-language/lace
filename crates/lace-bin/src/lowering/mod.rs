//! Lace Intermediate Representation

pub mod executor;
mod function;
pub mod lir;

use crate::lowering::function::FunctionLowerer;
use crate::lowering::lir::Lir;
use crate::parser::ast::{Ast, Item};
use crate::typechecking::solved::SolvedTypes;

pub fn lower(ast: &Ast, ty: &SolvedTypes) -> Lir {
    let mut functions = Vec::new();

    for item in ast.items {
        match item {
            Item::Function(f) => {
                let mut ctx = FunctionLowerer::new();
                functions.push(ctx.lower_function(f))
            }
        }
    }

    Lir { functions }
}
