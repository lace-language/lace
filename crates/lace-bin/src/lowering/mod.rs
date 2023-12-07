//! Lace Intermediate Representation
//! This is the representation on which optimizations are made.
//! To get here, first expressions are flattened, after which they are split into basic blocks

pub mod basic_block;
pub mod error;
mod function;
pub mod lir;
pub mod lire;
mod variable;

use crate::ast_metadata::Metadata;
use crate::ids::IdGenerator;
use crate::lowering::error::LoweringError;
use crate::lowering::lir::{Label, Lir, Variable};

use crate::parser::ast::{Ast, Item};
use crate::typechecking::solved::{ResolveTypeError, SolvedTypes};
use crate::typechecking::ty::{Type, TypeVariable};
use bumpalo::Bump;

use std::collections::HashMap;

pub fn lower(ast: &Ast, solved_types: &SolvedTypes) -> Result<Lir, LoweringError> {
    let mut functions = Vec::new();
    // used to allocate resolved types in.
    // TODO: maybe do this on the heap too. My reasoning for not doign that is
    //       that making new mir nodes usually doesn't create new types, so this
    //       arena won't grow unbounded.
    let type_arena = Bump::new();
    let mut ctx = LoweringContext::new(solved_types, &type_arena);

    for item in ast.items {
        match item {
            Item::Function(f) => functions.push(ctx.lower_function(f)),
        }
    }

    Ok(Lir { functions })
}

pub struct LoweringContext<'b, 't, 'a, 'n> {
    label_generator: IdGenerator<Label>,
    solved_types: &'t SolvedTypes<'a, 'n>,
    type_arena: &'b Bump,
    variable_mapping: HashMap<TypeVariable, Variable>,
    variable_generator: IdGenerator<Variable>,
}

impl<'b, 't, 'a, 'n> LoweringContext<'b, 't, 'a, 'n> {
    pub fn new(solved_types: &'t SolvedTypes<'a, 'n>, type_arena: &'b Bump) -> Self {
        Self {
            label_generator: IdGenerator::new(),
            solved_types,
            type_arena,
            variable_mapping: Default::default(),
            variable_generator: IdGenerator::new(),
        }
    }

    pub fn fresh_label(&self) -> Label {
        self.label_generator.fresh()
    }

    fn resolve_type<T>(&self, node: &Metadata<T>) -> Result<Type<'b>, ResolveTypeError> {
        self.solved_types
            .type_of_node(node.metadata, self.type_arena)
    }
}
