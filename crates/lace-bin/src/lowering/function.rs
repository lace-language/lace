use crate::ast_metadata::Metadata;
use crate::ids::IdGenerator;
use crate::lowering::error::FunctionLoweringError;
use crate::lowering::lir;
use crate::lowering::lir::{BasicBlock, Label, Variable};
use crate::parser::ast;
use crate::parser::ast::Block;
use crate::typechecking::solved::{ResolveTypeError, SolvedTypes};
use crate::typechecking::ty::Type;
use bumpalo::Bump;

pub struct FunctionLowerer<'l, 't, 'a, 'n> {
    label_generator: IdGenerator<Label>,
    variable_generator: IdGenerator<Variable>,
    solved_types: &'t SolvedTypes<'a, 'n>,
    type_arena: &'l Bump,
}

impl<'l, 't, 'a, 'n> FunctionLowerer<'l, 't, 'a, 'n> {
    pub fn new(solved_types: &'t SolvedTypes<'a, 'n>, type_arena: &'l Bump) -> Self {
        Self {
            label_generator: IdGenerator::new(),
            variable_generator: IdGenerator::new(),
            solved_types,
            type_arena,
        }
    }

    fn resolve_type<T>(&self, node: &Metadata<T>) -> Result<Type<'l>, ResolveTypeError> {
        self.solved_types
            .type_of_node(node.metadata, self.type_arena)
    }

    pub fn split_basic_blocks(&mut self, b: &Metadata<Block>) -> Vec<BasicBlock> {
        todo!()
    }

    pub fn lower_function(
        &mut self,
        f: &Metadata<ast::Function>,
    ) -> Result<lir::Function, FunctionLoweringError> {
        let f_ty = self.resolve_type(f)?;
        let bbs = self.split_basic_blocks(f.value.block);

        // lir::Function {
        //     name: ,
        //     blocks: vec![],
        // }
        todo!()
    }
}
