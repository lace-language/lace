use crate::ast_metadata::Metadata;
use crate::ids::IdGenerator;
use crate::lowering::lir;
use crate::lowering::lir::{Label, Variable};
use crate::parser::ast;

pub struct FunctionLowerer {
    label_generator: IdGenerator<Label>,
    variable_generator: IdGenerator<Variable>,
}

impl FunctionLowerer {
    pub fn new() -> Self {
        Self {
            label_generator: IdGenerator::new(),
            variable_generator: IdGenerator::new(),
        }
    }

    pub fn lower_function(&mut self, f: &Metadata<ast::Function>) -> lir::Function {
        todo!()
    }
}
