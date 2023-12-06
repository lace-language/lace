use crate::ast_metadata::Metadata;
use crate::lowering::basic_block::BasicBlockBuilder;
use crate::lowering::error::FunctionLoweringError;
use crate::lowering::variable::VariableDeclarations;
use crate::lowering::{lir, LoweringContext};
use crate::parser::ast;

impl<'b, 't, 'a, 'n> LoweringContext<'b, 't, 'a, 'n> {
    pub fn lower_function(
        &mut self,
        f: &Metadata<ast::Function>,
    ) -> Result<lir::Function, FunctionLoweringError> {
        let mut vd = VariableDeclarations::new();

        let f_ty = self.resolve_type(f)?;

        let mut bbb = BasicBlockBuilder::new(self, &mut vd);
        bbb.visit_block(&f.value.block.value);

        // lir::Function {
        //     name: ,
        //     blocks: vec![],
        // }
        todo!()
    }
}
