use crate::ast_metadata::Metadata;
use crate::lice::Lice;
use crate::lowering::basic_block::BasicBlockBuilder;
use crate::lowering::lir::Terminator;
use crate::lowering::variable::VariableDeclarations;
use crate::lowering::{lir, LoweringContext};
use crate::parser::ast;
use crate::typechecking::ty::Type;

impl<'b, 't, 'a, 'n> LoweringContext<'b, 't, 'a, 'n> {
    pub fn lower_function(&mut self, f: &Metadata<ast::Function>) -> lir::Function {
        let mut vd = VariableDeclarations::new();

        let Type::Function {
            params: _,
            ret: _,
            function_name,
        } = self.resolve_type(f).unwrap_or_lice("resolve type failed")
        else {
            lice!("function without function type");
        };

        let mut bbb = BasicBlockBuilder::new(self, &mut vd);
        bbb.visit_block(&f.value.block.value);

        lir::Function {
            name: function_name,
            blocks: bbb.finish(Terminator::Return),
            variables: vd.into_iter().collect(),
        }
    }
}
