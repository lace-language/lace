use crate::lowering::basic_block::BasicBlockBuilder;
use crate::lowering::lir;

impl<'l, 'b, 't, 'a, 'n> BasicBlockBuilder<'l, 'b, 't, 'a, 'n> {
    pub(super) fn emit_assignment(&mut self, place: lir::Place, expr: lir::Expr) {
        self.current_statements
            .push(lir::Statement::Assignment { expr, place })
    }

    pub(super) fn emit_variable_assignment(&mut self, expr: lir::Expr) -> lir::Place {
        let name = self.ctx().variable_generator.fresh();
        let place = lir::Place::Variable(self.vd().declare_variable(name));
        self.emit_assignment(place.clone(), expr);
        place
    }
}
