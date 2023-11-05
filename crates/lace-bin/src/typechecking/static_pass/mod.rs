use crate::parser::ast::Ast;
use crate::typechecking::ctx::TypeContext;
use crate::typechecking::static_pass::find_statics::find_statics_item;

pub mod find_statics;

pub fn find_statics_ast(ast: &Ast, global_ctx: &mut TypeContext) {
    for i in ast.items {
        find_statics_item(i, global_ctx)
    }
}
