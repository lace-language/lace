use bumpalo::Bump;
use crate::parser::ast::Ident;
use crate::parser::span::Spanned;
use crate::typechecking::constraint::Constraint;
use crate::typechecking::ty::TypeVariable;
use crate::typechecking::ty::TypeVariableGenerator;
use crate::typechecking::ty::TypeOrVariable;

pub struct TypeContext<'a> {
    variable_generator: TypeVariableGenerator,
    pub arena: &'a Bump,
    constraints: Vec<Constraint<'a>>
}

impl<'a> TypeContext<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        Self {
            variable_generator: TypeVariableGenerator::new(),
            arena,
            constraints: vec![],
        }
    }

    pub fn add_name_resolution_result(&mut self) {
        todo!()
    }

    pub fn fresh(&mut self) -> TypeVariable {
        self.variable_generator.next()
    }

    pub fn alloc<T>(&self, value: T) -> &'a T {
        self.arena.alloc(value)
    }

    pub fn add_equal_constraint(&mut self, a: impl Into<TypeOrVariable<'a>>, b: impl Into<TypeOrVariable<'a>>) {
        let a = a.into();
        let b = b.into();

        self.constraints.push(Constraint::Equal(a, b));
    }

    pub fn type_of_ident(&self, _ident: &Spanned<Ident>) -> TypeOrVariable<'a> {
        todo!()
    }
}
