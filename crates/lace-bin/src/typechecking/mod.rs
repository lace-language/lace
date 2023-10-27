use bumpalo::Bump;
use crate::typechecking::concrete_type::ConcreteType;
use crate::typechecking::variable::{TypeVariable, TypeVariableGenerator};

pub mod constraint;
pub mod variable;
pub mod concrete_type;

pub enum Type<'a> {
    Concrete(ConcreteType<'a>),
    Variable(TypeVariable)
}

pub struct TypeContext<'a> {
    variable_generator: TypeVariableGenerator,
    arena: &'a Bump,
}

impl<'a> TypeContext<'a> {
    pub fn fresh(&mut self) -> TypeVariable {
        self.variable_generator.next()
    }

    pub fn alloc<T>(&self, value: T) -> &'a T {
        self.arena.alloc(value)
    }
}