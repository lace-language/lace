use std::collections::hash_map::Entry;
use std::collections::HashMap;
use bumpalo::Bump;
use crate::name_resolution::NameResolutions;
use crate::parser::ast::Ident;
use crate::parser::span::{NodeId, Spanned};
use crate::typechecking::constraint::Constraint;
use crate::typechecking::ty::TypeVariable;
use crate::typechecking::ty::TypeVariableGenerator;
use crate::typechecking::ty::TypeOrVariable;

pub struct TypeContext<'a> {
    variable_generator: TypeVariableGenerator,
    pub arena: &'a Bump,
    constraints: Vec<Constraint<'a>>,
    // TODO: replace with FxHashMap
    pub name_mapping: HashMap<NodeId, TypeVariable>
}

impl<'a> TypeContext<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        Self {
            variable_generator: TypeVariableGenerator::new(),
            arena,
            constraints: vec![],
            name_mapping: Default::default(),
        }
    }

    fn get_or_insert_name_mapping(&mut self, node_id: NodeId) -> TypeVariable {
        match self.name_mapping.entry(node_id) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(v) => {
                let variable = self.variable_generator.next();
                *v.insert(variable)
            }
        }
    }

    pub fn add_name_resolutions(&mut self, name_resolutions: &NameResolutions) {
        for (a, b) in &name_resolutions.names {
            let a = self.get_or_insert_name_mapping(*a);
            let b = self.get_or_insert_name_mapping(*b);
            self.add_equal_constraint(a, b);
        }
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
