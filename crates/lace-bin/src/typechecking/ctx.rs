use crate::ast_metadata::{Metadata, MetadataId};
use crate::name_resolution::ResolvedNames;
use crate::parser::ast::Ident;
use crate::parser::span::{Span, Spans};
use crate::typechecking::ty::{PartialType, TypeVariable, TypeVariableGenerator};
use bumpalo::Bump;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Types<'a> {
    data: HashMap<TypeVariable, PartialType<'a>>,
}

impl<'a> Types<'a> {
    pub fn new() -> Self {
        Self {
            data: Default::default(),
        }
    }

    pub fn type_of_type_variable(&self, var: TypeVariable) -> Option<&PartialType<'a>> {
        match self.data.get(&var)? {
            p @ PartialType::Variable(v) if *v == var => Some(p),
            PartialType::Variable(v) => self.type_of_type_variable(*v),
            p => Some(p),
        }
    }

    pub fn get_representative_or_insert(&mut self, initial: PartialType<'a>) -> PartialType<'a> {
        if let PartialType::Variable(x) = initial {
            if let Some(i) = self.type_of_type_variable(x) {
                return *i;
            } else {
                self.data.insert(x, PartialType::Variable(x));
            }
        }

        initial
    }

    pub fn get_representative(&self, initial: PartialType<'a>) -> Option<PartialType<'a>> {
        if let PartialType::Variable(x) = initial {
            self.type_of_type_variable(x).copied()
        } else {
            Some(initial)
        }
    }

    pub fn insert(&mut self, from: TypeVariable, to: impl Into<PartialType<'a>>) {
        self.data.insert(from, to.into());
    }

    pub fn extend(&mut self, other: Types<'a>) {
        self.data.extend(other.data);
    }
}

#[derive(Clone)]
pub struct TypeContext<'a, 'r, 't> {
    pub(super) name_mapping: HashMap<MetadataId, TypeVariable>,
    pub(super) node_types: HashMap<MetadataId, PartialType<'a>>,
    pub(super) types: Types<'a>,
    variable_generator: &'t TypeVariableGenerator,
    resolved_names: &'r ResolvedNames,
    pub arena: &'a Bump,
    spans: &'r Spans,
}

impl<'a, 'r, 't> TypeContext<'a, 'r, 't> {
    pub fn new(
        resolved_names: &'r ResolvedNames,
        arena: &'a Bump,
        spans: &'r Spans,
        variable_generator: &'t TypeVariableGenerator,
    ) -> Self {
        Self {
            name_mapping: Default::default(),
            node_types: Default::default(),
            types: Types::new(),
            variable_generator,
            resolved_names,
            arena,
            spans,
        }
    }

    pub fn span_for(&self, meta: MetadataId) -> Span {
        self.spans.get(meta)
    }

    pub fn fresh(&mut self) -> TypeVariable {
        self.variable_generator.fresh()
    }

    pub fn alloc<T>(&self, value: T) -> &'a T {
        self.arena.alloc(value)
    }

    pub fn type_variable_for_identifier(&mut self, ident: &Metadata<Ident>) -> TypeVariable {
        // when we get a variable, it could be from a definition or from a usage.
        // if it's a usage, this lookup will get us the definition. If it was a definition
        // already, we get None back.
        let id = self
            .resolved_names
            .names
            .get(&ident.metadata)
            .unwrap_or(&ident.metadata);

        // We then use this definition site metadata id for the type variable lookup
        // if it's not yet in the map, we insert it.
        *self
            .name_mapping
            .entry(*id)
            .or_insert_with(|| self.variable_generator.fresh())
    }

    pub fn unify(
        &mut self,
        a: impl Into<PartialType<'a>>,
        b: impl Into<PartialType<'a>>,
    ) -> Result<(), UnifyError<'a>> {
        let a = self.types.get_representative_or_insert(a.into());
        let b = self.types.get_representative_or_insert(b.into());

        match (a, b) {
            (PartialType::Variable(a), PartialType::Variable(b)) => {
                self.types.insert(a, PartialType::Variable(b));
            }
            (PartialType::Variable(a), b) | (b, PartialType::Variable(a)) => {
                self.types.insert(a, b);
            }
            (PartialType::Int, PartialType::Int) => {}
            (PartialType::String, PartialType::String) => {}
            (PartialType::Bool, PartialType::Bool) => {}
            (PartialType::Tuple(a), PartialType::Tuple(b)) if a.len() == b.len() => {
                for (l, r) in a.iter().zip(b) {
                    self.unify(*l, *r)?;
                }
            }

            (
                PartialType::Function {
                    params: a_params,
                    ret: a_ret,
                },
                PartialType::Function {
                    params: b_params,
                    ret: b_ret,
                },
            ) if a_params.len() == b_params.len() => {
                for (l, r) in a_params.iter().zip(b_params) {
                    self.unify(*l, *r)?;
                }

                self.unify(*a_ret, *b_ret)?;
            }

            (a, b) => return Err((a, b)),
        }

        Ok(())
    }
}

pub type UnifyError<'a> = (PartialType<'a>, PartialType<'a>);
