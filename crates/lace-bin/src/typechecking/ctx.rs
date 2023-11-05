use crate::ast_metadata::{Metadata, MetadataId, WithNodeId};
use crate::lice::Lice;
use crate::name_resolution::ResolvedNames;
use crate::parser::ast::Ident;
use crate::typechecking::ty::{PartialType, TypeVariable, TypeVariableGenerator};
use bumpalo::Bump;
use std::collections::HashMap;

pub struct Types<'a> {
    data: HashMap<TypeVariable, PartialType<'a>>,
}

impl<'a> Types<'a> {
    pub fn new() -> Self {
        Self {
            data: Default::default(),
        }
    }

    pub fn add_type_variable(&mut self, var: TypeVariable) {
        self.data
            .insert(var, PartialType::Variable(var))
            .unwrap_or_lice("duplicate type variable");
    }

    pub fn type_of_type_variable(&self, var: TypeVariable) -> Option<&PartialType<'a>> {
        // NOTE: the double lookup here is because of a limitation in Rust's borrow checking.
        // NLL is not enough, and to make this a single get we need polonius. :(
        match self.data.get(&var)? {
            PartialType::Variable(v) if *v == var => self.data.get(&var),
            PartialType::Variable(v) => self.type_of_type_variable(*v),
            _ => self.data.get(&var),
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

pub struct TypeContext<'a, 'r> {
    pub(super) name_mapping: HashMap<MetadataId, TypeVariable>,
    pub(super) node_types: HashMap<MetadataId, PartialType<'a>>,
    pub(super) types: Types<'a>,
    variable_generator: TypeVariableGenerator,
    resolved_names: &'r ResolvedNames,
    pub arena: &'a Bump,
}

impl<'a, 'r> TypeContext<'a, 'r> {
    pub fn new(resolved_names: &'r ResolvedNames, arena: &'a Bump) -> Self {
        Self {
            name_mapping: Default::default(),
            node_types: Default::default(),
            types: Types::new(),
            variable_generator: TypeVariableGenerator::new(),
            resolved_names,
            arena,
        }
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

    pub fn type_of_type_variable(&mut self, var: TypeVariable) -> Option<&PartialType<'a>> {
        self.types.type_of_type_variable(var)
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
            (PartialType::Tuple(a), PartialType::Tuple(b)) => {
                if a.len() != b.len() {
                    return Err(UnifyError::TupleLength);
                }

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
            ) => {
                if a_params.len() != b_params.len() {
                    return Err(UnifyError::FunctionParamLength);
                }

                for (l, r) in a_params.iter().zip(b_params) {
                    self.unify(*l, *r)?;
                }

                self.unify(*a_ret, *b_ret)?;
            }

            (a, b) => return Err(UnifyError::NotEqual(a, b)),
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum UnifyError<'a> {
    NotEqual(PartialType<'a>, PartialType<'a>),
    TupleLength,
    FunctionParamLength,
}