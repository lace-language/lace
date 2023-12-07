use crate::ast_metadata::MetadataId;
use crate::lice::Lice;
use crate::name_resolution::ResolvedNames;
use crate::typechecking::ctx::Types;
use crate::typechecking::ty::{PartialType, Type, TypeVariable};
use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;
use miette::Diagnostic;
use std::collections::HashMap;
use std::num::NonZeroU16;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic, Clone, PartialEq)]
pub enum ResolveTypeError {
    #[error("type not resolved")]
    Unresolved,
}

#[derive(Debug)]
pub struct SolvedTypes<'a, 'n> {
    types: Types<'a>,
    /// TODO: give expr nodes types as well
    node_types: HashMap<MetadataId, PartialType<'a>>,
    name_mapping: HashMap<MetadataId, TypeVariable>,
    resolved_names: &'n ResolvedNames,
}

impl<'a, 'n> SolvedTypes<'a, 'n> {
    pub fn new(
        types: Types<'a>,
        node_types: HashMap<MetadataId, PartialType<'a>>,
        name_mapping: HashMap<MetadataId, TypeVariable>,
        resolved_names: &'n ResolvedNames,
    ) -> Self {
        Self {
            types,
            node_types,
            name_mapping,
            resolved_names,
        }
    }

    pub(super) fn resolve_type_recursive<'x>(
        &self,
        ty: impl Into<PartialType<'a>>,
        arena: &'x Bump,
    ) -> Result<Type<'x>, ResolveTypeError> {
        let representative = self
            .types
            .get_representative(ty.into())
            .unwrap_or_lice("should have been typechecked");

        Ok(match representative {
            PartialType::Int { bits, signed } => Type::Int { bits, signed },

            // default int type (i32)
            PartialType::Variable(TypeVariable::Int(_)) => Type::Int {
                bits: NonZeroU16::new(32).unwrap(),
                signed: true,
            },
            PartialType::Bool => Type::Bool,
            PartialType::Function {
                params,
                ret,
                function_name,
            } => {
                let mut new_params = BumpVec::new_in(arena);

                for i in params {
                    let ty = self.resolve_type_recursive(*i, arena)?;
                    new_params.push(ty);
                }

                Type::Function {
                    params: new_params.into_bump_slice(),
                    ret: arena.alloc(self.resolve_type_recursive(*ret, arena)?),
                    function_name: function_name.unwrap_or_lice(
                        "every function should have been assigned a function name id",
                    ),
                }
            }
            PartialType::Tuple(t) => {
                let mut new = BumpVec::new_in(arena);

                for i in t {
                    let ty = self.resolve_type_recursive(*i, arena)?;
                    new.push(ty);
                }

                Type::Tuple(new.into_bump_slice())
            }
            PartialType::String => Type::String,
            PartialType::Variable(_) => return Err(ResolveTypeError::Unresolved),
        })
    }

    pub fn type_variable_for_identifier(&self, ident: MetadataId) -> Option<TypeVariable> {
        // when we get a variable, it could be from a definition or from a usage.
        // if it's a usage, this lookup will get us the definition. If it was a definition
        // already, we get None back.
        let id = self.resolved_names.names.get(&ident).unwrap_or(&ident);

        // We then use this definition site metadata id for the type variable lookup
        self.name_mapping.get(id).cloned()
    }

    pub fn type_of_node<'x>(
        &self,
        node: MetadataId,
        arena: &'x Bump,
    ) -> Result<Type<'x>, ResolveTypeError> {
        let typevar = self
            .node_types
            .get(&node)
            .unwrap_or_else(|| lice!("node with id {node:?} has no type information saved"));

        self.resolve_type_recursive(*typevar, arena)
    }

    pub fn type_of_name<'x>(
        &self,
        name_node_id: MetadataId,
        arena: &'x Bump,
    ) -> Result<Type<'x>, ResolveTypeError> {
        let typevar = self
            .type_variable_for_identifier(name_node_id)
            .unwrap_or_else(|| lice!("name not typechecked {name_node_id:?}"));

        self.resolve_type_recursive(typevar, arena)
    }
}
