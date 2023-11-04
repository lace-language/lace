use crate::ast_metadata::MetadataId;
use crate::error::{ErrorId, FailedUnification};
use crate::lice::Lice;
use crate::typechecking::constraint::Constraint;
use crate::typechecking::constraint_metadata::ConstraintMetadata;
use crate::typechecking::context::{NameMapping, TypeContext};
use crate::typechecking::error::{InnerTypeError, TypeError};
use crate::typechecking::ty::{PartialType, Type, TypeOrVariable, TypeVariable};
use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;
use itertools::Itertools;
use std::collections::HashMap;
use unionfind::HashUnionFind;

type SolveState<'a> = HashUnionFind<TypeOrVariable<'a>>;

impl<'a, 'sp> TypeContext<'a, 'sp> {
    #[allow(unused)]
    fn cant_unify(
        &mut self,
        left: &'a PartialType<'a>,
        right: &'a PartialType<'a>,
    ) -> TypeOrVariable<'a> {
        println!("can't unify {left} == {right}");

        let error_id = self.error_id_generator.fresh();
        TypeOrVariable::Error(error_id)
    }

    fn unify_one(
        &mut self,
        a: TypeOrVariable<'a>,
        b: TypeOrVariable<'a>,
        metadata: &ConstraintMetadata<'a>,
    ) -> TypeOrVariable<'a> {
        match (a, b) {
            // unification of a concrete type and an error or error variable adds to the error
            (TypeOrVariable::Error(id), TypeOrVariable::Concrete(..))
            | (TypeOrVariable::Concrete(..), TypeOrVariable::Error(id)) => {
                TypeOrVariable::Error(id)
            }

            // unification of two errors merges the errors
            (TypeOrVariable::Error(ida), TypeOrVariable::Error(idb)) => TypeOrVariable::Error(ida),
            // unification between an error (variable) and a variable propagates the variable with a marker that it's also an error now
            (TypeOrVariable::Variable(b), TypeOrVariable::Error(id))
            | (TypeOrVariable::Error(id), TypeOrVariable::Variable(b)) => TypeOrVariable::Error(id),

            // if not, we choose one by a random dice roll. My dice rolled 0 so we choose a
            (a @ TypeOrVariable::Variable(_), TypeOrVariable::Variable(_)) => a,
            // if one of them is concrete, make that the representative
            (c @ TypeOrVariable::Concrete(_, _), TypeOrVariable::Variable(_))
            | (TypeOrVariable::Variable(_), c @ TypeOrVariable::Concrete(_, _)) => c,
            // however, when both are concrete, we can only union if they are the same.
            // we don't actually check that here. Instead, we assume they are the same,
            // continue with a (random dice roll) and then later check if they really were
            // the same or that we went ahead with the wrong type.
            (TypeOrVariable::Concrete(concrete_a, _), TypeOrVariable::Concrete(concrete_b, _)) => {
                match (concrete_a, concrete_b) {
                    (PartialType::Int, PartialType::Int) => {}
                    (PartialType::Bool, PartialType::Bool) => {}
                    (PartialType::String, PartialType::String) => {}
                    (PartialType::Tuple(elems_a), PartialType::Tuple(elems_b)) => {
                        if elems_a.len() != elems_b.len() {
                            todo!()
                            // return self.cant_unify(
                            //     concrete_a,
                            //     concrete_b,
                            //     ConstraintMetadata::TupleLength,
                            // );
                        }

                        let meta = self.alloc(metadata.clone());

                        for (x, y) in elems_a.iter().zip(*elems_b) {
                            self.add_equal_constraint(
                                *x,
                                *y,
                                ConstraintMetadata::TupleUnify { orig: meta },
                            );
                        }
                    }
                    (
                        PartialType::Function {
                            params: params_a,
                            ret: ret_a,
                        },
                        PartialType::Function {
                            params: params_b,
                            ret: ret_b,
                        },
                    ) => {
                        if params_a.len() != params_b.len() {
                            // todo!()
                            // return self.cant_unify(
                            //     concrete_a,
                            //     concrete_b,
                            //     ConstraintMetadata::ParamLength,
                            // );
                        }

                        let meta = self.alloc(metadata.clone());

                        for (x, y) in params_a.iter().zip(*params_b) {
                            self.add_equal_constraint(
                                *x,
                                *y,
                                ConstraintMetadata::FunctionParamUnify { orig: meta },
                            );
                        }

                        self.add_equal_constraint(
                            *ret_a,
                            *ret_b,
                            ConstraintMetadata::FunctionReturnUnify { orig: meta },
                        )
                    }

                    (ca, cb) => return self.cant_unify(ca, cb),
                }

                a
            }
        }
    }

    fn unify(
        &mut self,
        a: TypeOrVariable<'a>,
        b: TypeOrVariable<'a>,
        meta: &ConstraintMetadata<'a>,
        uf: &mut SolveState<'a>,
    ) {
        uf.union_by_or_add(&a, &b, |a, b| {
            let res = self.unify_one(a, b, meta);

            // println!("{a:?} and {b:?} to {res:?}");
            res
        })
        .unwrap_or_lice("all variables were inserted at the start");
    }

    pub fn solve(mut self) -> Result<SolvedTypes<'a>, TypeError> {
        let mut uf = SolveState::new(std::iter::empty()).unwrap_or_lice("empty iterator");
        let mut solved_constraints = Vec::with_capacity(self.constraints.len());

        while let Some((constraint, meta)) = self.constraints.pop_front() {
            match constraint {
                Constraint::Equal(a, b) => {
                    // pass uf explicitly
                    self.unify(a, b, &meta, &mut uf);
                }
            }

            solved_constraints.push((constraint, meta));
        }

        Self::find_errors(solved_constraints, &mut uf)?;

        let solved_types = SolvedTypes {
            name_mapping: self.name_mapping,
            uf,
        };

        Ok(solved_types)
    }

    fn convert_error_group(e: Vec<(Constraint, ConstraintMetadata)>) -> InnerTypeError {
        println!("{e:?}");

        todo!()
    }
    fn find_errors(
        solved_constraints: Vec<(Constraint<'a>, ConstraintMetadata<'a>)>,
        uf: &mut SolveState<'a>,
    ) -> Result<(), TypeError> {
        let mut error_groups = HashMap::new();

        for (c @ Constraint::Equal(l, _), meta) in solved_constraints {
            if let TypeOrVariable::Error(e) = uf.find_shorten(&l).unwrap_or_lice("should be in uf")
            {
                error_groups
                    .entry(e)
                    .or_insert_with(Vec::new)
                    .push((c, meta));
            }
        }

        if !error_groups.is_empty() {
            Err(TypeError {
                errors: error_groups
                    .into_values()
                    .map(Self::convert_error_group)
                    .collect_vec(),
            })
        } else {
            Ok(())
        }
    }
}

pub struct SolvedTypes<'a> {
    name_mapping: NameMapping,
    uf: SolveState<'a>,
}

impl<'a> SolvedTypes<'a> {
    /// Finds a representative type variable (one that maps to a concrete type through `type_mapping`)
    /// for any type variable.
    ///
    /// Returns an error when there was no concrete representative (i.e. there is a type variable that
    /// could not be inferred).
    fn find_representative(
        &self,
        var: TypeOrVariable<'a>,
    ) -> Result<&'a PartialType<'a>, TypeVariable> {
        match self
            .uf
            .find(&var)
            .unwrap_or_lice("type variable not in union find")
        {
            TypeOrVariable::Concrete(c, _) => Ok(c),
            TypeOrVariable::Variable(v) => Err(v),
            TypeOrVariable::Error(id) => {
                lice!("these should be filtered out at this point: {id:?}")
            }
        }
    }

    pub(super) fn resolve_type_recursive<'x>(
        &self,
        ty: TypeOrVariable<'a>,
        arena: &'x Bump,
    ) -> Type<'x> {
        let representative = self.find_representative(ty).expect("");

        match representative {
            PartialType::Int => Type::Int,
            PartialType::Bool => Type::Bool,
            PartialType::Function { params, ret } => {
                let mut new_params = BumpVec::new_in(arena);

                for i in *params {
                    let ty = self.resolve_type_recursive(*i, arena);
                    new_params.push(ty);
                }

                Type::Function {
                    params: new_params.into_bump_slice(),
                    ret: arena.alloc(self.resolve_type_recursive(*ret, arena)),
                }
            }
            PartialType::Tuple(t) => {
                let mut new = BumpVec::new_in(arena);

                for i in *t {
                    let ty = self.resolve_type_recursive(*i, arena);
                    new.push(ty);
                }

                Type::Tuple(new.into_bump_slice())
            }
            PartialType::String => Type::String,
        }
    }

    pub fn type_of_name<'x>(&self, name_node_id: MetadataId, arena: &'x Bump) -> Option<Type<'x>> {
        let typevar = self.name_mapping.get(&name_node_id)?;
        Some(self.resolve_type_recursive(TypeOrVariable::Variable(*typevar), arena))
    }
}
