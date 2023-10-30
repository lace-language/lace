use crate::ast_metadata::MetadataId;
use crate::lice::Lice;
use crate::typechecking::constraint::Constraint;
use crate::typechecking::constraint_metadata::ConstraintMetadata;
use crate::typechecking::context::{NameMapping, TypeContext};
use crate::typechecking::error::TypeError;
use crate::typechecking::ty::{ConcreteType, Type, TypeOrVariable, TypeVariable};
use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;
use unionfind::HashUnionFind;

type SolveState<'a> = HashUnionFind<TypeOrVariable<'a>>;

impl<'a, 'sp> TypeContext<'a, 'sp> {
    #[allow(unused)]
    fn cant_unify(&mut self, a: ConcreteType, b: ConcreteType, meta: ConstraintMetadata) {
        println!("can't unify {a} == {b}");
        match meta {
            ConstraintMetadata::NoConstraintMetadata => lice!("no constrain metadata"),
            ConstraintMetadata::BinaryOp(left, right, op) => {
                self.errors.push(TypeError::BinaryOp {
                    op,
                    left_ty: a.to_string(),
                    right_ty: b.to_string(),
                    left: self.spans.get(left),
                    right: self.spans.get(right),
                })
            }
            ConstraintMetadata::UnaryOp(expr, op) => self.errors.push(TypeError::UnaryOp {
                op,
                ty: a.to_string(),
                expr: self.spans.get(expr),
            }),
            ConstraintMetadata::BlockCondition(_) => todo!(),
            ConstraintMetadata::IfReturn(_, _) => todo!(),
            ConstraintMetadata::Call { .. } => todo!(),
            ConstraintMetadata::TypeSpec { .. } => todo!(),
            ConstraintMetadata::Assignment { .. } => todo!(),
            ConstraintMetadata::FunctionDefinition { .. } => todo!(),
            ConstraintMetadata::FunctionReturn { .. } => todo!(),
            ConstraintMetadata::NameRef => todo!(),
            ConstraintMetadata::TupleUnify { .. } => todo!(),
            ConstraintMetadata::FunctionParamUnify { .. } => todo!(),
            ConstraintMetadata::FunctionReturnUnify { .. } => todo!(),
            ConstraintMetadata::ParamLength => todo!(),
            ConstraintMetadata::TupleLength => todo!(),
        }
    }

    fn unify_one(
        &mut self,
        a: TypeOrVariable<'a>,
        b: TypeOrVariable<'a>,
        meta: ConstraintMetadata<'a>,
    ) -> TypeOrVariable<'a> {
        match (a, b) {
            // if not, we choose one by a random dice roll. My dice rolled 0 so we choose a
            (a @ TypeOrVariable::Variable(_), TypeOrVariable::Variable(_)) => a,
            // if one of them is concrete, make that the representative
            (c @ TypeOrVariable::Concrete(_), TypeOrVariable::Variable(_))
            | (TypeOrVariable::Variable(_), c @ TypeOrVariable::Concrete(_)) => c,
            // however, when both are concrete, we can only union if they are the same.
            // we don't actually check that here. Instead, we assume they are the same,
            // continue with a (random dice roll) and then later check if they really were
            // the same or that we went ahead with the wrong type.
            (TypeOrVariable::Concrete(concrete_a), TypeOrVariable::Concrete(concrete_b)) => {
                match (concrete_a, concrete_b) {
                    (ConcreteType::Int, ConcreteType::Int) => {}
                    (ConcreteType::Bool, ConcreteType::Bool) => {}
                    (ConcreteType::String, ConcreteType::String) => {}
                    (ConcreteType::Tuple(elems_a), ConcreteType::Tuple(elems_b)) => {
                        if elems_a.len() != elems_b.len() {
                            self.cant_unify(
                                concrete_a,
                                concrete_b,
                                ConstraintMetadata::TupleLength,
                            );
                        }

                        let meta = self.alloc(meta);

                        for (x, y) in elems_a.iter().zip(elems_b) {
                            self.add_equal_constraint(
                                *x,
                                *y,
                                ConstraintMetadata::TupleUnify { orig: meta },
                            );
                        }
                    }
                    (
                        ConcreteType::Function {
                            params: params_a,
                            ret: ret_a,
                        },
                        ConcreteType::Function {
                            params: params_b,
                            ret: ret_b,
                        },
                    ) => {
                        if params_a.len() != params_b.len() {
                            self.cant_unify(
                                concrete_a,
                                concrete_b,
                                ConstraintMetadata::ParamLength,
                            );
                        }

                        let meta = self.alloc(meta);

                        for (x, y) in params_a.iter().zip(params_b) {
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

                    (ca, cb) => {
                        self.cant_unify(ca, cb, meta);
                    }
                }

                a
            }
        }
    }

    fn unify(
        &mut self,
        a: TypeOrVariable<'a>,
        b: TypeOrVariable<'a>,
        meta: ConstraintMetadata<'a>,
        uf: &mut SolveState<'a>,
    ) {
        uf.union_by_or_add(&a, &b, |a, b| self.unify_one(a, b, meta))
            .unwrap_or_lice("all variables were inserted at the start");
    }

    pub fn solve(mut self) -> Result<SolvedTypes<'a>, Vec<TypeError>> {
        let mut uf = SolveState::new(std::iter::empty()).unwrap_or_lice("empty iterator");

        while let Some((constraint, meta)) = self.constraints.pop() {
            match constraint {
                Constraint::Equal(a, b) => {
                    // pass uf explicitly
                    self.unify(a, b, meta, &mut uf);
                }
            }
        }

        if !self.errors.is_empty() {
            return Err(self.errors);
        }

        Ok(SolvedTypes {
            name_mapping: self.name_mapping,
            uf,
        })
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
    ) -> Result<ConcreteType<'a>, TypeVariable> {
        match self
            .uf
            .find(&var)
            .unwrap_or_lice("type variable not in union find")
        {
            TypeOrVariable::Concrete(c) => Ok(c),
            TypeOrVariable::Variable(v) => Err(v),
        }
    }

    pub(super) fn resolve_type_recursive<'x>(
        &self,
        ty: impl Into<TypeOrVariable<'a>>,
        arena: &'x Bump,
    ) -> Type<'x> {
        let representative = self.find_representative(ty.into()).expect("");

        match representative {
            ConcreteType::Int => Type::Int,
            ConcreteType::Bool => Type::Bool,
            ConcreteType::Function { params, ret } => {
                let mut new_params = BumpVec::new_in(arena);

                for i in params {
                    let ty = self.resolve_type_recursive(*i, arena);
                    new_params.push(ty);
                }

                Type::Function {
                    params: new_params.into_bump_slice(),
                    ret: arena.alloc(self.resolve_type_recursive(*ret, arena)),
                }
            }
            ConcreteType::Tuple(t) => {
                let mut new = BumpVec::new_in(arena);

                for i in t {
                    let ty = self.resolve_type_recursive(*i, arena);
                    new.push(ty);
                }

                Type::Tuple(new.into_bump_slice())
            }
            ConcreteType::String => Type::String,
        }
    }

    pub fn type_of_name<'x>(&self, name_node_id: MetadataId, arena: &'x Bump) -> Option<Type<'x>> {
        let typevar = self.name_mapping.get(&name_node_id)?;
        Some(self.resolve_type_recursive(*typevar, arena))
    }
}
