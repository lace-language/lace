use crate::lice::Lice;
use crate::syntax_id::NodeId;
use crate::typechecking::constraint::Constraint;
use crate::typechecking::context::{NameMapping, TypeContext, TypeMapping};
use crate::typechecking::error::TypeError;
use crate::typechecking::ty::{ConcreteType, Type, TypeVariable, TypeVariableGenerator};
use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;
use unionfind::VecUnionFind;

type SolveState = VecUnionFind<usize>;

pub struct Solver<'a> {
    variable_generator: TypeVariableGenerator,
    constraints: Vec<Constraint>,
    type_mapping: TypeMapping<'a>,
    name_mapping: NameMapping,
    errors: Vec<TypeError>,
}

impl<'a> Solver<'a> {
    pub fn from_type_context(
        ctx: TypeContext<'a>
    ) -> Self {
        Self {
            variable_generator: ctx.variable_generator,
            constraints: ctx.constraints,
            type_mapping: ctx.type_mapping,
            name_mapping: ctx.name_mapping,
            errors: vec![],
        }
    }

    fn cant_unify(&mut self, ca: &ConcreteType, cb: &ConcreteType, a: TypeVariable, b: TypeVariable) {

    }

    fn unify_one(&mut self, a: TypeVariable, b: TypeVariable) -> TypeVariable {
        // look up if any correspond to concrete types we know
        let type_a = self.type_mapping.get(&a);
        let type_b = self.type_mapping.get(&b);

        match (type_a, type_b) {
            // if not, we choose one by a random dice roll. My dice rolled 0 so we choose a
            (None, None) => a,
            // if one of them is concrete, make that the representative
            (Some(_), None) => a,
            // or the other
            (None, Some(_)) => b,
            // however, when both are concrete, we can only union if they are the same.
            // we don't actually check that here. Instead, we assume they are the same,
            // continue with a (random dice roll) and then later check if they really were
            // the same or that we went ahead with the wrong type.
            (Some(concrete_a), Some(concrete_b)) => {
                match (concrete_a, concrete_b) {
                    (ConcreteType::Int, ConcreteType::Int) => {}
                    (ConcreteType::Bool, ConcreteType::Bool) => {}
                    (ConcreteType::String, ConcreteType::String) => {}
                    (ConcreteType::Tuple(elems_a), ConcreteType::Tuple(elems_b)) => {
                        self.constraints.extend(
                            elems_a
                                .iter()
                                .copied()
                                .zip(elems_b.iter().copied())
                                .map(|(x, y)| Constraint::Equal(x, y)),
                        );
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
                        self.constraints.extend(
                            params_a
                                .iter()
                                .copied()
                                .zip(params_b.iter().copied())
                                .map(|(x, y)| Constraint::Equal(x, y)),
                        );
                        self.constraints
                            .push(Constraint::Equal(**ret_a, **ret_b));
                    }

                    (ca, cb) => {
                        self.cant_unify(ca, cb, a, b);
                    }
                }

                a
            }
        }
    }

    fn unify(
        &mut self,
        a: TypeVariable,
        b: TypeVariable,
        uf: &mut SolveState,
    ) {
        uf
            .union_by(&a.as_usize(), &b.as_usize(), |a, b| {
                let res = self.unify_one(TypeVariable::from_usize(a), TypeVariable::from_usize(b));

                res.as_usize()
            })
            .unwrap_or_lice("all variables were inserted at the start");
    }

    pub fn solve(mut self) -> Result<SolvedTypes<'a>, TypeError> {
        let mut uf = VecUnionFind::new(0..=self.variable_generator.num_generated())
            .unwrap_or_lice("always increasing");

        while let Some(constraint) = self.constraints.pop() {
            match constraint {
                Constraint::Equal(a, b) => {
                    // pass uf explicitly
                    self.unify(a, b, &mut uf);
                }
            }
        }

        Ok(SolvedTypes {
            name_mapping: self.name_mapping,
            type_mapping: self.type_mapping,
            uf,
        })
    }
}

pub struct SolvedTypes<'a> {
    name_mapping: NameMapping,
    type_mapping: TypeMapping<'a>,
    uf: VecUnionFind<usize>,
}

impl<'a> SolvedTypes<'a> {
    /// Finds a representative type variable (one that maps to a concrete type through `type_mapping`)
    /// for any type variable.
    ///
    /// # ICE
    /// When the type variable was not typechecked (so not generated by VariableGenerator).
    fn find_representative(&self, var: TypeVariable) -> TypeVariable {
        TypeVariable::from_usize(
            self.uf
                .find(&var.as_usize())
                .unwrap_or_lice("type variable not in union find"),
        )
    }

    fn resolve_type_recursive<'x>(&self, ty: TypeVariable, arena: &'x Bump) -> Type<'x> {
        let representative = self.find_representative(ty);
        let type_of_representative = self.type_mapping.get(&representative).unwrap_or_else(|| {
            lice!(
                "type variable is not concrete: type variable {}",
                representative.as_usize(),
            )
        });

        match type_of_representative {
            ConcreteType::Int => Type::Int,
            ConcreteType::Bool => Type::Bool,
            ConcreteType::Function { params, ret } => {
                let mut new_params = BumpVec::new_in(arena);

                for i in *params {
                    let ty = self.resolve_type_recursive(*i, arena);
                    new_params.push(ty);
                }

                Type::Function {
                    params: new_params.into_bump_slice(),
                    ret: arena.alloc(self.resolve_type_recursive(**ret, arena)),
                }
            }
            ConcreteType::Tuple(t) => {
                let mut new = BumpVec::new_in(arena);

                for i in *t {
                    let ty = self.resolve_type_recursive(*i, arena);
                    new.push(ty);
                }

                Type::Tuple(new.into_bump_slice())
            }
            ConcreteType::String => Type::String,
        }
    }

    pub fn type_of_name<'x>(&self, name_node_id: NodeId, arena: &'x Bump) -> Option<Type<'x>> {
        let typevar = self.name_mapping.get(&name_node_id)?;
        Some(self.resolve_type_recursive(*typevar, arena))
    }
}
