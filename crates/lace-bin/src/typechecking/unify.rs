use std::collections::HashMap;
use std::convert::Infallible;
use unionfind::union::Union;
use unionfind::VecUnionFind;
use crate::ice::Ice;
use crate::typechecking::constraint::Constraint;
use crate::typechecking::ty::{ConcreteType, TypeVariable, TypeVariableGenerator};

pub struct MaybeBadUnion<'a> {
    pub type_variable_a: TypeVariable,
    pub type_variable_b: TypeVariable,

    pub concrete_a: ConcreteType<'a>,
    pub concrete_b: ConcreteType<'a>,
}

pub struct TypeVarUnion<'l, 'a> {
    type_mapping: &'l HashMap<TypeVariable, ConcreteType<'a>>,
    maybe_bad_unions: &'l mut Vec<MaybeBadUnion<'a>>
}

impl<'l, 'a> Union<usize> for TypeVarUnion<'l, 'a> {
    type Err = Infallible;

    fn union(&mut self, a: usize, b: usize) -> Result<usize, Self::Err> {
        // convert the usizes from the UF to type variables
        let a = TypeVariable::from_usize(a);
        let b = TypeVariable::from_usize(b);

        // look up if any correspond to concrete types we know
        let type_a = self.type_mapping.get(&a);
        let type_b = self.type_mapping.get(&b);

        Ok(match (type_a, type_b) {
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
                self.maybe_bad_unions.push(MaybeBadUnion {
                    type_variable_a: a,
                    type_variable_b: b,
                    concrete_a: *concrete_a,
                    concrete_b: *concrete_b,
                });

                a
            }
        }.as_usize())
    }
}

pub fn unify_constraints(
    variable_generator: &TypeVariableGenerator,
    constraints: &[Constraint],
    type_mapping: HashMap<TypeVariable, ConcreteType>,
) -> () {
    let mut uf = VecUnionFind::new(0..=variable_generator.num_generated())
        .unwrap_or_ice("always increasing");

    let mut maybe_bad_unions = Vec::new();

    for constraint in constraints {
        match constraint {
            Constraint::Equal(a, b) => {
                uf.union_by(
                    &a.as_usize(),
                    &b.as_usize(),
                    TypeVarUnion { type_mapping: &type_mapping, maybe_bad_unions: &mut maybe_bad_unions }
                    ).unwrap();
            }
        }
    }
}
