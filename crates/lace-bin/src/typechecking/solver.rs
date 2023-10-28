use crate::ice::Ice;
use crate::typechecking::constraint::Constraint;
use crate::typechecking::context::TypeMapping;
use crate::typechecking::ty::{ConcreteType, TypeVariable, TypeVariableGenerator};
use unionfind::VecUnionFind;
use crate::typechecking::error::TypeError;

pub struct Solver<'a> {
    type_mapping: TypeMapping<'a>,
    // TODO: find a better way to do this. I hate partial struct borrows
    // None *during* constraint application, passed as explicit parameter
    uf: Option<VecUnionFind<usize>>,
    maybe_bad_unions: Vec<MaybeBadUnion<'a>>,
}

impl<'a> Solver<'a> {
    pub fn new(variable_generator: TypeVariableGenerator, type_mapping: TypeMapping<'a>) -> Self {
        Self {
            type_mapping,
            maybe_bad_unions: vec![],
            uf: Some(VecUnionFind::new(0..=variable_generator.num_generated())
                .unwrap_or_ice("always increasing")),
        }
    }

    fn do_union(&mut self, a: TypeVariable, b: TypeVariable) -> TypeVariable {
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
                self.maybe_bad_union(a, b, *concrete_a, *concrete_b);
                a
            }
        }
    }

    fn union(&mut self, uf: &mut VecUnionFind<usize>, a: TypeVariable, b: TypeVariable) {
        uf
            .union_by(&a.as_usize(), &b.as_usize(), |a, b| {
                self
                    .do_union(TypeVariable::from_usize(a), TypeVariable::from_usize(b))
                    .as_usize()
            })
            .unwrap_or_ice("all variables were inserted at the start");
    }

    pub fn apply_constraints(&mut self, constraints: Vec<Constraint>) {
        // remove from the struct
        let mut uf = self.uf.take().unwrap_or_ice("don't call apply_constraints while applying constraints!");

        for constraint in constraints {
            match constraint {
                Constraint::Equal(a, b) => {
                    // pass uf explicitly
                    self.union(&mut uf, a, b);
                }
            }
        }

        // put back into the struct
        self.uf = Some(uf);
    }

    pub fn generate_type_errors(self) -> Result<SolvedTypes<'a>, TypeError> {
        for i in &self.maybe_bad_unions {
            self.process_maybe_bad_union(i)?
        }

        Ok(SolvedTypes {
            type_mapping: Default::default(),
            uf: self.uf.unwrap_or_ice("don't generate type errors while constraint processing"),
        })
    }

    fn maybe_bad_union(
        &mut self,
        type_variable_a: TypeVariable,
        type_variable_b: TypeVariable,
        concrete_a: ConcreteType<'a>,
        concrete_b: ConcreteType<'a>,
    ) {
        self.maybe_bad_unions.push(MaybeBadUnion {
            type_variable_a,
            type_variable_b,
            concrete_a,
            concrete_b,
        });
    }

    fn process_maybe_bad_union(&self, _union: &MaybeBadUnion) -> Result<(), TypeError> {
        todo!()
    }
}

pub struct MaybeBadUnion<'a> {
    pub type_variable_a: TypeVariable,
    pub type_variable_b: TypeVariable,

    pub concrete_a: ConcreteType<'a>,
    pub concrete_b: ConcreteType<'a>,
}

pub struct SolvedTypes<'a> {
    type_mapping: TypeMapping<'a>,
    uf: VecUnionFind<usize>,
}

impl<'a> SolvedTypes<'a> {
    pub fn type_of_variable(&self) {
        todo!()
    }
}