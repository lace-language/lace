use crate::debug_file::create_debug_file;
use crate::name_resolution::ResolvedNames;
use crate::parser::ast::Ident;
use crate::parser::span::Spans;
use crate::source_file::SourceFile;
use crate::syntax_id::{Identified, NodeId};
use crate::typechecking::constraint::Constraint;
use crate::typechecking::constraint_metadata::ConstraintMetadata;
use crate::typechecking::error::TypeError;
use crate::typechecking::ty::{ConcreteType, TypeVariable, TypeVariableGenerator};
use bumpalo::Bump;
use itertools::Itertools;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::io::Write;

// TODO: replace with FxHashMap
/// Maps concrete types to type variables
pub type TypeMapping<'a> = HashMap<TypeVariable, ConcreteType<'a>>;
// TODO: replace with FxHashMap
/// Maps identifiers from the source code to type variables
pub type NameMapping = HashMap<NodeId, TypeVariable>;

pub struct TypeContext<'a, 'sp> {
    /// Stores arrays of type variables needed for some concrete types,
    /// as well as any other allocation. It's more efficient in bump than on the real heap
    pub arena: &'a Bump,

    /// Generates new type variables in increasing order.
    pub variable_generator: TypeVariableGenerator,

    /// stores constraints. Constraints can only reference type variables,
    /// not concrete types. This makes the union find phase quicker.
    pub constraints: Vec<(Constraint, ConstraintMetadata<'a>)>,

    /// Stores a mapping from identifiers to type variables
    pub name_mapping: NameMapping,

    /// Stores a mapping from type variables to concrete types
    pub type_mapping: TypeMapping<'a>,

    pub errors: Vec<TypeError>,

    pub spans: &'sp Spans,
}

impl<'a, 'sp> TypeContext<'a, 'sp> {
    pub fn new(arena: &'a Bump, spans: &'sp Spans) -> Self {
        Self {
            variable_generator: TypeVariableGenerator::new(),
            arena,
            constraints: vec![],
            name_mapping: Default::default(),
            type_mapping: Default::default(),
            errors: vec![],
            spans,
        }
    }

    fn get_or_insert_name_mapping(&mut self, node_id: NodeId) -> TypeVariable {
        match self.name_mapping.entry(node_id) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(v) => {
                let variable = self.variable_generator.fresh();
                *v.insert(variable)
            }
        }
    }

    pub fn add_name_resolutions(&mut self, name_resolutions: &ResolvedNames) {
        for (a, b) in &name_resolutions.names {
            let a = self.get_or_insert_name_mapping(*a);
            let b = self.get_or_insert_name_mapping(*b);
            self.add_equal_constraint(a, b, ConstraintMetadata::NameRef);
        }
    }

    pub fn fresh(&mut self) -> TypeVariable {
        self.variable_generator.fresh()
    }

    pub fn concrete_type(&mut self, ty: ConcreteType<'a>) -> TypeVariable {
        let var = self.fresh();
        self.type_mapping.insert(var, ty);
        var
    }

    pub fn alloc<T>(&self, value: T) -> &'a T {
        self.arena.alloc(value)
    }

    pub fn add_equal_constraint_concrete(
        &mut self,
        a: TypeVariable,
        b: ConcreteType<'a>,
        meta: ConstraintMetadata<'a>,
    ) {
        let b = self.concrete_type(b);
        self.add_equal_constraint(a, b, meta);
    }

    pub fn add_equal_constraint(
        &mut self,
        a: TypeVariable,
        b: TypeVariable,
        meta: ConstraintMetadata<'a>,
    ) {
        self.constraints.push((Constraint::Equal(a, b), meta));
    }

    pub fn type_of_name(&mut self, ident: &Identified<Ident>) -> TypeVariable {
        self.get_or_insert_name_mapping(ident.node_id)
    }

    fn name_of_type_var(
        &self,
        inverse_name_mapping: &HashMap<&TypeVariable, &NodeId>,
        spans: &Spans,
        source: SourceFile,
        var: TypeVariable,
    ) -> String {
        if let Some(i) = inverse_name_mapping.get(&var) {
            let span = spans.get(**i);
            format!("variable {}", source.slice_span(span))
        } else if let Some(i) = self.type_mapping.get(&var) {
            match i {
                ConcreteType::Int => "int".to_string(),
                ConcreteType::Bool => "bool".to_string(),
                ConcreteType::Function { params, ret } => format!(
                    "fn ({}) -> {}",
                    params
                        .iter()
                        .map(|v| self.name_of_type_var(inverse_name_mapping, spans, source, *v))
                        .join(", "),
                    self.name_of_type_var(inverse_name_mapping, spans, source, **ret)
                ),
                ConcreteType::Tuple(vars) => format!(
                    "({})",
                    vars.iter()
                        .map(|v| self.name_of_type_var(inverse_name_mapping, spans, source, *v))
                        .join(", ")
                ),
                ConcreteType::String => "string".to_string(),
            }
        } else {
            format!("type variable {}", var.as_usize())
        }
    }

    pub fn save_debug(&self, spans: &Spans, source: SourceFile) {
        let mut f = create_debug_file("type-constraints");
        let inverse_name_mapping = self
            .name_mapping
            .iter()
            .map(|(a, b)| (b, a))
            .collect::<HashMap<_, _>>();

        writeln!(f, "constraints:").unwrap();
        for (Constraint::Equal(a, b), _) in &self.constraints {
            writeln!(
                f,
                "{: >40} == {:<40}       {} == {}",
                self.name_of_type_var(&inverse_name_mapping, spans, source, *a),
                self.name_of_type_var(&inverse_name_mapping, spans, source, *b),
                a.as_usize(),
                b.as_usize(),
            )
            .unwrap()
        }

        for _ in 0..5 {
            writeln!(f).unwrap();
        }

        writeln!(f, "concrete types variables:").unwrap();
        for tv in self.type_mapping.keys() {
            let name = self.name_of_type_var(&inverse_name_mapping, spans, source, *tv);
            writeln!(f, "tv {} ==> {name}", tv.as_usize()).unwrap();
        }
    }
}
