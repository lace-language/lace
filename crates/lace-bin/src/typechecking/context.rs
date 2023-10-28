use crate::name_resolution::NameResolutions;
use crate::parser::ast::Ident;
use crate::typechecking::constraint::Constraint;
use crate::typechecking::ty::TypeVariableGenerator;
use crate::typechecking::ty::{ConcreteType, TypeVariable};
use bumpalo::Bump;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fs;
use itertools::Itertools;
use crate::parser::span::Spans;
use crate::source_file::SourceFile;
use crate::syntax_id::{NodeId, Identified};
use std::io::Write;
use crate::debug_file::create_debug_file;

pub type TypeMapping<'a> = HashMap<TypeVariable, ConcreteType<'a>>;

pub struct TypeContext<'a> {
    /// Stores arrays of type variables needed for some concrete types,
    /// as well as any other allocation. It's more efficient in bump than on the real heap
    pub arena: &'a Bump,

    /// Generates new type variables in increasing order.
    pub variable_generator: TypeVariableGenerator,

    /// stores constraints. Constraints can only reference type variables,
    /// not concrete types. This makes the union find phase quicker.
    pub constraints: Vec<Constraint>,

    /// Stores a mapping from identifiers to type variables
    // TODO: replace with FxHashMap
    pub name_mapping: HashMap<NodeId, TypeVariable>,
    /// Stores a mapping from type variables to concrete types
    // TODO: replace with FxHashMap
    pub type_mapping: TypeMapping<'a>,
}

impl<'a> TypeContext<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        Self {
            variable_generator: TypeVariableGenerator::new(),
            arena,
            constraints: vec![],
            name_mapping: Default::default(),
            type_mapping: Default::default(),
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

    pub fn concrete_type(&mut self, ty: ConcreteType<'a>) -> TypeVariable {
        let var = self.fresh();
        self.type_mapping.insert(var, ty);
        var
    }

    pub fn alloc<T>(&self, value: T) -> &'a T {
        self.arena.alloc(value)
    }

    pub fn add_equal_constraint_concrete(&mut self, a: TypeVariable, b: ConcreteType<'a>) {
        let b = self.concrete_type(b);
        self.add_equal_constraint(a, b);
    }

    pub fn add_equal_constraint(&mut self, a: TypeVariable, b: TypeVariable) {
        self.constraints.push(Constraint::Equal(a, b));
    }

    pub fn type_of_name(&mut self, ident: &Identified<Ident>) -> TypeVariable {
        self.get_or_insert_name_mapping(ident.node_id)
    }

    fn name_of_type_var(
        &self,
        inverse_name_mapping: &HashMap<&TypeVariable, &NodeId>,
        spans: &Spans,
        source: SourceFile,
        var: TypeVariable
    ) -> String {
        if let Some(i) = inverse_name_mapping.get(&var) {
            let span = spans.get(**i);
            format!("variable {}", source.slice_span(span))
        } else if let Some(i) = self.type_mapping.get(&var) {
            match i {
                ConcreteType::Int => format!("int"),
                ConcreteType::Bool => format!("bool"),
                ConcreteType::Function { params, ret } => format!(
                    "fn ({}) -> {}",
                    params.iter()
                        .map(|v| self.name_of_type_var(inverse_name_mapping, spans, source,*v))
                        .join(", "),
                    self.name_of_type_var(inverse_name_mapping, spans, source, **ret)
                ),
                ConcreteType::Tuple(vars) => format!(
                    "({})",
                    vars.iter()
                        .map(|v| self.name_of_type_var(inverse_name_mapping, spans, source, *v))
                        .join(", ")
                ),
                ConcreteType::String => format!("string"),
            }
        } else {
            format!("type variable {}", var.as_usize())
        }
    }

    pub fn save_debug(&self, spans: &Spans, source: SourceFile) {
        let mut f = create_debug_file("type-constraints");
        let inverse_name_mapping = self.name_mapping
            .iter()
            .map(|(a, b)| (b, a))
            .collect::<HashMap<_, _>>();

        for Constraint::Equal(a, b) in &self.constraints {
            writeln!(
                f,
                "{} == {}",
                self.name_of_type_var(&inverse_name_mapping, spans, source, *a),
                self.name_of_type_var(&inverse_name_mapping, spans, source, *b)
            ).unwrap()
        }
    }
}
