use crate::ast_metadata::{Metadata, MetadataId};
use crate::debug_file::create_debug_file;
use crate::error::{ErrorId, FailedUnification};
use crate::name_resolution::ResolvedNames;
use crate::parser::ast::Ident;
use crate::parser::span::Spans;
use crate::source_file::SourceFile;
use crate::typechecking::constraint::Constraint;
use crate::typechecking::constraint_metadata::ConstraintMetadata;
use crate::typechecking::ty::{
    IntoTypeOrVariable, PartialType, TypeOrVariable, TypeVariable, VariableGenerator,
};
use bumpalo::Bump;
use itertools::Itertools;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};
use std::io::Write;

// TODO: replace with FxHashMap
/// Maps identifiers from the source code to type variables
pub type NameMapping = HashMap<MetadataId, TypeVariable>;

pub struct TypeContext<'a, 'sp> {
    /// Stores arrays of type variables needed for some concrete types,
    /// as well as any other allocation. It's more efficient in bump than on the real heap
    pub arena: &'a Bump,

    /// Generates new type variables in increasing order.
    pub type_variable_generator: VariableGenerator<TypeVariable>,

    /// Generates new error ids in increasing order.
    pub error_id_generator: VariableGenerator<ErrorId>,

    /// stores constraints. Constraints can only reference type variables,
    /// not concrete types. This makes the union find phase quicker.
    pub constraints: VecDeque<(Constraint<'a>, ConstraintMetadata<'a>)>,

    /// Stores a mapping from identifiers to type variables
    pub name_mapping: NameMapping,

    pub spans: &'sp Spans,
}

impl<'a, 'sp> TypeContext<'a, 'sp> {
    pub fn new(arena: &'a Bump, spans: &'sp Spans) -> Self {
        Self {
            type_variable_generator: VariableGenerator::new(),
            error_id_generator: VariableGenerator::new(),
            arena,
            constraints: VecDeque::new(),
            name_mapping: Default::default(),
            spans,
        }
    }

    fn get_or_insert_name_mapping(&mut self, node_id: MetadataId) -> TypeVariable {
        match self.name_mapping.entry(node_id) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(v) => {
                let variable = self.type_variable_generator.fresh();
                *v.insert(variable)
            }
        }
    }

    pub fn add_resolved_names(&mut self, resolved_names: &ResolvedNames) {
        for (a, b) in &resolved_names.names {
            let a = self.get_or_insert_name_mapping(*a);
            let b = self.get_or_insert_name_mapping(*b);
            self.add_equal_constraint(a, b, ConstraintMetadata::NameRef);
        }
    }

    pub fn fresh(&mut self) -> TypeVariable {
        self.type_variable_generator.fresh()
    }

    pub fn alloc<T>(&self, value: T) -> &'a T {
        self.arena.alloc(value)
    }

    pub fn add_equal_constraint(
        &mut self,
        a: impl IntoTypeOrVariable<'a>,
        b: impl IntoTypeOrVariable<'a>,
        meta: ConstraintMetadata<'a>,
    ) {
        let constraint =
            Constraint::Equal(a.into_type_or_variable(self), b.into_type_or_variable(self));
        self.constraints.push_back((constraint, meta));
    }

    pub fn type_of_name(&mut self, ident: &Metadata<Ident>) -> TypeVariable {
        self.get_or_insert_name_mapping(ident.metadata)
    }

    fn name_of_type_var(
        inverse_name_mapping: &HashMap<&TypeVariable, &MetadataId>,
        spans: &Spans,
        source: SourceFile,
        var: TypeOrVariable<'a>,
    ) -> String {
        match var {
            TypeOrVariable::Variable(v) => {
                if let Some(i) = inverse_name_mapping.get(&v) {
                    let span = spans.get(**i);
                    format!("variable {}", source.slice_span(span))
                } else {
                    format!("type variable {}", v.0)
                }
            }
            TypeOrVariable::Concrete(c, _tv) => match c {
                PartialType::Int => "int".to_string(),
                PartialType::Bool => "bool".to_string(),
                PartialType::Function { params, ret } => format!(
                    "fn ({}) -> {}",
                    params
                        .iter()
                        .map(|v| Self::name_of_type_var(inverse_name_mapping, spans, source, *v))
                        .join(", "),
                    Self::name_of_type_var(inverse_name_mapping, spans, source, *ret)
                ),
                PartialType::Tuple(vars) => format!(
                    "({})",
                    vars.iter()
                        .map(|v| Self::name_of_type_var(inverse_name_mapping, spans, source, *v))
                        .join(", ")
                ),
                PartialType::String => "string".to_string(),
            },
            TypeOrVariable::Error(_) => "error".to_string(),
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
                "{: >40} == {:<40}",
                Self::name_of_type_var(&inverse_name_mapping, spans, source, *a),
                Self::name_of_type_var(&inverse_name_mapping, spans, source, *b),
            )
            .unwrap()
        }

        for _ in 0..5 {
            writeln!(f).unwrap();
        }
    }
}
