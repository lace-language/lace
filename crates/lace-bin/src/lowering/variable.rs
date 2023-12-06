use crate::ast_metadata::MetadataId;

use crate::lowering::lir::Variable;
use crate::lowering::LoweringContext;

pub struct VariableDeclaration {
    pub name: Variable,
    // TODO: type etc
}

pub struct VariableDeclarations {
    declarations: Vec<VariableDeclaration>,
}

impl IntoIterator for VariableDeclarations {
    type Item = VariableDeclaration;
    type IntoIter = <Vec<VariableDeclaration> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.declarations.into_iter()
    }
}

impl<'a> IntoIterator for &'a VariableDeclarations {
    type Item = &'a VariableDeclaration;
    type IntoIter = <&'a Vec<VariableDeclaration> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.declarations.iter()
    }
}

impl Default for VariableDeclarations {
    fn default() -> Self {
        Self::new()
    }
}

impl VariableDeclarations {
    pub fn new() -> Self {
        Self {
            declarations: vec![],
        }
    }

    pub fn iter(&self) -> impl IntoIterator<Item = &VariableDeclaration> {
        self.declarations.iter()
    }

    /// Declares a variable in the current function
    pub fn declare_variable(&mut self, name: Variable /*TODO: type etc*/) -> Variable {
        self.declarations.push(VariableDeclaration { name });
        name
    }
}

impl<'b, 't, 'a, 'n> LoweringContext<'b, 't, 'a, 'n> {
    pub fn lookup_variable(&mut self, var: MetadataId) -> Variable {
        let tyvar = self
            .solved_types
            .type_variable_for_identifier(var)
            .unwrap_or_else(|| lice!("name not typechecked {var:?}"));

        *self
            .variable_mapping
            .entry(tyvar)
            .or_insert_with(|| self.variable_generator.fresh())
    }
}
