use crate::ids::IdGenerator;
use crate::lowering::lir::Variable;
use crate::parser::ast::Item;

pub struct VariableDeclaration {
    pub name: Variable,
    // TODO: type etc
}

pub struct VariableDeclarations {
    declarations: Vec<VariableDeclaration>,
    variable_generator: IdGenerator<Variable>,
}

impl IntoIterator for VariableDeclarations {
    type Item = VariableDeclaration;
    type IntoIter = <Vec<VariableDeclaration> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.declarations.into_iter()
    }
}

impl<'a> IntoIterator for &'a VariableDeclarations {
    type Item = VariableDeclaration;
    type IntoIter = <&'a Vec<VariableDeclaration> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.declarations.iter()
    }
}

impl VariableDeclarations {
    pub fn new() -> Self {
        Self {
            declarations: vec![],
            variable_generator: IdGenerator::new(),
        }
    }

    pub fn iter<'a>(&'a self) -> impl IntoIterator<Item = VariableDeclaration> + 'a {
        self.declarations.iter()
    }

    /// Declares a variable in the current function
    pub fn declare_variable(&mut self /*TODO: type etc*/) -> Variable {
        let name = self.variable_generator.fresh();
        self.declarations.push(VariableDeclaration { name });
        name
    }
}
