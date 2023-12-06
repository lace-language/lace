use crate::lowering::lir::{BasicBlock, Function, FunctionName, Label, Lir, Statement};
use std::fmt::{Display, Formatter};

impl Display for Lir {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for func in &self.functions {
            writeln!(f, "{func}")?;
        }

        Ok(())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "fn {} (todo) -> todo {{", self.name)?;

        for variable in self.variables {
            writeln!(f, "let mut {};", variable.name)?;
        }

        for b in self.blocks {
            writeln!(f, "{b}")?;
        }

        writeln!(f, "}}")
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "    {}:", self.name)?;

        for s in self.stmts {
            writeln!(f, "        {}", s)?;
        }

        Ok(())
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Assignment { .. } => todo!(),
        }
    }
}

impl Display for FunctionName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "func{}", self.0)
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb{}", self.0)
    }
}
