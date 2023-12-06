use crate::lowering::lir::int::BinaryValue;
use crate::lowering::lir::{
    BasicBlock, Expr, Function, FunctionName, Label, Lir, Place, Statement, Terminator, Value,
    Variable,
};
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

        for variable in &self.variables {
            writeln!(f, "let mut {};", variable.name)?;
        }

        for b in &self.blocks {
            writeln!(f, "{b}")?;
        }

        writeln!(f, "}}")
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "  {}:", self.name)?;

        for s in &self.stmts {
            writeln!(f, "    {}", s)?;
        }

        writeln!(f, "    {}", self.terminator)?;

        Ok(())
    }
}

impl Display for Terminator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Terminator::Goto(bb) => write!(f, "goto {bb}"),
            Terminator::Switch {
                on,
                cases,
                otherwise,
            } => {
                writeln!(f, "switch {on} {{")?;
                for (val, lbl) in cases {
                    writeln!(f, "      {val} => goto {lbl}")?;
                }
                writeln!(f, "      _ => goto {otherwise}")?;
                write!(f, "    }}")
            }
            Terminator::Abort => write!(f, "abort"),
            Terminator::Return => write!(f, "return"),
            Terminator::Unreachable => write!(f, "unreachable"),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Assignment { expr, place } => write!(f, "{place} = {expr}"),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Add(a, b) => write!(f, "{a} + {b}"),
            Expr::Sub(a, b) => write!(f, "{a} - {b}"),
            Expr::Mul(a, b) => write!(f, "{a} * {b}"),
            Expr::Div(a, b) => write!(f, "{a} / {b}"),
            Expr::Gt(a, b) => write!(f, "{a} > {b}"),
            Expr::Gte(a, b) => write!(f, "{a} >= {b}"),
            Expr::Lt(a, b) => write!(f, "{a} < {b}"),
            Expr::Lte(a, b) => write!(f, "{a} <= {b}"),
            Expr::Eq(a, b) => write!(f, "{a} == {b}"),
            Expr::Neq(a, b) => write!(f, "{a} != {b}"),
            Expr::Not(a) => write!(f, "!{a}"),
            Expr::Neg(a) => write!(f, "-{a}"),
            Expr::Value(v) => write!(f, "{v}"),
            Expr::Call { .. } => todo!(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Place(p) => write!(f, "{p}"),
            Value::Binary(b) => write!(f, "{b}"),
        }
    }
}

impl Display for BinaryValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryValue::Smol { data, bits, signed } => {
                if *signed {
                    write!(f, "{data}_i{bits}")
                } else {
                    write!(f, "{data}_u{bits}")
                }
            }
            BinaryValue::BigBoi { data, bits, signed } => {
                if *signed {
                    write!(f, "{data}_i{bits}")
                } else {
                    write!(f, "{data}_u{bits}")
                }
            }
        }
    }
}

impl Display for Place {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Place::Variable(v) => write!(f, "{v}"),
            Place::Void => write!(f, "_"),
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

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "var{}", self.0)
    }
}
