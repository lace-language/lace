use itertools::Itertools;
use std::fmt::{Display, Formatter};

/// Used after type checking, contains no unresolved types
#[derive(Copy, Clone)]
pub enum Type<'a> {
    Int,
    Bool,
    Function {
        params: &'a [Type<'a>],
        ret: &'a Type<'a>,
    },
    Tuple(&'a [Type<'a>]),
    String,
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Function { params, ret } => {
                write!(f, "fn (")?;
                let mut params = params.iter();
                if let Some(p) = params.next() {
                    write!(f, "{p}")?;
                }
                for p in params {
                    write!(f, ", {p}")?;
                }
                write!(f, ") -> {ret}")
            }
            // If there is only a single parameter, there should be a trailing comma
            Type::Tuple(&[p]) => {
                write!(f, "({p},)")
            }
            Type::Tuple(params) => {
                write!(f, "(")?;
                let mut params = params.iter();
                if let Some(p) = params.next() {
                    write!(f, "{p}")?;
                }
                for p in params {
                    write!(f, ", {p}")?;
                }
                write!(f, ")")
            }
            Type::String => write!(f, "string"),
        }
    }
}

/// Used during type checking, contains unresolved types (type variables)
#[derive(Copy, Clone, Hash, Debug, Eq, PartialEq)]
pub enum PartialType<'a> {
    Int,
    Bool,
    Function {
        params: &'a [PartialType<'a>],
        ret: &'a PartialType<'a>,
    },
    Tuple(&'a [PartialType<'a>]),
    String,
    Variable(TypeVariable),
}

impl<'a> Display for PartialType<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PartialType::Int => write!(f, "int"),
            PartialType::Bool => write!(f, "bool"),
            PartialType::Function { params, .. } => {
                write!(f, "fn ({}) -> _", params.iter().map(|_| "_").join(","))
            }
            // If there is only a single parameter, there should be a trailing comma
            PartialType::Tuple(&[_]) => write!(f, "(_,)"),
            PartialType::Tuple(t) => write!(f, "({})", t.iter().map(|_| "_").join(",")),
            PartialType::String => write!(f, "string"),
            PartialType::Variable(v) => write!(f, "type variable {v:?}"),
        }
    }
}

impl<'a> PartialType<'a> {
    #[allow(non_upper_case_globals)]
    pub const Unit: Self = Self::Tuple(&[]);
}

impl<'a> From<TypeVariable> for PartialType<'a> {
    fn from(value: TypeVariable) -> Self {
        Self::Variable(value)
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct TypeVariable(pub usize);

/// generates new type variables in increasing order.
pub struct TypeVariableGenerator {
    curr: usize,
}

impl TypeVariableGenerator {
    pub fn new() -> Self {
        Self { curr: 0 }
    }

    pub fn fresh(&mut self) -> TypeVariable {
        let old = self.curr;
        self.curr += 1;
        TypeVariable(old)
    }
}

#[cfg(test)]
mod test {
    use super::Type;

    #[test]
    fn display_function() {
        assert_eq!(
            Type::Function {
                params: &[],
                ret: &Type::Int,
            }
            .to_string(),
            "fn () -> int"
        );
        assert_eq!(
            Type::Function {
                params: &[Type::Int],
                ret: &Type::Int,
            }
            .to_string(),
            "fn (int) -> int"
        );
        assert_eq!(
            Type::Function {
                params: &[Type::Int, Type::Int],
                ret: &Type::Int,
            }
            .to_string(),
            "fn (int, int) -> int"
        );
    }

    #[test]
    fn display_tuple() {
        assert_eq!(Type::Tuple(&[]).to_string(), "()");
        assert_eq!(Type::Tuple(&[Type::Int]).to_string(), "(int,)");
        assert_eq!(
            Type::Tuple(&[Type::Int, Type::Int]).to_string(),
            "(int, int)"
        );
    }
}
