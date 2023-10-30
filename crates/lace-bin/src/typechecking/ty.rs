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
                if let Some((last, rest)) = params.split_last() {
                    for i in rest {
                        write!(f, "{i},")?;
                    }

                    write!(f, "{last}")?;
                    if rest.is_empty() {
                        write!(f, ",")?;
                    }
                }

                write!(f, ") -> {ret}")
            }
            Type::Tuple(t) => {
                write!(f, "(")?;
                if let Some((last, rest)) = t.split_last() {
                    for i in rest {
                        write!(f, "{},", i)?;
                    }

                    write!(f, "{}", last)?;
                    if rest.is_empty() {
                        write!(f, ",")?;
                    }
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
        params: &'a [TypeOrVariable<'a>],
        ret: &'a TypeOrVariable<'a>,
    },
    Tuple(&'a [TypeOrVariable<'a>]),
    String,
}

impl<'a> Display for PartialType<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PartialType::Int => write!(f, "int"),
            PartialType::Bool => write!(f, "bool"),
            PartialType::Function { params, .. } => {
                write!(f, "fn ({}) -> _", params.iter().map(|_| "_").join(","))
            }
            PartialType::Tuple(t) => write!(f, "({})", t.iter().map(|_| "_").join(",")),
            PartialType::String => write!(f, "string"),
        }
    }
}

impl<'a> PartialType<'a> {
    #[allow(non_upper_case_globals)]
    pub const Unit: Self = Self::Tuple(&[]);
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

#[derive(Copy, Clone, Hash, Debug, Eq, PartialEq)]
#[must_use]
pub enum TypeOrVariable<'a> {
    Concrete(PartialType<'a>),
    Variable(TypeVariable),
}

impl<'a> From<PartialType<'a>> for TypeOrVariable<'a> {
    fn from(value: PartialType<'a>) -> Self {
        Self::Concrete(value)
    }
}

impl<'a> From<TypeVariable> for TypeOrVariable<'a> {
    fn from(value: TypeVariable) -> Self {
        Self::Variable(value)
    }
}
