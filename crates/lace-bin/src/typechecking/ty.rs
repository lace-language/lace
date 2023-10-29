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
#[derive(Copy, Clone, Debug)]
pub enum ConcreteType<'a> {
    Int,
    Bool,
    Function {
        params: &'a [TypeVariable],
        ret: &'a TypeVariable,
    },
    Tuple(&'a [TypeVariable]),
    String,
}

impl<'a> Display for ConcreteType<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConcreteType::Int => write!(f, "int"),
            ConcreteType::Bool => write!(f, "bool"),
            ConcreteType::Function { params, .. } => {
                write!(f, "fn ({}) -> _", params.iter().map(|_| "_").join(","))
            }
            ConcreteType::Tuple(t) => write!(f, "({})", t.iter().map(|_| "_").join(",")),
            ConcreteType::String => write!(f, "string"),
        }
    }
}

impl<'a> ConcreteType<'a> {
    #[allow(non_upper_case_globals)]
    pub const Unit: Self = Self::Tuple(&[]);
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct TypeVariable(usize);

// TODO: remove
impl TypeVariable {
    /// This operation is explicit instead of a public first field so that
    /// it is harder to accidentally do and easier to control-f for.
    pub fn from_usize(v: usize) -> Self {
        Self(v)
    }

    /// This operation is explicit instead of a public first field so that
    /// it is harder to accidentally do and easier to control-f for.
    pub fn as_usize(&self) -> usize {
        self.0
    }
}

/// generates new type variables in increasing order.
pub struct TypeVariableGenerator {
    curr: usize,
}

impl TypeVariableGenerator {
    pub fn new() -> Self {
        Self { curr: 0 }
    }

    pub fn num_generated(&self) -> usize {
        self.curr - 1
    }

    pub fn fresh(&mut self) -> TypeVariable {
        let old = self.curr;
        self.curr += 1;
        TypeVariable(old)
    }
}

#[derive(Copy, Clone)]
#[must_use]
pub enum TypeOrVariable<'a> {
    Concrete(ConcreteType<'a>),
    Variable(TypeVariable),
}

impl<'a> From<ConcreteType<'a>> for TypeOrVariable<'a> {
    fn from(value: ConcreteType<'a>) -> Self {
        Self::Concrete(value)
    }
}

impl<'a> From<TypeVariable> for TypeOrVariable<'a> {
    fn from(value: TypeVariable) -> Self {
        Self::Variable(value)
    }
}
