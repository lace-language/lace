use crate::lowering::lir::FunctionName;

use itertools::Itertools;
use std::fmt::{Display, Formatter};
use std::num::NonZeroU16;

/// Used after type checking, contains no unresolved types
#[derive(Copy, Clone)]
pub enum Type<'a> {
    Int {
        bits: NonZeroU16,
        signed: bool,
    },
    Bool,
    Function {
        params: &'a [Type<'a>],
        ret: &'a Type<'a>,
        /// every function has a unique ID that marks its type different
        function_name: FunctionName,
    },
    Tuple(&'a [Type<'a>]),
    String,
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int { bits, signed } => {
                if *signed {
                    write!(f, "i{bits}")
                } else {
                    write!(f, "u{bits}")
                }
            }
            Type::Bool => write!(f, "bool"),
            Type::Function { params, ret, .. } => {
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
    Int {
        bits: NonZeroU16,
        signed: bool,
    },
    Bool,
    Function {
        params: &'a [PartialType<'a>],
        ret: &'a PartialType<'a>,
        /// each function is a unique type. They can be unified during typechecking,
        /// but after typechecking, different functions with the same signature are
        /// different. However, a partial type may not have a function id. However, while
        /// unifying functions, None function ids are always replaced with the function id
        /// of what is unified with. That way, the typechecker can at any point assert that
        /// something is a certain function, not caring about the ID, but typechecking will
        /// produce a function type with an ID. Function IDs are generated in the static pass.
        function_name: Option<FunctionName>,
    },
    Tuple(&'a [PartialType<'a>]),
    String,
    Variable(TypeVariable),
}

impl<'a> Display for PartialType<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PartialType::Int { bits, signed } => {
                if *signed {
                    write!(f, "i{bits}")
                } else {
                    write!(f, "u{bits}")
                }
            }
            PartialType::Bool => write!(f, "bool"),
            PartialType::Function { params, .. } => {
                write!(f, "fn ({}) -> _", params.iter().map(|_| "_").join(","))
            }
            // If there is only a single parameter, there should be a trailing comma
            PartialType::Tuple(&[_]) => write!(f, "(_,)"),
            PartialType::Tuple(t) => write!(f, "({})", t.iter().map(|_| "_").join(",")),
            PartialType::String => write!(f, "string"),
            PartialType::Variable(TypeVariable::Int(_)) => write!(f, "{{integer}}"),
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
pub enum TypeVariable {
    Type(usize),
    Int(usize),
}

impl From<usize> for TypeVariable {
    fn from(value: usize) -> Self {
        Self::Type(value)
    }
}

impl TypeVariable {
    pub fn variable(&self) -> usize {
        match self {
            TypeVariable::Type(v) => *v,
            TypeVariable::Int(v) => *v,
        }
    }
}

#[cfg(test)]
mod test {
    use super::Type;
    use std::num::NonZeroU16;
    #[test]
    fn display_function() {
        assert_eq!(
            Type::Function {
                params: &[],
                ret: &Type::Int {
                    bits: NonZeroU16::new(32).unwrap(),
                    signed: true
                },
                function_name: 0.into(),
            }
            .to_string(),
            "fn () -> i32"
        );
        assert_eq!(
            Type::Function {
                params: &[Type::Int {
                    bits: NonZeroU16::new(32).unwrap(),
                    signed: true
                }],
                ret: &Type::Int {
                    bits: NonZeroU16::new(32).unwrap(),
                    signed: true
                },
                function_name: 0.into(),
            }
            .to_string(),
            "fn (i32) -> i32"
        );
        assert_eq!(
            Type::Function {
                params: &[
                    Type::Int {
                        bits: NonZeroU16::new(32).unwrap(),
                        signed: true
                    },
                    Type::Int {
                        bits: NonZeroU16::new(32).unwrap(),
                        signed: true
                    }
                ],
                ret: &Type::Int {
                    bits: NonZeroU16::new(32).unwrap(),
                    signed: true
                },
                function_name: 0.into(),
            }
            .to_string(),
            "fn (i32, i32) -> i32"
        );
    }

    #[test]
    fn display_tuple() {
        assert_eq!(Type::Tuple(&[]).to_string(), "()");
        assert_eq!(
            Type::Tuple(&[Type::Int {
                bits: NonZeroU16::new(32).unwrap(),
                signed: true
            }])
            .to_string(),
            "(i32,)"
        );
        assert_eq!(
            Type::Tuple(&[
                Type::Int {
                    bits: NonZeroU16::new(32).unwrap(),
                    signed: true
                },
                Type::Int {
                    bits: NonZeroU16::new(32).unwrap(),
                    signed: true
                }
            ])
            .to_string(),
            "(i32, i32)"
        );
    }
}
