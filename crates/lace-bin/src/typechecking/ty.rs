use crate::error::ErrorId;
use crate::typechecking::context::TypeContext;
use derive_more::From;
use itertools::Itertools;
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;

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
        ret: TypeOrVariable<'a>,
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

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, From)]
pub struct TypeVariable(pub usize);

/// generates new type variables in increasing order.
pub struct VariableGenerator<T> {
    curr: usize,
    phantom: PhantomData<T>,
}

impl<T: From<usize>> VariableGenerator<T> {
    pub fn new() -> Self {
        Self {
            curr: 0,
            phantom: Default::default(),
        }
    }

    pub fn fresh(&mut self) -> T {
        let old = self.curr;
        self.curr += 1;
        old.into()
    }
}

#[derive(Copy, Clone, Hash, Debug, Eq, PartialEq)]
pub enum TypeOrVariable<'a> {
    /// The type variable here is to make each concrete type unique,
    /// so different error messages referring to *a* concrete type
    /// don't all refer to the same concrete type
    Concrete(&'a PartialType<'a>, TypeVariable),
    Variable(TypeVariable),

    /// Generated after type errors occurred. Type errors are like poison.
    /// When unifications occur with type errors, the type error becomes bigger.
    Error(ErrorId),
}

pub trait IntoTypeOrVariable<'a> {
    fn into_type_or_variable(self, ctx: &mut TypeContext<'a, '_>) -> TypeOrVariable<'a>;
}

impl<'a> IntoTypeOrVariable<'a> for PartialType<'a> {
    fn into_type_or_variable(self, ctx: &mut TypeContext<'a, '_>) -> TypeOrVariable<'a> {
        TypeOrVariable::Concrete(ctx.alloc(self), ctx.fresh())
    }
}

impl<'a> IntoTypeOrVariable<'a> for &'a PartialType<'a> {
    fn into_type_or_variable(self, ctx: &mut TypeContext<'a, '_>) -> TypeOrVariable<'a> {
        TypeOrVariable::Concrete(self, ctx.fresh())
    }
}

impl<'a> IntoTypeOrVariable<'a> for TypeVariable {
    fn into_type_or_variable(self, _ctx: &mut TypeContext<'a, '_>) -> TypeOrVariable<'a> {
        TypeOrVariable::Variable(self)
    }
}

impl<'a> IntoTypeOrVariable<'a> for TypeOrVariable<'a> {
    fn into_type_or_variable(self, _ctx: &mut TypeContext<'a, '_>) -> TypeOrVariable<'a> {
        self
    }
}
