
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

impl<'a> ConcreteType<'a> {
    #[allow(non_upper_case_globals)]
    pub const Unit: Self = Self::Tuple(&[]);
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct TypeVariable(usize);

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

    pub fn next(&mut self) -> TypeVariable {
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
